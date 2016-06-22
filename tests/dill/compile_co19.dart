// Copyright (c) 2016, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

import 'dart:io';
import 'dart:async';
import 'dart:convert';

import '../../third_party/kernel/bin/dartk.dart' as dartk;
import 'package:path/path.dart' as path;

main(args) async {
  if (args.length != 1) {
    print('Usage: ${Platform.executable} ${Platform.script} '
          '<instructions.txt>');
    exit(1);
  }

  // Partition [lines] into 32 roughly equal sets and run 32 parallel batch
  // runners.
  List<String> lines = new File(args[0]).readAsLinesSync();

  String cwd = path.dirname(Platform.script.toFilePath());
  var partitions = partition(lines, 32);
  var futures = [];
  for (var partition in partitions) {
    var arguments = [
      '--package-root=${Platform.packageRoot}',
      '$cwd/compile_co19_batch.dart'
    ];

    var process = await Process.start(Platform.executable, arguments);
    for (var line in partition) process.stdin.writeln(line);
    process.stdin.close();

    // Just print everything the subprocesses are printing.
    process.stdout
      .transform(UTF8.decoder)
      .transform(new LineSplitter())
      .listen(print);
    process.stderr
      .transform(UTF8.decoder)
      .transform(new LineSplitter())
      .listen(print);

    futures.add(process.exitCode);
  }
  await Future.wait(futures);
}

partition(List items, int count) {
  var partitions = new List.generate(count, (i) => []);
  int partitionNr = 0;
  for (var item in items) {
    partitions[partitionNr].add(item);
    partitionNr = (partitionNr + 1) % count;
  }
  return partitions;
}
