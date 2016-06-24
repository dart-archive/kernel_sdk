// Copyright (c) 2016, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

import 'dart:io';
import 'dart:convert';

import '../../third_party/rasta/bin/rastak.dart' as rastak;

main(args) async {
  var lines = await stdin
      .transform(UTF8.decoder)
      .transform(new LineSplitter())
      .toList();

  // We use the SDK from the dart executable we were run on.
  for (var line in lines) {
    var tuple = line.trim().split(' ');
    if (tuple.length > 0) {
      var dartFile = tuple[0].trim();
      var dillFile = tuple[1].trim();

      print('Handling $dartFile');
      if (isPositiveTest(dartFile)) {
        try {
          // As long as rastak has a memory leak we can't do it in this process.
          var out = await Process.run(Platform.executable,
              ['third_party/rasta/bin/rastak.dart', dartFile, dillFile]);
          print(out.stdout);
          print(out.stderr);
          //await rastak.main([dartFile, dillFile], null);
        } catch (e, s) {
          print("Failed to compile $dartFile\nerror:\n$e\nstack:\n$s\n");
        }
      }
    }
  }
}

bool isPositiveTest(dartFile) {
  return Process.runSync(Platform.executable, [dartFile]).exitCode == 0;
}
