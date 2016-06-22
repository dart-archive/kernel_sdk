// Copyright (c) 2016, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

import 'dart:io';
import 'dart:convert';

import '../../third_party/kernel/bin/dartk.dart' as dartk;
import 'package:path/path.dart' as path;

main(args) async {
  var lines = await stdin
      .transform(UTF8.decoder)
      .transform(new LineSplitter())
      .toList();

  // We use the SDK from the dart executable we were run on.
  String dartSdk = path.dirname(path.dirname(Platform.resolvedExecutable));
  for (var line in lines) {
    var tuple = line.trim().split(' ');
    if (tuple.length > 0) {
      var dartFile = tuple[0].trim();
      var dillFile = tuple[1].trim();

      print('Handling $dartFile');
      if (isPositiveTest(dartFile)) {
        String dilDirectory = path.dirname(dillFile);
        var dir = new Directory(dilDirectory);
        if (!dir.existsSync()) dir.createSync(recursive: true);

        // Suprisingly enough, running dartk is synchronous from start to end!
        try {
          dartk.main([dartFile,
              '--format=bin', '--link', '--sdk=$dartSdk', '--out=$dillFile']);
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
