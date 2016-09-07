#!/usr/bin/env dart
// Copyright (c) 2016, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.
library status_diff;

import 'dart:async';
import 'dart:convert';
import 'dart:io';

import 'status_expression.dart';

/// A single status file change found in a test log.
class StatusDiffAtom implements Comparable<StatusDiffAtom> {
  final String suite;
  final String testName;
  final String compiler;
  final String runtime;
  final String expected;
  final String actual;

  StatusDiffAtom(
      {this.suite,
      this.testName,
      this.compiler,
      this.runtime,
      this.expected,
      this.actual});

  toString() => '$compiler-$runtime $suite/$testName: $expected => $actual';

  int compareTo(StatusDiffAtom other) {
    return testName.compareTo(other.testName);
  }

  String get statusLine => '$testName: $actual';
}

/// A collection of status diffs organized into files and sections.
class StatusDiff {
  final List<StatusFileDiff> files = <StatusFileDiff>[];

  StatusFileDiff getFile(String filename) {
    for (var file in files) {
      if (file.filename == filename) return file;
    }
    var file = new StatusFileDiff(filename);
    files.add(file);
    return file;
  }
}

class StatusFileDiff {
  final String filename;
  final List<SectionDiff> sections = <SectionDiff>[];

  StatusFileDiff(this.filename);

  SectionDiff tryGetSection(String compiler, String runtime) {
    for (var section in sections) {
      if (section.compiler == compiler && section.runtime == runtime) {
        return section;
      }
    }
    return null;
  }

  SectionDiff getSection(String compiler, String runtime) {
    var section = tryGetSection(compiler, runtime);
    if (section != null) return section;
    section = new SectionDiff(compiler, runtime);
    sections.add(section);
    return section;
  }

  String toString() => filename;
}

class SectionDiff {
  final String compiler;
  final String runtime;

  final List<StatusDiffAtom> failures = <StatusDiffAtom>[];
  final Set<String> testNames = new Set<String>();

  SectionDiff(this.compiler, this.runtime);

  void add(StatusDiffAtom atom) {
    testNames.add(atom.testName);
    if (atom.actual != 'Pass') {
      failures.add(atom);
    }
  }

  String toString() => '$compiler-$runtime';
}

// Regular expressions for matching default test.py output. For example:
//   FAILED: dartk-vm debug_x64 suite/testname
//   Expected: RuntimeError
//   Actual: Pass
RegExp rexFailed = new RegExp(r'FAILED: (\w+)-(\w+) (\w+) (\w+)/(.*)');
RegExp rexExpected = new RegExp(r'Expected: (.*)');
RegExp rexActual = new RegExp(r'Actual: (.*)');

Match tryMatch(RegExp regExp, String lineOrNull) {
  if (lineOrNull == null) return null;
  return regExp.firstMatch(lineOrNull);
}

/// Parses the default output of `test.py` to a stream of status changes.
Stream<StatusDiffAtom> parseTestLog(String file) async* {
  Stream<String> lines = new File(file)
      .openRead()
      .transform(UTF8.decoder)
      .transform(new LineSplitter());
  StreamIterator<String> iterator = new StreamIterator<String>(lines);
  while (await iterator.moveNext()) {
    Match matchFailed = tryMatch(rexFailed, iterator.current);
    if (matchFailed == null) continue;
    await iterator.moveNext();
    Match matchExpected = tryMatch(rexExpected, iterator.current);
    if (matchExpected == null) continue;
    await iterator.moveNext();
    Match matchActual = tryMatch(rexActual, iterator.current);
    if (matchActual == null) continue;
    String compiler = matchFailed.group(1);
    String runtime = matchFailed.group(2);
    String suiteName = matchFailed.group(4);
    String testName = matchFailed.group(5).trimRight();
    String expected = matchExpected.group(1).trim();
    String actual = matchActual.group(1).trim();
    yield new StatusDiffAtom(
        compiler: compiler,
        runtime: runtime,
        suite: suiteName,
        testName: testName,
        expected: expected,
        actual: actual);
  }
}

final Map<String, String> statusFileMap = {
  'dartk-language': 'tests/language/language_kernel.status',
  'dartkp-language': 'tests/language/language_kernel.status',
  'dartk-co19': 'tests/co19/co19-kernel.status',
  'dartkp-co19': 'tests/co19/co19-kernel.status',
  'dartk-dill': 'tests/dill/dill.status',
  'dartkp-dill': 'tests/dill/dill.status',
};

String getStatusFileFor(StatusDiffAtom atom) {
  return statusFileMap['${atom.compiler}-${atom.suite}'];
}

/// Organizes a list of status changes into a [StatusDiff].
StatusDiff buildStatusDiff(List<StatusDiffAtom> atoms) {
  StatusDiff diff = new StatusDiff();
  for (var atom in atoms) {
    String file = getStatusFileFor(atom);
    diff.getFile(file).getSection(atom.compiler, atom.runtime).add(atom);
  }
  for (var file in diff.files) {
    for (var section in file.sections) {
      // Sort the status changes and remove duplicates.  Duplicates may occur
      // because test.py prints out the failure during the run, and then again
      // in the summary.
      section.failures.sort();
      int deleted = 0;
      for (int i = 1; i < section.failures.length; ++i) {
        if (section.failures[i].testName == section.failures[i - 1].testName) {
          ++deleted;
        } else if (deleted > 0) {
          section.failures[i - deleted] = section.failures[i];
        }
      }
      section.failures.length -= deleted;
    }
  }
  return diff;
}

/// Matches simple status lines with a single expected outcome, i.e. no
/// "Pass, Slow" outcomes.
RegExp rexStatusLine = new RegExp(r'([^#:]+):\s*(\w+)(.*)');

/// Checks if the given expression is of form:
///
///      $var1 == value1 && $var2 == value2 && ...
///
/// The expected values for each variable is recorded in [environment].
///
/// Returns `true` if the expression had the expected form, `false` otherwise.
bool analyzeCondition(BooleanExpression node, Map<String, String> environment) {
  if (node is BooleanOperation && node.op == '&&') {
    bool left = analyzeCondition(node.left, environment);
    bool right = analyzeCondition(node.right, environment);
    return left && right;
  } else if (node is Comparison && !node.negate) {
    if (environment.containsKey(node.left.name)) return false;
    environment[node.left.name] = node.right.value;
    return true;
  } else {
    return false;
  }
}

/// Applies the changes in a status diff directly to the status files.
Future applyStatusDiff(StatusDiff diff) async {
  List<Future> futures = <Future>[];
  for (StatusFileDiff fileDiff in diff.files) {
    String filename = fileDiff.filename;

    List<String> output = <String>[]; // Contents of the new status file.

    // We scan the statis file while maintaining a currently open section, and
    // an index denoting how many new failure lines we have emitted so far.
    SectionDiff currentSection = null;
    int failureIndex = -1;
    int linesRemovedFromSection = 0;

    Set<SectionDiff> emittedSections = new Set<SectionDiff>();

    // When a section ends, we want to flush the remaining failures below the
    // last seen status line.  There may be comments other noise separating the
    // sections, so we don't want to flush to the end of the buffer.
    int lastStatusLineIndex = -1;

    void emitStatusLine(String line) {
      output.add(line);
      lastStatusLineIndex = output.length - 1;
    }
    void emitNoise(String line) {
      output.add(line);
    }
    void flushSection() {
      if (currentSection == null) return;
      int missingLines =
          currentSection.testNames.length - linesRemovedFromSection;
      if (missingLines != 0) {
        print('Missing $missingLines status lines '
            'from $currentSection in $filename');
      }
      output.insertAll(lastStatusLineIndex + 1,
          currentSection.failures.skip(failureIndex).map((f) => f.statusLine));
      emittedSections.add(currentSection);
      currentSection = null;
      lastStatusLineIndex = -1;
    }

    Stream<String> lines = new File(filename)
        .openRead()
        .transform(UTF8.decoder)
        .transform(new LineSplitter());
    await for (String line in lines) {
      if (line.startsWith('[') && line.endsWith(']')) {
        flushSection();
        emitStatusLine(line);
        // Parse the condition string and look for a matching section diff.
        String conditionString = line.substring(1, line.length - 1);
        var tokenizer = new Tokenizer(conditionString);
        var scanner = new Scanner(tokenizer.tokenize());
        var expression = new ExpressionParser(scanner).parseBooleanExpression();
        var environment = {};
        if (!analyzeCondition(expression, environment)) continue;
        if (environment.keys.length != 2) continue;
        if (!environment.containsKey('compiler')) continue;
        if (!environment.containsKey('runtime')) continue;
        currentSection = fileDiff.tryGetSection(
            environment['compiler'], environment['runtime']);
        failureIndex = 0;
        linesRemovedFromSection = 0;
      } else if (currentSection != null) {
        var match = rexStatusLine.matchAsPrefix(line);
        if (match == null) {
          emitNoise(line);
          continue;
        }
        String testName = match.group(1);
        // Emit failures that should occur before this status line, in order
        // to maintain alphabetical ordering.
        while (failureIndex < currentSection.failures.length) {
          StatusDiffAtom atom = currentSection.failures[failureIndex];
          if (testName.compareTo(atom.testName) >= 0) {
            emitStatusLine(atom.statusLine);
            ++failureIndex;
          } else {
            break;
          }
        }
        // Strip out status lines that either pass or have a new status line
        // generated by the diff.
        if (!currentSection.testNames.contains(testName)) {
          emitStatusLine(line);
        } else {
          ++linesRemovedFromSection;
        }
      } else {
        emitNoise(line);
      }
    }
    flushSection();
    futures.add(new File(filename).writeAsString(output.join('\n')));

    // Warn if some sections could not be found.
    for (var section in fileDiff.sections) {
      if (!emittedSections.contains(section)) {
        print('Missing section $section in $filename');
      }
    }
  }
  return Future.wait(futures);
}

final String usage = """
Usage: status_diff test.log

  where `test.log` is the output from test.py, with the default --progress flag.

This updates the status files in-place!  Use `git diff` to review the changes.
""";

main(List<String> args) async {
  if (args.length != 1) {
    print(usage);
    exit(1);
  }
  String filename = args[0];
  if (!new File(filename).existsSync()) {
    print('Test log not found: $filename');
    exit(1);
  }
  var testLog = await parseTestLog(filename).toList();
  var diff = buildStatusDiff(testLog);
  await applyStatusDiff(diff);
  if (diff.files.isEmpty) {
    print('No status changes found');
  }
}
