// Copyright (c) 2013, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

library vmservice_io;

import 'dart:async';
import 'dart:collection';
import 'dart:convert';
import 'dart:io';
import 'dart:isolate';
import 'dart:_vmservice';

part 'loader.dart';
part 'server.dart';

// The TCP ip/port that the HTTP server listens on.
int _port;
String _ip;
// Should the HTTP server auto start?
bool _autoStart;

bool _isWindows = false;
var _signalWatch;
var _signalSubscription;

// HTTP server.
Server server;
Future<Server> serverFuture;

_lazyServerBoot() {
  if (server != null) {
    return;
  }
  // Lazily create service.
  var service = new VMService();
  // Lazily create server.
  server = new Server(service, _ip, _port);
}

Future cleanupCallback() async {
  shutdownLoaders();
  // Cancel the sigquit subscription.
  if (_signalSubscription != null) {
    await _signalSubscription.cancel();
    _signalSubscription = null;
  }
  if (server != null) {
    try {
      await server.cleanup(true);
    } catch (e, st) {
      print("Error in vm-service shutdown: $e\n$st\n");
    }
  }
  if (_registerSignalHandlerTimer != null) {
    _registerSignalHandlerTimer.cancel();
    _registerSignalHandlerTimer = null;
  }
  // Call out to embedder's shutdown callback.
  _shutdown();
}

Future<Uri> createTempDirCallback(String base) async {
  Directory temp = await Directory.systemTemp.createTemp(base);
  return temp.uri;
}

Future deleteDirCallback(Uri path) async {
  Directory dir = new Directory.fromUri(path);
  await dir.delete(recursive: true);
}

Future writeFileCallback(Uri path, List<int> bytes) async {
  var file = await new File.fromUri(path);
  await file.writeAsBytes(bytes);
}

Future<List<int>> readFileCallback(Uri path) async {
  var file = await new File.fromUri(path);
  return await file.readAsBytes();
}

Future<List<Map<String,String>>> listFilesCallback(Uri dirPath) async {
  var dir = new Directory.fromUri(dirPath);
  var dirPathStr = dirPath.path;
  var stream = dir.list(recursive: true);
  var result = [];
  await for (var fileEntity in stream) {
    var filePath = new Uri.file(fileEntity.path).path;
    var stat = await fileEntity.stat();
    if (stat.type == FileSystemEntityType.FILE &&
        filePath.startsWith(dirPathStr)) {
      var map = {};
      map['name'] = '/' + filePath.substring(dirPathStr.length);
      map['size'] = stat.size;
      map['modified'] = stat.modified.millisecondsSinceEpoch;
      result.add(map);
    }
  }
  return result;
}

_clearFuture(_) {
  serverFuture = null;
}

_onSignal(ProcessSignal signal) {
  if (serverFuture != null) {
    // Still waiting.
    return;
  }
  _lazyServerBoot();
  // Toggle HTTP server.
  if (server.running) {
    serverFuture = server.shutdown(true).then(_clearFuture);
  } else {
    serverFuture = server.startup().then(_clearFuture);
  }
}

Timer _registerSignalHandlerTimer;

_registerSignalHandler() {
  _registerSignalHandlerTimer = null;
  if (_signalWatch == null) {
    // Cannot register for signals.
    return;
  }
  if (_isWindows) {
    // Cannot register for signals on Windows.
    return;
  }
  _signalSubscription = _signalWatch(ProcessSignal.SIGQUIT).listen(_onSignal);
}

main() {
  // Set embedder hooks.
  VMServiceEmbedderHooks.cleanup = cleanupCallback;
  VMServiceEmbedderHooks.createTempDir = createTempDirCallback;
  VMServiceEmbedderHooks.deleteDir = deleteDirCallback;
  VMServiceEmbedderHooks.writeFile = writeFileCallback;
  VMServiceEmbedderHooks.readFile = readFileCallback;
  VMServiceEmbedderHooks.listFiles = listFilesCallback;
  // Always instantiate the vmservice object so that the exit message
  // can be delivered and waiting loaders can be cancelled.
  var service = new VMService();
  if (_autoStart) {
    _lazyServerBoot();
    server.startup();
    // It's just here to push an event on the event loop so that we invoke the
    // scheduled microtasks.
    Timer.run(() {});
  }
  scriptLoadPort.handler = _processLoadRequest;
  // Register signal handler after a small delay to avoid stalling main
  // isolate startup.
  _registerSignalHandlerTimer = new Timer(shortDelay, _registerSignalHandler);
  return scriptLoadPort;
}

_shutdown() native "VMServiceIO_Shutdown";
