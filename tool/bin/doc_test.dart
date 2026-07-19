import 'dart:io';

import 'package:args/args.dart';

import 'package:tool/src/expectations.dart';

/// Runs the code samples embedded in the markdown documentation, so the
/// docs cannot drift from the language.
///
/// Fenced blocks tagged ```` ```generic ```` must run cleanly (exit 0); if
/// the block contains `# expect: <text>` comments (the same dialect the
/// `.gen` test suites use), the interpreter's stdout must match them.
/// Blocks tagged ```` ```generic-error ```` must exit with a non-zero
/// status (they demonstrate errors on purpose). Every other fence tag
/// (`text`, `sh`, `rust`, ...) is skipped.
///
/// Samples run from a temp directory; `--fixture name=path` copies a file
/// in beside them (e.g. a built plugin dylib, so `import "demo"` samples
/// resolve).
void main(List<String> arguments) {
  var parser = ArgParser()
    ..addOption("interpreter", abbr: "i", help: "Path to interpreter.")
    ..addMultiOption("docs",
        abbr: "d",
        help: "Markdown files or directories to scan.",
        defaultsTo: ["docs"])
    ..addMultiOption("fixture",
        abbr: "f", help: "name=path file to copy beside the samples.");
  var options = parser.parse(arguments);

  var interpreter = options["interpreter"] as String?;
  if (interpreter == null) {
    print("Missing required option --interpreter.");
    exit(64);
  }

  var files = <File>[];
  for (var path in options["docs"] as List<String>) {
    if (FileSystemEntity.isDirectorySync(path)) {
      files.addAll(Directory(path)
          .listSync(recursive: true)
          .whereType<File>()
          .where((f) => f.path.endsWith(".md")));
    } else {
      files.add(File(path));
    }
  }
  files.sort((a, b) => a.path.compareTo(b.path));

  var checked = 0;
  var failures = <String>[];
  var tempDir = Directory.systemTemp.createTempSync("generic-doc-test");
  try {
    for (var fixture in options["fixture"] as List<String>) {
      var parts = fixture.split("=");
      if (parts.length != 2) {
        print("Malformed --fixture '$fixture', expected name=path.");
        exit(64);
      }
      File(parts[1]).copySync("${tempDir.path}/${parts[0]}");
    }

    for (var file in files) {
      for (var block in _extractBlocks(file)) {
        checked++;
        var failure = _runBlock(interpreter, tempDir, block);
        if (failure != null) {
          failures.add(failure);
          print("FAIL ${block.location}: $failure");
        }
      }
    }
  } finally {
    tempDir.deleteSync(recursive: true);
  }

  if (failures.isNotEmpty) {
    print("\n${failures.length} of $checked documentation samples failed.");
    exit(1);
  }
  print("All $checked documentation samples passed.");
}

class _Block {
  final String location;
  final String tag;
  final List<String> lines;

  _Block(this.location, this.tag, this.lines);
}

/// The fence tags this tool executes.
const _runnableTags = {"generic", "generic-error"};

List<_Block> _extractBlocks(File file) {
  var blocks = <_Block>[];
  var lines = file.readAsLinesSync();
  String? tag;
  int start = 0;
  var code = <String>[];
  for (var i = 0; i < lines.length; i++) {
    var line = lines[i].trim();
    if (tag == null) {
      var match = RegExp(r"^```(\S+)$").firstMatch(line);
      if (match != null && _runnableTags.contains(match[1])) {
        tag = match[1];
        start = i + 1;
        code = [];
      }
    } else if (line == "```") {
      blocks.add(_Block("${file.path}:$start", tag, code));
      tag = null;
    } else {
      code.add(lines[i]);
    }
  }
  return blocks;
}

/// Runs one block; returns a failure description or null on success.
String? _runBlock(String interpreter, Directory tempDir, _Block block) {
  var genFile = File("${tempDir.path}/sample.gen")
    ..writeAsStringSync("${block.lines.join("\n")}\n");
  var result = Process.runSync(File(interpreter).absolute.path, [genFile.path]);

  if (block.tag == "generic-error") {
    return result.exitCode == 0
        ? "expected an error, but the sample ran cleanly"
        : null;
  }

  if (result.exitCode != 0) {
    var firstError = (result.stderr as String)
        .split("\n")
        .firstWhere((l) => l.isNotEmpty, orElse: () => "unknown error");
    return "exited with ${result.exitCode}: $firstError";
  }

  // Verify `# expect:` comments, when the sample carries them. Samples
  // without them only need to run cleanly.
  var expected = parseExpectedOutput(block.lines);
  if (expected.isEmpty) return null;
  var failures = compareOutput(expected, (result.stdout as String).split("\n"));
  return failures.isEmpty ? null : failures.join("; ");
}
