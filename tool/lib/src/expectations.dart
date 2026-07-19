/// The `# expect:` output-expectation dialect, shared by the `.gen` test
/// suites (`bin/test.dart`) and the runnable documentation samples
/// (`bin/doc_test.dart`).
library;

/// The `# expect: <text>` comment declaring one line of expected stdout.
final expectedOutputPattern = RegExp(r"# expect: ?(.*)");

/// One expected line of output, with the source line that declared it.
class ExpectedOutput {
  final int line;
  final String output;

  ExpectedOutput(this.line, this.output);

  @override
  String toString() => 'ExpectedOutput(line: $line, output: "$output")';
}

/// Collects the `# expect:` declarations of [lines], in order.
List<ExpectedOutput> parseExpectedOutput(List<String> lines) {
  var expected = <ExpectedOutput>[];
  for (var lineNum = 1; lineNum <= lines.length; lineNum++) {
    var match = expectedOutputPattern.firstMatch(lines[lineNum - 1]);
    if (match != null) expected.add(ExpectedOutput(lineNum, match[1]!));
  }
  return expected;
}

/// Compares interpreter stdout against the expected lines. Returns failure
/// messages; empty when everything matches. Every output line must be
/// declared and every declared line must appear, in order.
List<String> compareOutput(
    List<ExpectedOutput> expected, List<String> outputLines) {
  // Remove the trailing last empty line.
  var lines = List.of(outputLines);
  if (lines.isNotEmpty && lines.last == "") {
    lines.removeLast();
  }

  var failures = <String>[];
  var index = 0;
  for (; index < lines.length; index++) {
    var line = lines[index];
    if (index >= expected.length) {
      failures.add("Got output '$line' when none was expected.");
      continue;
    }

    if (expected[index].output != line) {
      failures.add("Expected output '${expected[index].output}' on line "
          "${expected[index].line}  and got '$line'.");
    }
  }

  while (index < expected.length) {
    failures.add("Missing expected output '${expected[index].output}' on "
        "line ${expected[index].line}.");
    index++;
  }
  return failures;
}
