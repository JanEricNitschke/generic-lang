import 'dart:convert';
import 'dart:io';

import 'package:args/args.dart';
import 'package:glob/glob.dart';
import 'package:glob/list_local_fs.dart';
import 'package:path/path.dart' as p;

import 'package:tool/src/term.dart' as term;

/// Runs the tests.

final _expectedOutputPattern = RegExp(r"# expect: ?(.*)");
final _expectedErrorPattern = RegExp(r"# (Error.*)");
final _errorLinePattern = RegExp(r"# \[((java|c) )?line (\d+)\] (Error.*)");
final _expectedRuntimeErrorPattern = RegExp(r"# expect runtime error: (.+)");
final _syntaxErrorPattern = RegExp(r"\[.*line (\d+)\] (Error.+)");
final _stackTracePattern = RegExp(r"\[line (\d+)\]");
final _nonTestPattern = RegExp(r"# nontest");

var _passed = 0;
var _failed = 0;
var _skipped = 0;
var _expectations = 0;

Suite? _suite;
String? _filterPath;
String? _customInterpreter;
List<String>? _customArguments;

final _allSuites = <String, Suite>{};
final _cSuites = <String>[];
final _javaSuites = <String>[];

class Suite {
  final String name;
  final String language;
  final String executable;
  final List<String> args;
  final Map<String, String> tests;

  Suite(this.name, this.language, this.executable, this.args, this.tests);
}

void main(List<String> arguments) {
  _defineTestSuites();

  var parser = ArgParser();

  parser.addOption("interpreter", abbr: "i", help: "Path to interpreter.");
  parser.addMultiOption("arguments",
      abbr: "a", help: "Additional interpreter arguments.");

  var options = parser.parse(arguments);

  if (options.rest.isEmpty) {
    _usageError(parser, "Missing suite name.");
  } else if (options.rest.length > 2) {
    _usageError(
        parser, "Unexpected arguments '${options.rest.skip(2).join(' ')}'.");
  }

  var suite = options.rest[0];
  if (options.rest.length == 2) _filterPath = arguments[1];

  if (options.wasParsed("interpreter")) {
    _customInterpreter = options["interpreter"] as String;
  }

  if (options.wasParsed("arguments")) {
    _customArguments = options["arguments"] as List<String>;

    if (_customInterpreter == null) {
      _usageError(parser,
          "Must pass an interpreter path if providing custom arguments.");
    }
  }

  if (suite == "all") {
    _runSuites(_allSuites.keys.toList());
  } else if (suite == "c") {
    _runSuites(_cSuites);
  } else if (suite == "java") {
    _runSuites(_javaSuites);
  } else if (!_allSuites.containsKey(suite)) {
    print("Unknown interpreter '$suite'");
    exit(1);
  } else if (!_runSuite(suite)) {
    exit(1);
  }
}

void _usageError(ArgParser parser, String message) {
  print(message);
  print("");
  print("Usage: test.dart <suites> [filter] [custom interpreter...]");
  print("");
  print("Optional custom interpreter options:");
  print(parser.usage);
  exit(1);
}

void _runSuites(List<String> names) {
  var anyFailed = false;
  for (var name in names) {
    print("=== $name ===");
    if (!_runSuite(name)) anyFailed = true;
  }

  if (anyFailed) exit(1);
}

bool _runSuite(String name) {
  _suite = _allSuites[name];

  _passed = 0;
  _failed = 0;
  _skipped = 0;
  _expectations = 0;


  for (var file in Glob("test/**.gen").listSync()) {
    _runTest(file.path);
  }

  term.clearLine();

  if (_failed == 0) {
    print("All ${term.green(_passed)} tests passed "
        "($_expectations expectations).");
  } else {
    print("${term.green(_passed)} tests passed. "
        "${term.red(_failed)} tests failed.");
  }

  return _failed == 0;
}

void _runTest(String path) {
  if (path.contains("benchmark")) return;

  // Make a nice short path relative to the working directory. Normalize it to
  // use "/" since the interpreters expect the argument to use that.
  path = p.posix.normalize(path);
  path = path.replaceAll("\\", "/");
  // Check if we are just running a subset of the tests.
  final filterPath = _filterPath;
  if (filterPath != null) {
    var thisTest = p.posix.relative(path, from: "test");
    if (!thisTest.startsWith(filterPath)) return;
  }

  // Update the status line.
  var grayPath = term.gray("($path)");
  term.writeLine("Passed: ${term.green(_passed)} "
      "Failed: ${term.red(_failed)} "
      "Skipped: ${term.yellow(_skipped)} $grayPath");

  // Read the test and parse out the expectations.
  var test = Test(path);

  // See if it's a skipped or non-test file.
  if (!test.parse()) return;

  var failures = test.run();

  // Display the results.
  if (failures.isEmpty) {
    _passed++;
  } else {
    _failed++;
    term.writeLine("${term.red("FAIL")} $path");
    print("");
    for (var failure in failures) {
      print("     ${term.pink(failure)}");
    }
    print("");
  }
}

class ExpectedOutput {
  final int line;
  final String output;

  ExpectedOutput(this.line, this.output);
}

class Test {
  final String _path;

  final _expectedOutput = <ExpectedOutput>[];

  /// The set of expected compile error messages.
  final _expectedErrors = <String>{};

  /// The expected runtime error message or `null` if there should not be one.
  String? _expectedRuntimeError;

  /// If there is an expected runtime error, the line it should occur on.
  int _runtimeErrorLine = 0;

  int _expectedExitCode = 0;

  /// The list of failure message lines.
  final _failures = <String>[];

  Test(this._path);

  bool parse() {
    // Get the path components.
    var parts = _path.split("/");
    // var parts = _path.split(Platform.pathSeparator);
    var subpath = "";
    String? state;
    // Figure out the state of the test. We don't break out of this loop because
    // we want lines for more specific paths to override more general ones.
    for (var part in parts) {
      if (part == ".") continue;
      if (subpath.isNotEmpty) subpath += "/";
      subpath += part;

      if (_suite!.tests.containsKey(subpath)) {
        state = _suite!.tests[subpath];
      }
    }
    if (state == null) {
      throw "Unknown test state for '$_path'.";
    } else if (state == "skip") {
      _skipped++;
      return false;
    }

    var lines = File(_path).readAsLinesSync();
    for (var lineNum = 1; lineNum <= lines.length; lineNum++) {
      var line = lines[lineNum - 1];

      // Not a test file at all, so ignore it.
      var match = _nonTestPattern.firstMatch(line);
      if (match != null) return false;

      match = _expectedOutputPattern.firstMatch(line);
      if (match != null) {
        _expectedOutput.add(ExpectedOutput(lineNum, match[1]!));
        _expectations++;
        continue;
      }

      match = _expectedErrorPattern.firstMatch(line);
      if (match != null) {
        _expectedErrors.add("[$lineNum] ${match[1]}");

        // If we expect a compile error, it should exit with EX_DATAERR.
        _expectedExitCode = 65;
        _expectations++;
        continue;
      }

      match = _errorLinePattern.firstMatch(line);
      if (match != null) {
        // The two interpreters are slightly different in terms of which
        // cascaded errors may appear after an initial compile error because
        // their panic mode recovery is a little different. To handle that,
        // the tests can indicate if an error line should only appear for a
        // certain interpreter.
        var language = match[2];
        if (language == null || language == _suite!.language) {
          _expectedErrors.add("[${match[3]}] ${match[4]}");

          // If we expect a compile error, it should exit with EX_DATAERR.
          _expectedExitCode = 65;
          _expectations++;
        }
        continue;
      }

      match = _expectedRuntimeErrorPattern.firstMatch(line);
      if (match != null) {
        _runtimeErrorLine = lineNum;
        _expectedRuntimeError = match[1];
        // If we expect a runtime error, it should exit with EX_SOFTWARE.
        _expectedExitCode = 70;
        _expectations++;
      }
    }

    if (_expectedErrors.isNotEmpty && _expectedRuntimeError != null) {
      print("${term.magenta('TEST ERROR')} $_path");
      print("     Cannot expect both compile and runtime errors.");
      print("");
      return false;
    }

    // If we got here, it's a valid test.
    return true;
  }

  /// Invoke the interpreter and run the test.
  List<String> run() {
    var args = [
      if (_customInterpreter != null) ...?_customArguments else ..._suite!.args,
      _path
    ];
    var result = Process.runSync(_customInterpreter ?? _suite!.executable, args);

    // Normalize Windows line endings.
    var outputLines = const LineSplitter().convert(result.stdout as String);
    var errorLines = const LineSplitter().convert(result.stderr as String);

    // Validate that an expected runtime error occurred.
    if (_expectedRuntimeError != null) {
      _validateRuntimeError(errorLines);
    } else {
      _validateCompileErrors(errorLines);
    }

    _validateExitCode(result.exitCode, errorLines);
    _validateOutput(outputLines);
    return _failures;
  }

  void _validateRuntimeError(List<String> errorLines) {
    if (errorLines.length < 2) {
      fail("Expected runtime error '$_expectedRuntimeError' and got none.");
      return;
    }

    if (errorLines[0] != _expectedRuntimeError) {
      fail("Expected runtime error '$_expectedRuntimeError' and got:");
      fail(errorLines[0]);
    }

    // Make sure the stack trace has the right line.
    RegExpMatch? match;
    var stackLines = errorLines.sublist(1);
    for (var line in stackLines) {
      match = _stackTracePattern.firstMatch(line);
      if (match != null) break;
    }

    if (match == null) {
      fail("Expected stack trace and got:", stackLines);
    } else {
      var stackLine = int.parse(match[1]!);
      if (stackLine != _runtimeErrorLine) {
        fail("Expected runtime error on line $_runtimeErrorLine "
            "but was on line $stackLine.");
      }
    }
  }

  void _validateCompileErrors(List<String> error_lines) {
    // Validate that every compile error was expected.
    var foundErrors = <String>{};
    var unexpectedCount = 0;
    for (var line in error_lines) {
      var match = _syntaxErrorPattern.firstMatch(line);
      if (match != null) {
        var error = "[${match[1]}] ${match[2]}";
        if (_expectedErrors.contains(error)) {
          foundErrors.add(error);
        } else {
          if (unexpectedCount < 10) {
            fail("Unexpected error:");
            fail(line);
          }
          unexpectedCount++;
        }
      } else if (line != "") {
        if (unexpectedCount < 10) {
          fail("Unexpected output on stderr:");
          fail(line);
        }
        unexpectedCount++;
      }
    }

    if (unexpectedCount > 10) {
      fail("(truncated ${unexpectedCount - 10} more...)");
    }

    // Validate that every expected error occurred.
    for (var error in _expectedErrors.difference(foundErrors)) {
      fail("Missing expected error: $error");
    }
  }

  void _validateExitCode(int exitCode, List<String> errorLines) {
    if (exitCode == _expectedExitCode) return;

    if (errorLines.length > 10) {
      errorLines = errorLines.sublist(0, 10);
      errorLines.add("(truncated...)");
    }

    fail("Expected return code $_expectedExitCode and got $exitCode. Stderr:",
        errorLines);
  }

  void _validateOutput(List<String> outputLines) {
    // Remove the trailing last empty line.
    if (outputLines.isNotEmpty && outputLines.last == "") {
      outputLines.removeLast();
    }

    var index = 0;
    for (; index < outputLines.length; index++) {
      var line = outputLines[index];
      if (index >= _expectedOutput.length) {
        fail("Got output '$line' when none was expected.");
        continue;
      }

      var expected = _expectedOutput[index];
      if (expected.output != line) {
        fail("Expected output '${expected.output}' on line ${expected.line} "
            " and got '$line'.");
      }
    }

    while (index < _expectedOutput.length) {
      var expected = _expectedOutput[index];
      fail("Missing expected output '${expected.output}' on line "
          "${expected.line}.");
      index++;
    }
  }

  void fail(String message, [List<String>? lines]) {
    _failures.add(message);
    if (lines != null) _failures.addAll(lines);
  }
}

void _defineTestSuites() {
  void c(String name, Map<String, String> tests) {
    var executable = name == "clox" ? "build/cloxd" : "build/$name";
    _allSuites[name] = Suite(name, "c", executable, [], tests);
    _cSuites.add(name);
  }

  void java(String name, Map<String, String> tests) {
    var dir = name == "jlox" ? "build/java" : "build/gen/$name";
    _allSuites[name] = Suite(name, "java", "java",
        ["-cp", dir, "com.craftinginterpreters.gen.gen"], tests);
    _javaSuites.add(name);
  }

  // These are just for earlier chapters.
  var earlyChapters = {
    "test/scanning": "skip",
    "test/expressions": "skip",
  };

  // These are just to be imported.
  var onlyImport = {
    "test/import/nested/fib.gen": "skip",
    "test/import/foo.gen": "skip",
    "test/import/circular/circ2.gen": "skip",
    "test/import/circular/circ3.gen": "skip",
    "test/import/nested/nested_rust_stdlib.gen": "skip",
  };

  // JVM doesn't correctly implement IEEE equality on boxed doubles.
  var javaNaNEquality = {
    "test/number/nan_equality.gen": "skip",
  };

  // No hardcoded limits in jlox.
  var noJavaLimits = {
    "test/limit/loop_too_large.gen": "skip",
    "test/limit/no_reuse_constants.gen": "skip",
    "test/limit/too_many_constants.gen": "skip",
    "test/limit/too_many_locals.gen": "skip",
    "test/limit/too_many_upvalues.gen": "skip",

    // Rely on JVM for stack overflow checking.
    "test/limit/stack_overflow.gen": "skip",
  };

  // No classes in Java yet.
  var noJavaClasses = {
    "test/assignment/to_this.gen": "skip",
    "test/call/object.gen": "skip",
    "test/class": "skip",
    "test/closure/close_over_method_parameter.gen": "skip",
    "test/constructor": "skip",
    "test/field": "skip",
    "test/inheritance": "skip",
    "test/method": "skip",
    "test/number/decimal_point_at_eof.gen": "skip",
    "test/number/trailing_dot.gen": "skip",
    "test/operator/equals_class.gen": "skip",
    "test/operator/equals_method.gen": "skip",
    "test/operator/not_class.gen": "skip",
    "test/regression/394.gen": "skip",
    "test/super": "skip",
    "test/this": "skip",
    "test/return/in_method.gen": "skip",
    "test/variable/local_from_method.gen": "skip",
  };

  // No functions in Java yet.
  var noJavaFunctions = {
    "test/call": "skip",
    "test/closure": "skip",
    "test/for/closure_in_body.gen": "skip",
    "test/for/return_closure.gen": "skip",
    "test/for/return_inside.gen": "skip",
    "test/for/syntax.gen": "skip",
    "test/function": "skip",
    "test/operator/not.gen": "skip",
    "test/regression/40.gen": "skip",
    "test/return": "skip",
    "test/unexpected_character.gen": "skip",
    "test/while/closure_in_body.gen": "skip",
    "test/while/return_closure.gen": "skip",
    "test/while/return_inside.gen": "skip",
  };

  // No resolution in Java yet.
  var noJavaResolution = {
    "test/closure/assign_to_shadowed_later.gen": "skip",
    "test/function/local_mutual_recursion.gen": "skip",
    "test/variable/collide_with_parameter.gen": "skip",
    "test/variable/duplicate_local.gen": "skip",
    "test/variable/duplicate_parameter.gen": "skip",
    "test/variable/early_bound.gen": "skip",

    // Broken because we haven"t fixed it yet by detecting the error.
    "test/return/at_top_level.gen": "skip",
    "test/variable/use_local_in_initializer.gen": "skip",
  };

  // No control flow in C yet.
  var noCControlFlow = {
    "test/block/empty.gen": "skip",
    "test/for": "skip",
    "test/if": "skip",
    "test/limit/loop_too_large.gen": "skip",
    "test/logical_operator": "skip",
    "test/variable/unreached_undefined.gen": "skip",
    "test/while": "skip",
  };

  // No functions in C yet.
  var noCFunctions = {
    "test/call": "skip",
    "test/closure": "skip",
    "test/for/closure_in_body.gen": "skip",
    "test/for/return_closure.gen": "skip",
    "test/for/return_inside.gen": "skip",
    "test/for/syntax.gen": "skip",
    "test/function": "skip",
    "test/limit/no_reuse_constants.gen": "skip",
    "test/limit/stack_overflow.gen": "skip",
    "test/limit/too_many_constants.gen": "skip",
    "test/limit/too_many_locals.gen": "skip",
    "test/limit/too_many_upvalues.gen": "skip",
    "test/regression/40.gen": "skip",
    "test/return": "skip",
    "test/unexpected_character.gen": "skip",
    "test/variable/collide_with_parameter.gen": "skip",
    "test/variable/duplicate_parameter.gen": "skip",
    "test/variable/early_bound.gen": "skip",
    "test/while/closure_in_body.gen": "skip",
    "test/while/return_closure.gen": "skip",
    "test/while/return_inside.gen": "skip",
  };

  // No classes in C yet.
  var noCClasses = {
    "test/assignment/to_this.gen": "skip",
    "test/call/object.gen": "skip",
    "test/class": "skip",
    "test/closure/close_over_method_parameter.gen": "skip",
    "test/constructor": "skip",
    "test/field": "skip",
    "test/inheritance": "skip",
    "test/method": "skip",
    "test/number/decimal_point_at_eof.gen": "skip",
    "test/number/trailing_dot.gen": "skip",
    "test/operator/equals_class.gen": "skip",
    "test/operator/equals_method.gen": "skip",
    "test/operator/not.gen": "skip",
    "test/operator/not_class.gen": "skip",
    "test/regression/394.gen": "skip",
    "test/return/in_method.gen": "skip",
    "test/super": "skip",
    "test/this": "skip",
    "test/variable/local_from_method.gen": "skip",
  };

  // No inheritance in C yet.
  var noCInheritance = {
    "test/class/local_inherit_other.gen": "skip",
    "test/class/local_inherit_self.gen": "skip",
    "test/class/inherit_self.gen": "skip",
    "test/class/inherited_method.gen": "skip",
    "test/inheritance": "skip",
    "test/regression/394.gen": "skip",
    "test/super": "skip",
  };

  java("jlox", {
    "test": "pass",
    ...earlyChapters,
    ...javaNaNEquality,
    ...noJavaLimits,
  });

  java("chap04_scanning", {
    // No interpreter yet.
    "test": "skip",
    "test/scanning": "pass"
  });

  // No test for chapter 5. It just has a hardcoded main() in AstPrinter.

  java("chap06_parsing", {
    // No real interpreter yet.
    "test": "skip",
    "test/expressions/parse.gen": "pass"
  });

  java("chap07_evaluating", {
    // No real interpreter yet.
    "test": "skip",
    "test/expressions/evaluate.gen": "pass"
  });

  java("chap08_statements", {
    "test": "pass",
    ...earlyChapters,
    ...javaNaNEquality,
    ...noJavaLimits,
    ...noJavaFunctions,
    ...noJavaResolution,
    ...noJavaClasses,

    // No control flow.
    "test/block/empty.gen": "skip",
    "test/for": "skip",
    "test/if": "skip",
    "test/logical_operator": "skip",
    "test/while": "skip",
    "test/variable/unreached_undefined.gen": "skip",
  });

  java("chap09_control", {
    "test": "pass",
    ...earlyChapters,
    ...javaNaNEquality,
    ...noJavaLimits,
    ...noJavaFunctions,
    ...noJavaResolution,
    ...noJavaClasses,
  });

  java("chap10_functions", {
    "test": "pass",
    ...earlyChapters,
    ...javaNaNEquality,
    ...noJavaLimits,
    ...noJavaResolution,
    ...noJavaClasses,
  });

  java("chap11_resolving", {
    "test": "pass",
    ...earlyChapters,
    ...javaNaNEquality,
    ...noJavaLimits,
    ...noJavaClasses,
  });

  java("chap12_classes", {
    "test": "pass",
    ...earlyChapters,
    ...noJavaLimits,
    ...javaNaNEquality,

    // No inheritance.
    "test/class/local_inherit_other.gen": "skip",
    "test/class/local_inherit_self.gen": "skip",
    "test/class/inherit_self.gen": "skip",
    "test/class/inherited_method.gen": "skip",
    "test/inheritance": "skip",
    "test/regression/394.gen": "skip",
    "test/super": "skip",
  });

  java("chap13_inheritance", {
    "test": "pass",
    ...earlyChapters,
    ...javaNaNEquality,
    ...noJavaLimits,
  });

  c("clox", {
    "test": "pass",
    ...earlyChapters,
    ...onlyImport,
  });

  c("chap17_compiling", {
    // No real interpreter yet.
    "test": "skip",
    "test/expressions/evaluate.gen": "pass",
  });

  c("chap18_types", {
    // No real interpreter yet.
    "test": "skip",
    "test/expressions/evaluate.gen": "pass",
  });

  c("chap19_strings", {
    // No real interpreter yet.
    "test": "skip",
    "test/expressions/evaluate.gen": "pass",
  });

  c("chap20_hash", {
    // No real interpreter yet.
    "test": "skip",
    "test/expressions/evaluate.gen": "pass",
  });

  c("chap21_global", {
    "test": "pass",
    ...earlyChapters,
    ...noCControlFlow,
    ...noCFunctions,
    ...noCClasses,

    // No blocks.
    "test/assignment/local.gen": "skip",
    "test/variable/in_middle_of_block.gen": "skip",
    "test/variable/in_nested_block.gen": "skip",
    "test/variable/scope_reuse_in_different_blocks.gen": "skip",
    "test/variable/shadow_and_local.gen": "skip",
    "test/variable/undefined_local.gen": "skip",

    // No local variables.
    "test/block/scope.gen": "skip",
    "test/variable/duplicate_local.gen": "skip",
    "test/variable/shadow_global.gen": "skip",
    "test/variable/shadow_local.gen": "skip",
    "test/variable/use_local_in_initializer.gen": "skip",
  });

  c("chap22_local", {
    "test": "pass",
    ...earlyChapters,
    ...noCControlFlow,
    ...noCFunctions,
    ...noCClasses,
  });

  c("chap23_jumping", {
    "test": "pass",
    ...earlyChapters,
    ...noCFunctions,
    ...noCClasses,
  });

  c("chap24_calls", {
    "test": "pass",
    ...earlyChapters,
    ...noCClasses,

    // No closures.
    "test/closure": "skip",
    "test/for/closure_in_body.gen": "skip",
    "test/for/return_closure.gen": "skip",
    "test/function/local_recursion.gen": "skip",
    "test/limit/too_many_upvalues.gen": "skip",
    "test/regression/40.gen": "skip",
    "test/while/closure_in_body.gen": "skip",
    "test/while/return_closure.gen": "skip",
  });

  c("chap25_closures", {
    "test": "pass",
    ...earlyChapters,
    ...noCClasses,
  });

  c("chap26_garbage", {
    "test": "pass",
    ...earlyChapters,
    ...noCClasses,
  });

  c("chap27_classes", {
    "test": "pass",
    ...earlyChapters,
    ...noCInheritance,

    // No methods.
    "test/assignment/to_this.gen": "skip",
    "test/class/local_reference_self.gen": "skip",
    "test/class/reference_self.gen": "skip",
    "test/closure/close_over_method_parameter.gen": "skip",
    "test/constructor": "skip",
    "test/field/get_and_set_method.gen": "skip",
    "test/field/method.gen": "skip",
    "test/field/method_binds_this.gen": "skip",
    "test/method": "skip",
    "test/operator/equals_class.gen": "skip",
    "test/operator/equals_method.gen": "skip",
    "test/return/in_method.gen": "skip",
    "test/this": "skip",
    "test/variable/local_from_method.gen": "skip",
  });

  c("chap28_methods", {
    "test": "pass",
    ...earlyChapters,
    ...noCInheritance,
  });

  c("chap29_superclasses", {
    "test": "pass",
    ...earlyChapters,
  });

  c("chap30_optimization", {
    "test": "pass",
    ...earlyChapters,
  });
}
