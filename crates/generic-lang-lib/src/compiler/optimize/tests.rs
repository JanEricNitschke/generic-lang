use super::constant_fold::*;
use super::loop_unrolling::*;
use super::walk::{Visitor, walk_stmt_kind};
use super::*;
use crate::compiler::ast::{
    BinaryOp, BoolOperator, Expr, ExprKind, FstringPart, Literal, Stmt, StmtKind, UnaryOp,
};
use crate::compiler::front::Parser;
use crate::scanner::Scanner;
use crate::types::Location;
use num_bigint::BigInt;
use std::str::FromStr;

fn parse_expr(source: &str) -> Expr {
    let scanner = Scanner::new(
        source,
        #[cfg(feature = "debug_scanner")]
        false,
    );
    let mut parser = Parser::new(
        scanner,
        #[cfg(feature = "debug_parser")]
        false,
    );
    parser.advance();
    let expr = parser.expression();
    assert!(!parser.had_error, "failed to parse {source}");
    expr
}

fn parse_program(source: &str) -> Program {
    let scanner = Scanner::new(
        source,
        #[cfg(feature = "debug_scanner")]
        false,
    );
    let mut parser = Parser::new(
        scanner,
        #[cfg(feature = "debug_parser")]
        false,
    );
    let program = parser.parse_program();
    assert!(!parser.had_error, "failed to parse {source}");
    program
}

fn optimize_source(source: &str) -> Program {
    let mut program = parse_program(source);
    program.optimize();
    program
}

#[derive(Default)]
struct RecordingVisitor {
    statements: Vec<&'static str>,
}

impl Visitor for RecordingVisitor {
    fn visit_stmt_kind(&mut self, stmt: &mut StmtKind) {
        self.statements.push(match stmt {
            StmtKind::Expression { .. } => "expression",
            StmtKind::VarDecl { .. } => "var",
            StmtKind::Block { .. } => "block",
            StmtKind::FunctionDecl { .. } => "function",
            StmtKind::Class(_) => "class",
            StmtKind::Conditional { .. } => "conditional",
            StmtKind::Loop { .. } => "loop",
            StmtKind::Break { .. } => "break",
            StmtKind::Continue { .. } => "continue",
            StmtKind::For { .. } => "for",
            StmtKind::Foreach { .. } => "foreach",
            StmtKind::Return { .. } => "return",
            StmtKind::Throw { .. } => "throw",
            StmtKind::Try(_) => "try",
            StmtKind::Switch { .. } => "switch",
            StmtKind::Import { .. } => "import",
            StmtKind::ImportFrom { .. } => "import-from",
        });
        walk_stmt_kind(self, stmt);
    }
}

fn function_body(program: &Program) -> &[Stmt] {
    match &program.declarations[0].kind {
        StmtKind::FunctionDecl { fn_lit, .. } => &fn_lit.body,
        other => panic!("expected function declaration, got {other:?}"),
    }
}

fn expression_statement_expr(stmt: &StmtKind) -> &Expr {
    match stmt {
        StmtKind::Expression { expr } => expr,
        other => panic!("expected expression statement, got {other:?}"),
    }
}

fn fold(source: &str) -> (Expr, Location) {
    let mut expr = parse_expr(source);
    let location = expr.location;
    fold_expr(&mut expr);
    (expr, location)
}

fn folded_literal(source: &str) -> Literal {
    let (expr, location) = fold(source);
    assert_eq!(expr.location, location, "location changed for {source}");
    literal_expr(&expr)
        .unwrap_or_else(|| panic!("expected folded literal for {source}, got {:?}", expr.kind))
}

fn literal_expr(expr: &Expr) -> Option<Literal> {
    match &expr.kind {
        ExprKind::Literal(literal) => Some(literal.clone()),
        _ => None,
    }
}

fn assert_int(source: &str, expected: i64) {
    match folded_literal(source) {
        Literal::Int(value) => assert_eq!(value, expected, "{source}"),
        other => panic!("expected int for {source}, got {other:?}"),
    }
}

fn assert_bigint(source: &str, expected: &str) {
    let expected = BigInt::from_str(expected).unwrap();
    match folded_literal(source) {
        Literal::BigInt(value) => assert_eq!(value, expected, "{source}"),
        other => panic!("expected big int for {source}, got {other:?}"),
    }
}

fn assert_float(source: &str, expected: f64) {
    match folded_literal(source) {
        Literal::Float(value) => {
            assert!((value - expected).abs() < f64::EPSILON, "{source}: {value}");
        }
        other => panic!("expected float for {source}, got {other:?}"),
    }
}

fn assert_bool(source: &str, expected: bool) {
    match folded_literal(source) {
        Literal::True => assert!(expected, "{source}"),
        Literal::False => assert!(!expected, "{source}"),
        other => panic!("expected bool for {source}, got {other:?}"),
    }
}

fn assert_str(source: &str, expected: &str) {
    match folded_literal(source) {
        Literal::Str(value) => assert_eq!(value, expected, "{source}"),
        other => panic!("expected string for {source}, got {other:?}"),
    }
}

fn assert_binary_kept(source: &str, expected: BinaryOp) {
    let (expr, location) = fold(source);
    assert_eq!(expr.location, location, "location changed for {source}");
    match expr.kind {
        ExprKind::Binary { op, .. } => assert_eq!(op, expected, "{source}"),
        other => panic!("expected binary expression for {source}, got {other:?}"),
    }
}

fn assert_unary_kept(source: &str, expected: UnaryOp) {
    let (expr, location) = fold(source);
    assert_eq!(expr.location, location, "location changed for {source}");
    match expr.kind {
        ExprKind::Unary { op, .. } => assert_eq!(op, expected, "{source}"),
        other => panic!("expected unary expression for {source}, got {other:?}"),
    }
}

fn assert_tuple_kept(source: &str) {
    let (expr, location) = fold(source);
    assert_eq!(expr.location, location, "location changed for {source}");
    assert!(
        tuple_expr(&expr),
        "expected tuple expression for {source}, got {:?}",
        expr.kind
    );
}

fn tuple_expr(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Tuple { .. } => true,
        ExprKind::Grouping { inner } => tuple_expr(inner),
        _ => false,
    }
}

fn assert_rational(source: &str, expected_numerator: i64, expected_denominator: i64) {
    assert_big_rational(
        source,
        &expected_numerator.to_string(),
        &expected_denominator.to_string(),
    );
}

fn assert_big_rational(source: &str, expected_numerator: &str, expected_denominator: &str) {
    let expected_numerator = BigInt::from_str(expected_numerator).unwrap();
    let expected_denominator = BigInt::from_str(expected_denominator).unwrap();
    match folded_literal(source) {
        Literal::Rational {
            numerator,
            denominator,
        } => {
            assert_eq!(numerator, expected_numerator, "{source}");
            assert_eq!(denominator, expected_denominator, "{source}");
        }
        other => panic!("expected rational for {source}, got {other:?}"),
    }
}

#[test]
fn folds_integer_arithmetic() {
    assert_int("1 + 2", 3);
    assert_int("5 - 8", -3);
    assert_int("6 * 7", 42);
    assert_int("1 + 2 * 3", 7);
    assert_int("(1 + 2) * 3", 9);
    assert_int("((1))", 1);
    assert_int("(((1 + 2)))", 3);
}

#[test]
fn folds_overflowing_integer_arithmetic_to_bigint() {
    assert_bigint("9223372036854775807 + 1", "9223372036854775808");
    assert_bigint("9223372036854775808 - 1", "9223372036854775807");
    assert_bigint("9223372036854775807 * 2", "18446744073709551614");
    assert_unary_kept("-(-9223372036854775807 - 1)", UnaryOp::Neg);
}

#[test]
fn folds_float_and_mixed_arithmetic() {
    assert_float("1.5 + 2.25", 3.75);
    assert_float("1 + 2.5", 3.5);
    assert_float("2.5 * 2", 5.0);
    assert_float("1.5 - 2", -0.5);
}

#[test]
fn folds_string_concatenation() {
    assert_str(r#""foo" + "bar""#, "foobar");
    assert_str(r#""a" + ("" + "b")"#, "ab");
}

#[test]
fn folds_unary_negation() {
    assert_int("-5", -5);
    assert_float("-1.5", -1.5);
    assert_unary_kept("-9223372036854775808", UnaryOp::Neg);
}

#[test]
fn folds_not_on_literals() {
    assert_bool("!true", false);
    assert_bool("!false", true);
    assert_bool("!nil", true);
    assert_bool("!0", true);
    assert_bool("!1", false);
    assert_bool(r#"!"""#, true);
    assert_bool(r#"!"x""#, false);
    assert_bool("!!true", true);
    assert_bool("!0.0", true);
    assert_bool("!(0 / 5)", true);
    assert_bool("!()", true);
    assert_bool("!(1,)", false);
    assert_bool("!9223372036854775808", false);
    assert!(!literal_truthy(&Literal::BigInt(BigInt::from(0))));
}

#[test]
fn leaves_unsupported_operators_alone() {
    assert_binary_kept("1 / 0", BinaryOp::Div);
    assert_binary_kept("1 // 0", BinaryOp::FloorDiv);
    assert_binary_kept("1 % 0", BinaryOp::Mod);
    assert_binary_kept("1 is 1", BinaryOp::Is);
    assert_binary_kept("1 in [1]", BinaryOp::In);
    assert_binary_kept("1 + \"x\"", BinaryOp::Add);
    assert_binary_kept("\"a\" * 2.5", BinaryOp::Mul);
    assert_unary_kept("-true", UnaryOp::Neg);
    assert_unary_kept("!-true", UnaryOp::Not);
}

#[test]
fn leaves_logical_ternary_and_collections_alone() {
    let (expr, _) = fold("true and false");
    assert!(matches!(
        expr.kind,
        ExprKind::Logical {
            op: BoolOperator::And,
            ..
        }
    ));

    let (expr, _) = fold("true ? 1 : 2");
    assert!(matches!(expr.kind, ExprKind::Ternary { .. }));

    let (expr, _) = fold("[1 + 2, 3]");
    match expr.kind {
        ExprKind::ListLit { items } => {
            assert!(matches!(items[0].kind, ExprKind::Literal(Literal::Int(3))));
        }
        other => panic!("expected list, got {other:?}"),
    }
}

#[test]
fn folds_inside_fstring_and_tstring_interpolations() {
    let (expr, _) = fold(r#"f"${1 + 2}""#);
    match expr.kind {
        ExprKind::Fstring { parts } => match &parts[0] {
            FstringPart::Str(_) => panic!("expected interpolation first"),
            FstringPart::Interp(expr) => {
                assert!(matches!(expr.kind, ExprKind::Literal(Literal::Int(3))));
            }
        },
        other => panic!("expected fstring, got {other:?}"),
    }

    let (expr, _) = fold(r#"t"${1 + 2}""#);
    match expr.kind {
        ExprKind::Tstring(tstring) => {
            assert!(matches!(
                tstring.interpolations[0].value.kind,
                ExprKind::Literal(Literal::Int(3))
            ));
        }
        other => panic!("expected tstring, got {other:?}"),
    }
}

#[test]
fn folds_tuples_of_literals() {
    match folded_literal("(1, 2)") {
        Literal::Tuple(items) => {
            assert!(matches!(items[0], Literal::Int(1)));
            assert!(matches!(items[1], Literal::Int(2)));
        }
        other => panic!("expected tuple literal, got {other:?}"),
    }
    match folded_literal("((1, 2), 3)") {
        Literal::Tuple(items) => {
            assert!(matches!(items[0], Literal::Tuple(_)));
            assert!(matches!(items[1], Literal::Int(3)));
        }
        other => panic!("expected tuple literal, got {other:?}"),
    }
    assert!(matches!(folded_literal("()"), Literal::Tuple(items) if items.is_empty()));
}

#[test]
fn keeps_tuples_with_non_literals_unfolded() {
    assert_tuple_kept("(1, [2])");
    assert_tuple_kept("(1, {2: 3})");
    assert_tuple_kept("(1, {2})");
    assert_tuple_kept("(1, ->() 2)");
    assert_tuple_kept("(1, value)");
    assert_tuple_kept("((1, [2]), 3)");
}

#[test]
fn folds_string_repetition() {
    assert_str(r#""ab" * 3"#, "ababab");
    assert_str(r#"3 * "ab""#, "ababab");
    assert_str(r#""a" * 0"#, "");
    assert_str(r#""a" * -2"#, "");
    assert_str(r#""" * 100000"#, "");

    assert_binary_kept(r#""a" * 100000"#, BinaryOp::Mul);
    assert_binary_kept(r#""abcdefghij" * 9223372036854775807"#, BinaryOp::Mul);
    assert_binary_kept(r#""a" * 9223372036854775808"#, BinaryOp::Mul);
}

#[test]
fn respects_the_string_length_cap() {
    let long = "a".repeat(MAX_STRING_BYTES);
    let source = format!("\"{long}\" + \"b\"");
    assert_binary_kept(&source, BinaryOp::Add);

    let source = format!("\"{}\" * 2", "a".repeat(MAX_STRING_BYTES / 2 + 1));
    assert_binary_kept(&source, BinaryOp::Mul);
}

#[test]
fn folds_division() {
    assert_rational("8 / 2", 4, 1);
    assert_rational("1 / 3", 1, 3);
    assert_rational("6 / 4", 3, 2);
    assert_rational("-1 / 3", -1, 3);
    assert_rational("1 / -3", -1, 3);
    assert_big_rational(
        "170141183460469231731687303715884105727 / 3",
        "170141183460469231731687303715884105727",
        "3",
    );
    assert_float("1.0 / 2", 0.5);
    assert_float("3 / 2.0", 1.5);

    assert_binary_kept("1 / 0", BinaryOp::Div);
    assert_binary_kept("1 / 0.0", BinaryOp::Div);
}

#[test]
fn folds_floor_division_and_modulo() {
    assert_int("7 // 2", 3);
    assert_int("-7 // 2", -3);
    assert_int("7 % 3", 1);
    assert_int("-7 % 3", -1);
    assert_float("7.5 // 2", 3.0);
    assert_float("7 % 2.5", 2.0);

    assert_binary_kept("(-9223372036854775807 - 1) // -1", BinaryOp::FloorDiv);
    assert_int("(-9223372036854775807 - 1) % -1", 0);

    assert_binary_kept("1 // 0", BinaryOp::FloorDiv);
    assert_binary_kept("1 % 0", BinaryOp::Mod);
    assert_binary_kept("1.0 // 0.0", BinaryOp::FloorDiv);
    assert_binary_kept("1.0 % 0.0", BinaryOp::Mod);
}

#[test]
fn folds_bounded_exponentiation() {
    assert_int("2 ** 10", 1024);
    assert_int("(-2) ** 3", -8);
    assert_int("(-2) ** 4", 16);
    assert_int("0 ** 0", 1);

    assert_binary_kept("9223372036854775808 ** 2", BinaryOp::Exp);
    assert_binary_kept("2 ** 63", BinaryOp::Exp);
    assert_binary_kept("2 ** -1", BinaryOp::Exp);
    assert_binary_kept("2 ** 100000", BinaryOp::Exp);
    assert_binary_kept("2 ** 9223372036854775808", BinaryOp::Exp);
}

#[test]
fn respects_the_bigint_bit_cap() {
    let huge = "9".repeat(2000);
    let source = format!("{huge} + 1");
    assert_binary_kept(&source, BinaryOp::Add);

    let source = format!("{huge} * {huge}");
    assert_binary_kept(&source, BinaryOp::Mul);

    let source = format!("{huge} / 3");
    assert_binary_kept(&source, BinaryOp::Div);

    let source = format!("3 / {huge}");
    assert_binary_kept(&source, BinaryOp::Div);
}

#[test]
fn folds_not_on_rationals() {
    assert_bool("!(1 / 2)", false);
}

#[test]
fn folds_literal_comparisons() {
    assert_bool("1 < 2", true);
    assert_bool("1 <= 1", true);
    assert_bool("1 > 2", false);
    assert_bool("2 >= 2", true);
    assert_bool("1 == 2", false);
    assert_bool("1 != 2", true);
    assert_bool("1 == 1.0", true);
    assert_bool("0.0 == 0", true);
    assert_bool("9223372036854775808 == 9223372036854775808", true);
    assert_bool("9223372036854775808 < 9223372036854775809", true);
    assert_bool("1 / 3 < 1 / 2", true);
    assert_bool("1 / 2 == 2 / 4", true);
    assert_bool(r#""a" < "b""#, true);
    assert_bool(r#""a" == "a""#, true);
    assert_bool("true == true", true);
    assert_bool("nil == nil", true);
    assert_bool("(1, 2) == (1, 2)", true);
    assert_bool("(1, 2) == (1, 3)", false);
    assert_bool("(1, 2) != (1, 3)", true);
    assert_bool("1 == \"1\"", false);

    assert_binary_kept("0.0 / 0.0 == 0.0 / 0.0", BinaryOp::Equal);
    assert_binary_kept("1 / 2 == 0.5", BinaryOp::Equal);
    assert_binary_kept("1 / 2 < 0.75", BinaryOp::Less);
}

#[test]
fn drops_statements_after_terminators() {
    let program = optimize_source(
        r#"
            fun f() {
                print("before");
                return 1;
                print("dead");
            }
            fun g() {
                throw "boom";
                print("dead");
            }
            fun h() {
                while (true) {
                    break;
                    print("dead");
                }
                return 0;
            }
            "#,
    );
    let f = match &program.declarations[0].kind {
        StmtKind::FunctionDecl { fn_lit, .. } => &fn_lit.body,
        other => panic!("expected function, got {other:?}"),
    };
    assert_eq!(f.len(), 2);
    assert!(matches!(f[1].kind, StmtKind::Return { .. }));

    let g = match &program.declarations[1].kind {
        StmtKind::FunctionDecl { fn_lit, .. } => &fn_lit.body,
        other => panic!("expected function, got {other:?}"),
    };
    assert_eq!(g.len(), 1);
    assert!(matches!(g[0].kind, StmtKind::Throw { .. }));

    let h = match &program.declarations[2].kind {
        StmtKind::FunctionDecl { fn_lit, .. } => &fn_lit.body,
        other => panic!("expected function, got {other:?}"),
    };
    match &h[0].kind {
        StmtKind::Loop { body, .. } => {
            assert_eq!(body.len(), 1);
            assert!(matches!(body[0].kind, StmtKind::Break { .. }));
        }
        other => panic!("expected loop, got {other:?}"),
    }
}

#[test]
fn simplifies_constant_conditionals() {
    let program = optimize_source(
        r#"
            if (true) { print("then"); } else { print("else"); }
            if (false) { print("dead"); }
            unless (false) { print("unless"); } else { print("dead"); }
            "#,
    );
    assert_eq!(program.declarations.len(), 2);
    for (declaration, expected) in program.declarations.iter().zip(["then", "unless"]) {
        match &declaration.kind {
            StmtKind::Block { stmts } => {
                assert_eq!(stmts.len(), 1);
                let expr = expression_statement_expr(&stmts[0].kind);
                match &expr.kind {
                    ExprKind::Call { args, .. } => {
                        assert!(matches!(
                            &args[0].value.kind,
                            ExprKind::Literal(Literal::Str(value)) if value == expected
                        ));
                    }
                    other => panic!("expected call, got {other:?}"),
                }
            }
            other => panic!("expected block, got {other:?}"),
        }
    }
}

#[test]
fn drops_statically_false_loops() {
    let program = optimize_source(
        r#"
            fun f() {
                while (false) { print("dead"); }
                until (true) { print("dead"); }
                for (var i = 0; false; i = i + 1) { print("dead"); }
                while (true) { break; }
            }
            "#,
    );
    let body = function_body(&program);
    assert_eq!(body.len(), 1);
    assert!(matches!(body[0].kind, StmtKind::Loop { .. }));
}

#[test]
fn keeps_false_for_initializer_side_effects() {
    let program = optimize_source(
        r#"
            fun f() {
                for (var x = print("init"); false; x = x) { print("dead"); }
            }
            "#,
    );
    let body = function_body(&program);
    assert_eq!(body.len(), 1);
    match &body[0].kind {
        StmtKind::Block { stmts } => {
            assert_eq!(stmts.len(), 1);
            assert!(matches!(stmts[0].kind, StmtKind::VarDecl { .. }));
        }
        other => panic!("expected init block, got {other:?}"),
    }
}

#[test]
fn drops_pure_expression_statements() {
    let program = optimize_source(
        r#"
            fun f() {
                1;
                "unused";
                (2 + 3);
                nil;
                print("kept");
            }
            "#,
    );
    let body = function_body(&program);
    assert_eq!(body.len(), 1);
    let expr = expression_statement_expr(&body[0].kind);
    assert!(matches!(expr.kind, ExprKind::Call { .. }));
}

#[test]
fn drops_pure_expression_statements_at_top_level() {
    let program = optimize_source(
        r#"
            1;
            "unused";
            (2 + 3);
            nil;
            print("kept");
            "#,
    );
    assert_eq!(program.declarations.len(), 1);
    let expr = expression_statement_expr(&program.declarations[0].kind);
    assert!(matches!(expr.kind, ExprKind::Call { .. }));
}

#[test]
fn unrolls_loops_at_top_level() {
    let program = optimize_source(
        r#"
            for (var i = 0; i < 3; i = i + 1) {
                print(i);
            }
            "#,
    );
    assert_iteration_values(&program.declarations, &[0, 1, 2], "top-level loop");
}

#[test]
fn drops_statements_after_continue() {
    let program = optimize_source(
        r#"
            fun f() {
                while (true) {
                    continue;
                    print("dead");
                }
            }
            "#,
    );
    let body = function_body(&program);
    let StmtKind::Loop { body, .. } = &body[0].kind else {
        panic!("expected loop");
    };
    assert_eq!(body.len(), 1);
    assert!(matches!(body[0].kind, StmtKind::Continue { .. }));
}

#[test]
fn keeps_expression_statements_that_can_raise_or_have_effects() {
    let program = optimize_source(
        r#"
            fun f() {
                [1, 2];
                ({1: 2});
                missing;
                print("call");
            }
            "#,
    );
    let body = function_body(&program);
    assert_eq!(body.len(), 4);
}

#[test]
fn visits_loop_initializers_through_the_statement_hook() {
    let mut program = parse_program(
        "
            fun f() {
                for (var i = 0; i < 1; i = i + 1) {
                    print(i);
                }
            }
            ",
    );
    let mut visitor = RecordingVisitor::default();
    visitor.visit_stmts(&mut program.declarations);
    assert_eq!(visitor.statements, ["function", "for", "var", "expression"]);
}

#[test]
fn removes_empty_blocks() {
    let program = optimize_source(
        "
            fun f() {
                {}
                { var x = 1; }
            }
            ",
    );
    let body = function_body(&program);
    assert_eq!(body.len(), 1);
    assert!(matches!(body[0].kind, StmtKind::Block { ref stmts } if stmts.len() == 1));
}

#[test]
fn removes_nested_empty_blocks() {
    let program = optimize_source(
        "
            fun f() {
                { { } }
                if (true) { { } }
            }
            ",
    );
    let body = function_body(&program);
    assert!(body.is_empty());
}

#[test]
fn unrolls_c_style_for_loops() {
    let program = optimize_source(
        "
            fun f() {
                for (var i = 0; i < 3; i = i + 1) {
                    print(i);
                }
            }
            ",
    );
    let body = function_body(&program);
    assert_eq!(body.len(), 3);
    for (iteration, (stmt, expected)) in body.iter().zip([0, 1, 2]).enumerate() {
        let StmtKind::Block { stmts } = &stmt.kind else {
            panic!("expected iteration block");
        };
        let StmtKind::VarDecl {
            initializer: Some(initializer),
            ..
        } = &stmts[0].kind
        else {
            panic!("expected loop variable declaration");
        };
        assert!(
            matches!(
                initializer.kind,
                ExprKind::Literal(Literal::Int(value)) if value == expected
            ),
            "iteration {iteration}"
        );
    }
}

fn assert_iteration_values(stmts: &[Stmt], expected: &[i64], context: &str) {
    assert_eq!(stmts.len(), expected.len(), "{context}");
    for (stmt, expected) in stmts.iter().zip(expected) {
        let StmtKind::Block { stmts } = &stmt.kind else {
            panic!("expected iteration block for {context}");
        };
        let StmtKind::VarDecl {
            initializer: Some(initializer),
            ..
        } = &stmts[0].kind
        else {
            panic!("expected loop variable declaration for {context}");
        };
        assert!(
            matches!(
                initializer.kind,
                ExprKind::Literal(Literal::Int(value)) if value == *expected
            ),
            "{context}: expected {expected}"
        );
    }
}

fn assert_for_iterations(case: &str, expected: &[i64]) {
    let source = format!("fun f() {{ {case} }}");
    let program = optimize_source(&source);
    assert_iteration_values(function_body(&program), expected, case);
}

#[test]
fn unrolls_loop_comparison_shapes() {
    assert_for_iterations(
        "for (var i = 0; i <= 2; i = i + 1) { print(i); }",
        &[0, 1, 2],
    );
    assert_for_iterations(
        "for (var i = 3; i > 0; i = i - 1) { print(i); }",
        &[3, 2, 1],
    );
    assert_for_iterations(
        "for (var i = 2; i >= 0; i = i - 1) { print(i); }",
        &[2, 1, 0],
    );
    assert_for_iterations(
        "for (var i = -2; i <= 0; i += 1) { print(i); }",
        &[-2, -1, 0],
    );
    assert_for_iterations("for (var i = 2; i >= 0; i -= 1) { print(i); }", &[2, 1, 0]);
    assert_for_iterations(
        "for (var i = 9223372036854775807; i <= 9223372036854775807; i = i + 1) { print(i); }",
        &[i64::MAX],
    );
    assert_for_iterations("for (var i = 5; i < 3; i = i + 1) { print(i); }", &[]);
}

#[test]
fn does_not_unroll_loops_that_never_reach_the_bound() {
    let cases = [
        "for (var i = 0; i < 3; i = i - 1) { print(i); }",
        "for (var i = 3; i > 0; i = i + 1) { print(i); }",
        "for (var i = 0; i < 3; i = i + 0) { print(i); }",
    ];
    for case in cases {
        let source = format!("fun f() {{ {case} }}");
        let program = optimize_source(&source);
        let body = function_body(&program);
        assert!(
            matches!(body[0].kind, StmtKind::For { .. }),
            "unexpectedly unrolled {case}"
        );
    }
}

#[test]
fn does_not_unroll_unsafe_for_loops() {
    let cases = [
        "for (var i = 0; i < 3; i = i + 1) { break; }",
        "for (var i = 0; i < 3; i = i + 1) { continue; }",
        "for (var i = 0; i < 3; i = i + 1) { i = 10; }",
        "for (const i = 0; i < 3; i = i + 1) { print(i); }",
        "for (var i = start; i < 3; i = i + 1) { print(i); }",
    ];
    for case in cases {
        let source = format!("fun f() {{ {case} }}");
        let program = optimize_source(&source);
        let body = function_body(&program);
        assert!(
            matches!(body[0].kind, StmtKind::For { .. }),
            "unexpectedly unrolled {case}"
        );
    }
}

#[test]
fn does_not_unroll_loops_containing_lambdas_functions_or_classes() {
    let cases = [
        "for (var i = 0; i < 3; i = i + 1) { var f = ->() 1; }",
        "for (var i = 0; i < 3; i = i + 1) { fun inner() {} }",
        "for (var i = 0; i < 3; i = i + 1) { class Inner {} }",
        "foreach (var x in (1, 2, 3)) { var f = ->() x; }",
        "foreach (var x in (1, 2, 3)) { fun inner() {} }",
        "foreach (var x in (1, 2, 3)) { class Inner {} }",
    ];
    for case in cases {
        let source = format!("fun f() {{ {case} }}");
        let program = optimize_source(&source);
        let body = function_body(&program);
        assert!(
            matches!(
                body[0].kind,
                StmtKind::For { .. } | StmtKind::Foreach { .. }
            ),
            "unexpectedly unrolled {case}"
        );
    }
}

#[test]
fn does_not_unroll_too_many_trips() {
    let cases = ["for (var i = 0; i < 33; i = i + 1) { print(i); }"];
    for case in cases {
        let source = format!("fun f() {{ {case} }}");
        let program = optimize_source(&source);
        let body = function_body(&program);
        assert!(
            matches!(body[0].kind, StmtKind::For { .. }),
            "unexpectedly unrolled {case}"
        );
    }

    let tuple = format!(
        "({})",
        std::iter::repeat_n("1", MAX_UNROLL_TRIPS + 1)
            .collect::<Vec<_>>()
            .join(", ")
    );
    let source = format!("fun f() {{ foreach (var x in {tuple}) {{ print(x); }} }}");
    let program = optimize_source(&source);
    let body = function_body(&program);
    assert!(matches!(body[0].kind, StmtKind::Foreach { .. }));
}

fn nested_for_source(body_statements: usize) -> String {
    let body = "print(i);".repeat(body_statements);
    format!("fun f() {{ for (var i = 0; i < {MAX_UNROLL_TRIPS}; i = i + 1) {{ {{ {body} }} }} }}")
}

fn nested_foreach_source(body_statements: usize) -> String {
    let body = "print(x);".repeat(body_statements);
    let tuple = (1..=MAX_UNROLL_TRIPS)
        .map(|value| value.to_string())
        .collect::<Vec<_>>()
        .join(", ");
    format!("fun f() {{ foreach (var x in ({tuple})) {{ {{ {body} }} }} }}")
}

#[test]
fn unrolls_nested_bodies_at_the_statement_budget() {
    for source in [nested_for_source(125), nested_foreach_source(125)] {
        let program = optimize_source(&source);
        let body = function_body(&program);
        assert_eq!(body.len(), MAX_UNROLL_TRIPS, "{source}");
    }
}

#[test]
fn does_not_unroll_nested_bodies_past_the_statement_budget() {
    for source in [nested_for_source(126), nested_foreach_source(126)] {
        let program = optimize_source(&source);
        let body = function_body(&program);
        assert!(
            matches!(
                body[0].kind,
                StmtKind::For { .. } | StmtKind::Foreach { .. }
            ),
            "unexpectedly unrolled {source}"
        );
    }
}

#[test]
fn cleans_up_empty_iteration_bodies() {
    let program = optimize_source("fun f() { for (var i = 0; i < 3; i = i + 1) {} }");
    let body = function_body(&program);
    assert_eq!(body.len(), 3);
    for statement in body {
        let StmtKind::Block { stmts } = &statement.kind else {
            panic!("expected iteration block");
        };
        assert_eq!(stmts.len(), 1);
        assert!(matches!(stmts[0].kind, StmtKind::VarDecl { .. }));
    }
}

#[test]
fn statement_count_excludes_function_and_method_bodies() {
    let program = parse_program(
        "
            fun outer() {
                fun inner() {
                    print(1);
                    print(2);
                }
                class Thing {
                    method() {
                        print(3);
                    }
                }
            }
            ",
    );
    let body = function_body(&program);
    assert_eq!(statement_count(body), 3);
}

#[test]
fn unrolls_foreach_over_constant_tuples() {
    let program = optimize_source(
        r#"
            fun f() {
                foreach (var x in (1, 2, 3)) {
                    print(x);
                }
                foreach (var x in ()) {
                    print("dead");
                }
            }
            "#,
    );
    let body = function_body(&program);
    assert_eq!(body.len(), 3);
    for (iteration, (stmt, expected)) in body.iter().zip([1, 2, 3]).enumerate() {
        let StmtKind::Block { stmts } = &stmt.kind else {
            panic!("expected iteration block");
        };
        let StmtKind::VarDecl {
            initializer: Some(initializer),
            ..
        } = &stmts[0].kind
        else {
            panic!("expected loop variable declaration");
        };
        assert!(
            matches!(
                initializer.kind,
                ExprKind::Literal(Literal::Int(value)) if value == expected
            ),
            "iteration {iteration}"
        );
    }
}

#[test]
fn does_not_unroll_foreach_over_stop_iteration() {
    let program = optimize_source(
        "
            fun f() {
                foreach (var x in (1, StopIteration, 2)) {
                    print(x);
                }
            }
            ",
    );
    let body = function_body(&program);
    assert!(matches!(body[0].kind, StmtKind::Foreach { .. }));
}

#[test]
fn unrolls_foreach_that_assigns_the_loop_variable() {
    let program = optimize_source(
        "
            fun f() {
                foreach (var x in (1, 2)) {
                    x = x + 10;
                    print(x);
                }
            }
            ",
    );
    let body = function_body(&program);
    assert_eq!(body.len(), 2);
    for (stmt, expected) in body.iter().zip([1, 2]) {
        let StmtKind::Block { stmts } = &stmt.kind else {
            panic!("expected iteration block");
        };
        assert!(matches!(
            stmts[0].kind,
            StmtKind::VarDecl {
                initializer: Some(ref initializer),
                ..
            } if matches!(initializer.kind, ExprKind::Literal(Literal::Int(value)) if value == expected)
        ));
        let StmtKind::Block { stmts: iteration } = &stmts[1].kind else {
            panic!("expected iteration body block");
        };
        assert!(matches!(iteration[0].kind, StmtKind::Expression { .. }));
    }
}

#[test]
fn does_not_unroll_unsafe_foreach_loops() {
    let cases = [
        "foreach (var x in (1, 2, 3)) { break; }",
        "foreach (var x in (1, 2, 3)) { continue; }",
        "foreach (var x in (1, [2])) { print(x); }",
        "foreach (var x in values) { print(x); }",
    ];
    for case in cases {
        let source = format!("fun f() {{ {case} }}");
        let program = optimize_source(&source);
        let body = function_body(&program);
        assert!(
            matches!(body[0].kind, StmtKind::Foreach { .. }),
            "unexpectedly unrolled {case}"
        );
    }
}

#[test]
fn preserves_the_whole_expression_span() {
    let source = "1 +\n  2";
    let (expr, location) = fold(source);
    assert_eq!(*location.start_line, 1);
    assert_eq!(*location.end_line, 2);
    match expr.kind {
        ExprKind::Literal(Literal::Int(3)) => {}
        other => panic!("expected folded literal, got {other:?}"),
    }
    assert_eq!(expr.location, location);
}
