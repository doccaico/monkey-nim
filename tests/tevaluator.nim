import strformat, macros
import ../src/[ast, lexer, parser, obj, evaluator]
from ./utils import EQ, TRUE, errInfo

proc testEval(input: string): Object =
  var L = initLexer(input)
  var P = initParser(L)
  var program = P.parseProgram()
  var env = newEnvironment()
  return eval(program, env)

proc testIntegerObject(o: Object, expected: int): bool =
  if o.kind != okInteger:
    errinfo.got = $o.kind
    errinfo.expected = $okInteger
    return false
  if o.intVal != expected:
    errinfo.got = $o.intVal
    errinfo.expected = $expected
    return false
  return true

proc testBoolObject(o: Object, expected: bool): bool =
  if o.kind != okBool:
    errinfo.got = $o.kind
    errinfo.expected = $okBool
    return false
  if o.boolVal != expected:
    errinfo.got = $o.boolVal
    errinfo.expected = $expected
    return false
  return true

proc testNullObject(kind: ObjectKind): bool =
  if kind != okNull:
    errinfo.got = $kind
    errinfo.expected = $okNull
    return false
  return true

proc test_eval_integer_exp() =
  let tests1 = [
      ("5", 5),
      ("10", 10),
      ("-5", -5),
      ("-10", -10),
      ("5 + 5 + 5 + 5 - 10", 10),
      ("2 * 2 * 2 * 2 * 2", 32),
      ("-50 + 100 + -50", 0),
      ("5 * 2 + 10", 20),
      ("5 + 2 * 10", 25),
      ("20 + 2 * -10", 0),
      ("50 / 2 * 2 + 10", 60),
      ("2 * (5 + 10)", 30),
      ("3 * 3 * 3 + 10", 37),
      ("3 * (3 * 3) + 10", 37),
      ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
  ]
  for (input, expected) in tests1:
    let evaluated = testEval(input)
    TRUE(testIntegerObject, evaluated, expected)

proc test_eval_bool_exp() =
  let tests1 = [
      ("true", true),
      ("false", false),
      ("1 < 2", true),
      ("1 > 2", false),
      ("1 < 1", false),
      ("1 > 1", false),
      ("1 == 1", true),
      ("1 != 1", false),
      ("1 == 2", false),
      ("1 != 2", true),
      ("true == true", true),
      ("false == false", true),
      ("true == false", false),
      ("true != false", true),
      ("false != true", true),
      ("(1 < 2) == true", true),
      ("(1 < 2) == false", false),
      ("(1 > 2) == true", false),
      ("(1 > 2) == false", true),
  ]
  for (input, expected) in tests1:
    let evaluated = testEval(input)
    TRUE(testBoolObject, evaluated, expected)

func test_bang_op() =
  let tests1 = [
      ("!true", false),
      ("!false", true),
      ("!5", false),
      ("!!true", true),
      ("!!false", false),
      ("!!5", true),
  ]
  for (input, expected) in tests1:
    {.cast(noSideEffect).}:
      let evaluated = testEval(input)
      TRUE(testBoolObject, evaluated, expected)

proc test_ifelse_exp() =
  let tests1 = [
      ("if (true) { 10 }", 10),
      ("if (1) { 10 }", 10),
      ("if (1 < 2) { 10 }", 10),
      ("if (1 > 2) { 10 } else { 20 }", 20),
      ("if (1 < 2) { 10 } else { 20 }", 10),
  ]
  for (input, expected) in tests1:
    let evaluated = testEval(input)
    TRUE(testIntegerObject, evaluated, expected)
  let tests2 = [
      ("if (false) { 10 }", okNull),
      ("if (1 > 2) { 10 }", okNull),
  ]
  for (input, expected) in tests2:
    let evaluated = testEval(input)
    EQ(testNullObject(evaluated.kind), true)

proc test_return_statements() =
  let tests1 = [
      ("return 10;", 10),
      ("return 10; 9;", 10),
      ("return 2 * 5; 9;", 10),
      ("9; return 2 * 5; 9;", 10),
      ("""
if (10 > 1) {
  if (10 > 1) {
    return 10;
  }
  return 1;
}
""", 10),
  ]
  for (input, expected) in tests1:
    let evaluated = testEval(input)
    TRUE(testIntegerObject, evaluated, expected)

proc test_error_handling() =
  let tests1 = [
      ("5 + true;",
       "type mismatch: INTEGER + BOOL",),
      ("5 + true; 5;",
       "type mismatch: INTEGER + BOOL",),
      ("-true",
       "unknown operator: -BOOL",),
      ("true + false;",
       "unknown operator: BOOL + BOOL",),
      ("5; true + false; 5",
       "unknown operator: BOOL + BOOL",),
      ("if (10 > 1) { true + false; }",
       "unknown operator: BOOL + BOOL",),
       ("""
 if (10 > 1) {
   if (10 > 1) {
     return true + false;
   }

   return 1;
 }
 """, "unknown operator: BOOL + BOOL",),
      ("foobar",
       "identifier not found: foobar", ),
      ("\"Hello\" - \"World!\"",
       "unknown operator: STRING - STRING", ),
  ]
  for (input, expected) in tests1:
    let evaluated = testEval(input)
    EQ(evaluated.kind, okError)
    EQ(evaluated.errorVal, expected)

proc test_let_stmt() =
  let tests1 = [
      ("let a = 5; a;", 5),
      ("let a = 5 * 5; a;", 25),
      ("let a = 5; let b = a; b;", 5),
      ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
  ]
  for (input, expected) in tests1:
    TRUE(testIntegerObject, testEval(input), expected)

proc test_function_object() =
  let input = "fn(x) { x + 2; };"
  let evaluated = testEval(input)
  # echo evaluated.repr
  EQ(evaluated.kind, okFunction)
  EQ(evaluated.fnParameters.len, 1)
  EQ(evaluated.fnParameters[0].identVal, "x")
  let expectedBody = "(x + 2)"
  EQ(evaluated.fnBody.astToString(), expectedBody)

proc test_function_application() =
  let tests1 = [
      ("let identity = fn(x) { x; }; identity(5);", 5),
      ("let identity = fn(x) { return x; }; identity(5);", 5),
      ("let double = fn(x) { x * 2; }; double(5);", 10),
      ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
      ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
      ("fn(x) { x; }(5)", 5),
  ]
  for (input, expected) in tests1:
    TRUE(testIntegerObject, testEval(input), expected)

proc test_closures() =
  let input = """
let newAdder = fn(x) {
  fn(y) { x + y };
};

let addTwo = newAdder(2);
addTwo(2);
"""
  TRUE(testIntegerObject, testEval(input), 4)

proc test_stringlit() =
  let input = "\"Hello World!\""
  let evaluated = testEval(input)
  EQ(evaluated.kind, okString)
  EQ(evaluated.stringVal, "Hello World!")

proc test_string_concatenation() =
  let input = "\"Hello\" + \" \" + \"World!\""
  let evaluated = testEval(input)
  EQ(evaluated.kind, okString)
  EQ(evaluated.stringVal, "Hello World!")

proc test_builtin_functions() =
  let tests1 = [
      ("len(\"\")", 0),
      ("len(\"four\")", 4),
      ("len(\"hello world\")", 11),
  ]
  for (input, expected) in tests1:
    let evaluated = testEval(input)
    TRUE(testIntegerObject, evaluated, expected)

  let tests2 = [
      ("len(1)", "argument to `len` not supported, got INTEGER"),
      ("len(\"one\", \"two\")", "wrong number of arguments. got=2, want=1"),
  ]
  for (input, expected) in tests2:
    let evaluated = testEval(input)
    EQ(evaluated.kind, okError)
    EQ(evaluated.errorVal, expected)

test_eval_integer_exp()
test_eval_bool_exp()
test_bang_op()
test_ifelse_exp()
test_return_statements()
test_error_handling()
test_let_stmt()
test_function_object()
test_function_application()
test_closures()
test_stringlit()
test_string_concatenation()
test_builtin_functions()
