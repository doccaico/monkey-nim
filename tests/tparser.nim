import strformat, macros
import ../src/[ast, lexer, parser]
from ./utils import EQ, TRUE, errInfo

# helper

proc checkParserErrors(p: Parser) =
  if p.errors.len == 0: return
  echo fmt"parser has {p.errors.len} errors"
  for msg in p.errors:
    echo fmt"parser error: {msg}"
  quit(QuitFailure)

proc initProgram(input: string): PNode =
  var l = initLexer(input)
  var p = initParser(l)
  var program = p.parseProgram()
  checkParserErrors(p)
  return program

proc testIntLit(stmt: PNode, val: int): bool =
  if stmt.kind != nkIntLit:
    errinfo.got = $stmt.kind
    errinfo.expected = $nkIntLit
    return false
  if stmt.intVal != val:
    errinfo.got = $stmt.intVal
    errinfo.expected = $val
    return false
  if stmt.tokenLiteral() != $val:
    errinfo.got = $stmt.tokenLiteral()
    errinfo.expected = $val
    return false
  return true

proc testIdent(stmt: PNode, val: string): bool =
  if stmt.kind != nkIdent:
    errinfo.got = $stmt.kind
    errinfo.expected = $nkIntLit
    return false
  if stmt.identVal != val:
    errinfo.got = $stmt.identVal
    errinfo.expected = val
    return false
  if stmt.tokenLiteral() != val:
    errinfo.got = $stmt.tokenLiteral()
    errinfo.expected = val
    return false
  return true

proc testBoolLit(stmt: PNode, val: bool): bool =
  if stmt.kind != nkBoolLit:
    errinfo.got = $stmt.kind
    errinfo.expected = $nkBoolLit
    return false
  if stmt.boolVal != val:
    errinfo.got = $stmt.boolVal
    errinfo.expected = $val
    return false
  if stmt.tokenLiteral() != $val:
    errinfo.got = $stmt.tokenLiteral()
    errinfo.expected = $val
    return false
  return true

proc testLitExp(stmt: PNode, expected: int): bool =
  return testIntLit(stmt, expected)

proc testLitExp(stmt: PNode, expected: string): bool =
  return testIdent(stmt, expected)

proc testLitExp(stmt: PNode, expected: bool): bool =
  return testBoolLit(stmt, expected)

proc testLetStmt(stmt: PNode, name: string): bool =
  if stmt.tokenLiteral() != "let":
    errinfo.got = stmt.tokenLiteral()
    errinfo.expected = "let"
    return false
  if stmt.kind != nkLetStatement:
    errinfo.got = $stmt.expression.kind
    errinfo.expected = $nkLetStatement
    return false
  if stmt.letIdent.identVal != name:
    errinfo.got = stmt.letIdent.identVal
    errinfo.expected = name
    return false
  if stmt.letIdent.tokenLiteral() != name:
    errinfo.got = stmt.letIdent.tokenLiteral()
    errinfo.expected = name
    return false
  return true

proc testInfixExp(
    stmt: PNode,
    left: int|string|bool,
    op: string,
    right: int|string|bool): bool =
  if stmt.kind != nkInfixExpression:
    errinfo.got = $stmt.kind
    errinfo.expected = $nkInfixExpression
    return false
  if not testLitExp(stmt.inExpLeft, left):
    return false
  if stmt.inExpOpVal != op:
    errinfo.got = $stmt.inExpOpVal
    errinfo.expected = op
    return false
  if not testLitExp(stmt.inExpRight, right):
    return false
  true

# tests

proc test_return_stmt() =
  let input = """
return 5;
return 10;
return 993322;
"""
  let expects = [5, 10, 993322]
  let program = initProgram(input)
  EQ(program.statements.len, 3)
  for i, expected in expects:
    let stmt = program.statements[i]
    EQ(stmt.tokenLiteral(), "return")
    EQ(stmt.returnVal.intVal, expected)

proc test_error() =
  let input = """
let x = 5
let   = 10;
let foobar = 838383;
"""
  let expected = 2
  var L = initLexer(input)
  var P = initParser(L)
  discard P.parseProgram()
  if P.errors.len != expected:
    quit(fmt"expected=`{expected}`. got=`{P.errors.len}`")

proc test_let_stmt() =
  let tests1 = [
      "let x = 5;",
      "let y = true;",
      "let foobar = y",
      ]
  let expectedIdent = ["x", "y", "foobar"]
  let expectedVal = (5, true, "y")
  for i, input in tests1:
    let program = initProgram(input)
    EQ(program.statements.len, 1)
    let stmt = program.statements[0]
    TRUE(testLetStmt, stmt, expectedIdent[i])
    case i
    of 0: TRUE(testLitExp, stmt.letVal, expectedVal[0])
    of 1: TRUE(testLitExp, stmt.letVal, expectedVal[1])
    of 2: TRUE(testLitExp, stmt.letVal, expectedVal[2])

proc test_ident_exp() =
  let input = "foobar;"
  let program = initProgram(input)
  EQ(program.statements.len, 1)
  let stmt = program.statements[0]
  EQ(stmt.kind, nkExpressionStatement)
  EQ(stmt.expression.kind, nkIdent)
  EQ(stmt.expression.identVal, "foobar")
  EQ(stmt.expression.tok.literal, "foobar")

proc test_intlit_exp() =
  let input = "5;"
  let program = initProgram(input)
  EQ(program.statements.len, 1)
  let stmt = program.statements[0]
  EQ(stmt.kind, nkExpressionStatement)
  EQ(stmt.expression.kind, nkIntLit)
  EQ(stmt.expression.intVal, 5)
  EQ(stmt.expression.tok.literal, "5")

proc test_parsing_prefix_exp() =
  let tests1 = [
      ("!5;", "!", 5),
      ("-15;", "-", 15),
      ]
  for (input, expectedOp, expectedVal) in tests1:
    let program = initProgram(input)
    EQ(program.statements.len, 1)
    let stmt = program.statements[0]
    EQ(stmt.kind, nkExpressionStatement)
    EQ(stmt.expression.kind, nkPrefixExpression)
    EQ(stmt.expression.preExpOpVal, expectedOp)
    TRUE(testIntLit, stmt.expression.preExpRight, expectedVal)

  let tests2 = [
      ("!true;", "!", true),
      ("!false;", "!", false),
      ]
  for (input, expectedOp, expectedVal) in tests2:
    let program = initProgram(input)
    EQ(program.statements.len, 1)
    let stmt = program.statements[0]
    EQ(stmt.kind, nkExpressionStatement)
    EQ(stmt.expression.kind, nkPrefixExpression)
    EQ(stmt.expression.preExpOpVal, expectedOp)
    TRUE(testBoolLit, stmt.expression.preExpRight, expectedVal)

proc test_parsing_infix_exp() =
  let tests1 = [
      ("5 + 5;", 5, "+", 5),
      ("5 - 5;", 5, "-", 5),
      ("5 * 5;", 5, "*", 5),
      ("5 / 5;", 5, "/", 5),
      ("5 > 5;", 5, ">", 5),
      ("5 < 5;", 5, "<", 5),
      ("5 == 5;", 5, "==", 5),
      ("5 != 5;", 5, "!=", 5),
      ]
  for (input, expectedLeftVal, expectedOp, expectedRightVal) in tests1:
    let program = initProgram(input)
    EQ(program.statements.len, 1)
    let stmt = program.statements[0]
    EQ(stmt.kind, nkExpressionStatement)
    TRUE(testInfixExp, stmt.expression, expectedLeftVal, expectedOp, expectedRightVal)

  let tests2 = [
      ("true == true", true, "==", true),
      ("true != false", true, "!=", false),
      ("false == false", false, "==", false),
      ]
  for (input, expectedLeftVal, expectedOp, expectedRightVal) in tests2:
    let program = initProgram(input)
    EQ(program.statements.len, 1)
    let stmt = program.statements[0]
    EQ(stmt.kind, nkExpressionStatement)
    TRUE(testInfixExp, stmt.expression, expectedLeftVal, expectedOp, expectedRightVal)

proc test_operator_precedence_parsing() =
  let tests1 = [
      ("-a * b",
       "((-a) * b)", ),
      ("!-a",
       "(!(-a))", ),
      ("a + b + c",
       "((a + b) + c)", ),
      ("a + b - c",
       "((a + b) - c)", ),
      ("a * b * c",
       "((a * b) * c)", ),
      ("a * b / c",
       "((a * b) / c)", ),
      ("a + b / c",
       "(a + (b / c))", ),
      ("a + b * c + d / e - f",
       "(((a + (b * c)) + (d / e)) - f)", ),
      ("3 + 4; -5 * 5",
       "(3 + 4)((-5) * 5)", ),
      ("5 > 4 == 3 < 4",
       "((5 > 4) == (3 < 4))", ),
      ("5 < 4 != 3 > 4",
       "((5 < 4) != (3 > 4))", ),
      ("3 + 4 * 5 == 3 * 1 + 4 * 5",
       "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))", ),
      ("3 + 4 * 5 == 3 * 1 + 4 * 5",
       "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))", ),
      ("true",
       "true", ),
      ("false",
       "false", ),
      ("3 > 5 == false",
       "((3 > 5) == false)", ),
      ("3 < 5 == true",
       "((3 < 5) == true)", ),
      # Grouping
      ("1 + (2 + 3) + 4",
       "((1 + (2 + 3)) + 4)", ),
      ("(5 + 5) * 2",
       "((5 + 5) * 2)", ),
      ("2 / (5 + 5)",
       "(2 / (5 + 5))", ),
      ("-(5 + 5)",
       "(-(5 + 5))", ),
      ("!(true == true)",
       "(!(true == true))", ),
      # Function
      ("a + add(b * c) + d",
       "((a + add((b * c))) + d)", ),
      ("add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
       "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))", ),
      ("add(a + b + c * d / f + g)",
       "add((((a + b) + ((c * d) / f)) + g))", ),
      ]
  for (input, expected) in tests1:
    let program = initProgram(input)
    let actual = program.astToString()
    EQ(actual, expected)

proc test_bool_exp() =
  let input = "true;"
  let program = initProgram(input)
  EQ(program.statements.len, 1)
  let stmt = program.statements[0]
  EQ(stmt.kind, nkExpressionStatement)
  EQ(stmt.expression.kind, nkBoolLit)
  EQ(stmt.expression.boolVal, true)
  EQ(stmt.expression.tok.literal, "true")

proc test_if_exp() =
  let input = "if (x < y) { x }"
  let program = initProgram(input)
  EQ(program.statements.len, 1)
  let stmt = program.statements[0]
  EQ(stmt.kind, nkExpressionStatement)
  EQ(stmt.expression.kind, nkIfExpression)
  TRUE(testInfixExp, stmt.expression.ifExpCondition, "x", "<", "y")
  EQ(stmt.expression.ifExpConsequence.blockStatements.len, 1)
  EQ(stmt.expression.ifExpConsequence.blockStatements[0].kind, nkExpressionStatement)
  TRUE(testIdent, stmt.expression.ifExpConsequence.blockStatements[0].expression, "x")
  if stmt.expression.ifExpAlternative != nil:
    quit("stmt.expression.ifExpAlternative is not `nil` (expected `nil`)")

proc test_ifelse_exp() =
  let input = "if (x < y) { x } else { y }"
  let program = initProgram(input)
  EQ(program.statements.len, 1)
  let stmt = program.statements[0]
  EQ(stmt.kind, nkExpressionStatement)
  EQ(stmt.expression.kind, nkIfExpression)
  TRUE(testInfixExp, stmt.expression.ifExpCondition, "x", "<", "y")
  EQ(stmt.expression.ifExpConsequence.blockStatements.len, 1)
  EQ(stmt.expression.ifExpConsequence.blockStatements[0].kind, nkExpressionStatement)
  TRUE(testIdent, stmt.expression.ifExpConsequence.blockStatements[0].expression, "x")
  # else
  EQ(stmt.expression.ifExpAlternative.blockStatements.len, 1)
  EQ(stmt.expression.ifExpAlternative.blockStatements[0].kind, nkExpressionStatement)
  TRUE(testIdent, stmt.expression.ifExpAlternative.blockStatements[0].expression, "y")

proc test_functionlit_parsing() =
  let input = "fn(x, y) { x + y; }"
  let program = initProgram(input)
  EQ(program.statements.len, 1)
  let stmt = program.statements[0]
  EQ(stmt.kind, nkExpressionStatement)
  EQ(stmt.expression.kind, nkFunctionLit)
  EQ(stmt.expression.fnParameters.len, 2)
  TRUE(testLitExp, stmt.expression.fnParameters[0], "x")
  TRUE(testLitExp, stmt.expression.fnParameters[1], "y")
  EQ(stmt.expression.fnBody.blockStatements.len, 1)
  EQ(stmt.expression.fnBody.blockStatements[0].kind, nkExpressionStatement)
  TRUE(testInfixExp, stmt.expression.fnBody.blockStatements[0].expression, "x", "+", "y")

proc test_function_parameter_parsing() =
  let tests1 = [
      ("fn() {};", @[]),
      ("fn(x) {};", @["x"]),
      ("fn(x, y, z) {};", @["x", "y", "z"]),
      ]
  for (input, expected) in tests1:
    let program = initProgram(input)
    EQ(program.statements.len, 1)
    let stmt = program.statements[0]
    EQ(stmt.kind, nkExpressionStatement)
    EQ(stmt.expression.kind, nkFunctionLit)
    EQ(stmt.expression.fnParameters.len, expected.len)
    for i, ident in expected:
      TRUE(testLitExp, stmt.expression.fnParameters[i], ident)

proc test_call_exp_parsing() =
  let input = "add(1, 2 * 3, 4 + 5);"
  let program = initProgram(input)
  EQ(program.statements.len, 1)
  let stmt = program.statements[0]
  EQ(stmt.kind, nkExpressionStatement)
  EQ(stmt.expression.kind, nkCallExpression)
  TRUE(testIdent, stmt.expression.callExpFunction, "add")
  EQ(stmt.expression.callExpArguments.len, 3)
  TRUE(testLitExp, stmt.expression.callExpArguments[0], 1)
  TRUE(testInfixExp, stmt.expression.callExpArguments[1], 2, "*", 3)
  TRUE(testInfixExp, stmt.expression.callExpArguments[2], 4, "+", 5)

proc test_stringlit_exp() =
  let input = "\"hello world\";"
  let program = initProgram(input)
  let stmt = program.statements[0]
  EQ(stmt.expression.kind, nkStringLit)
  EQ(stmt.expression.stringVal, "hello world")

test_return_stmt()
test_error()
test_let_stmt()
test_ident_exp()
test_intlit_exp()
test_parsing_prefix_exp()
test_parsing_infix_exp()
test_operator_precedence_parsing()
test_bool_exp()
test_if_exp()
test_ifelse_exp()
test_functionlit_parsing()
test_function_parameter_parsing()
test_call_exp_parsing()
test_stringlit_exp()
