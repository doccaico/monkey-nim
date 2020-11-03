import ./token

import strutils


type
  Precedence* = enum
    Lowest,
    Equals,  # ==
    LessGreater,      # > or <
    Sum,     # +
    Product, # *
    Prefix,  # -X or !X
    Call     # myFunction(X)

type
  TNodeKind* = enum
    nkProgram,

    nkLetStatement,
    nkReturnStatement,
    nkExpressionStatement,

    nkIdent,
    nkIntLit,
    nkStringLit,
    nkBoolLit,
    nkfunctionLit,

    nkPrefixExpression,
    nkInfixExpression,

    nkIfExpression,
    nkCallExpression,
    nkBlockStatement,

    nkNull,

type
  PNode* = ref TNode
  # TNodeSeq = seq[PNode]
  TNode = object
    tok*: Token
    case kind*: TNodeKind
    of nkProgram:
      statements*: seq[PNode]

    of nkLetStatement:
      letIdent*: PNode
      letVal*:  PNode
    of nkReturnStatement:
      returnVal*:  PNode
    of nkExpressionStatement:
      expression*: PNode
    of nkBlockStatement:
      blockStatements*: seq[PNode]

    of nkIdent:
      identVal*: string
    of nkIntLit:
      intVal*: int
    of nkStringLit:
      stringVal*: string
    of nkBoolLit:
      boolVal*: bool
    of nkfunctionLit:
      fnParameters*: seq[PNode]
      fnBody*: PNode

    of nkPrefixExpression:
      preExpOpVal*: string
      preExpRight*: PNode
    of nkInfixExpression:
      inExpOpVal*: string
      inExpLeft*: PNode
      inExpRight*: PNode

    of nkIfExpression:
      ifExpCondition*: PNode
      ifExpConsequence*: PNode
      ifExpAlternative*: PNode
    of nkCallExpression:
      callExpFunction*: PNode
      callExpArguments*: seq[PNode]
    of nkNull: discard

proc tokenLiteral*(n: PNode): string
proc astToString*(n: PNode): string

# implementation

proc tokenLiteral(n: PNode): string = n.tok.literal

proc astToString*(n: PNode): string =
  case n.kind:
  of nkProgram:
    for statement in n.statements:
      result.add(statement.astToString())

  of nkLetStatement:
    result.add("let ")
    result.add(n.letIdent.tokenLiteral())
    result.add(" = ")
    result.add(n.letVal.astToString())
    result.add(";")
  of nkReturnStatement:
    result.add("return ")
    result.add(n.returnVal.astToString())
    result.add(";")
  of nkExpressionStatement:
    result = n.expression.astToString()

  # of nkIntLit, nkBoolLit, nkStringLit:
  #   result = n.tokenLiteral()
  of nkFunctionLit:
    var params: seq[string]
    for p in n.fnParameters:
      params.add(p.astToString())
    result.add(n.tokenLiteral())
    result.add("(")
    result.add(params.join(", "))
    result.add(")")
    result.add(n.fnBody.astToString())

  of nkPrefixExpression:
    result.add("(")
    result.add(n.preExpOpVal)
    result.add(n.preExpRight.astToString())
    result.add(")")
  of nkInfixExpression:
    result.add("(")
    result.add(n.inExpLeft.astToString())
    result.add(" " & n.inExpOpVal & " ")
    result.add(n.inExpRight.astToString())
    result.add(")")

  of nkIFExpression:
    result.add("if")
    result.add(n.ifExpCondition.astToString())
    result.add(" ")
    result.add(n.ifExpConsequence.astToString())
    if n.ifExpAlternative != nil:
      result.add("else ")
      result.add(n.ifExpAlternative.astToString())
  of nkCallExpression:
    var args: seq[string]
    for arg in n.callExpArguments:
      args.add(arg.astToString())
    result.add(n.callExpFunction.astToString())
    result.add("(")
    result.add(args.join(", "))
    result.add(")")
  of nkBlockStatement:
    for stmt in n.blockStatements:
      result.add(stmt.astToString())
  else:
    result = n.tokenLiteral()
