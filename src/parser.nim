import ./ast, ./lexer, ./token
import strutils, tables, strformat

when defined(trace): import ./parser_tracing

var precedences = {
    tkEq:       Equals,
    tkNotEq:    Equals,
    tkLt:       LessGreater,
    tkGt:       LessGreater,
    tkPlus:     Sum,
    tkMinus:    Sum,
    tkSlash:    Product,
    tkAsterisk: Product,
    tkLparen:   CALL,
}.toTable


type
  prefixParseFn = proc(p: var Parser): PNode
  infixParseFn =  proc(p: var Parser, exp: PNode): PNode
  Parser* = object
    lexer*: Lexer
    curToken*:  Token
    peekToken*: Token
    errors*: seq[string]
    prefixParseFns: Table[TokType, prefixParseFn]
    infixParseFns:  Table[TokType, infixParseFn]


proc initParser*(L: Lexer): Parser
proc parseProgram*(p: var Parser): PNode
proc parseStatement(p: var Parser): PNode
proc parseExpressionStatement(p: var Parser): PNode
proc parseReturnStatement(p: var Parser): PNode
proc parseLetStatement(p: var Parser): PNode
proc parseExpression(p: var Parser, precedence: Precedence): Pnode
proc parseIdent(p: var Parser): PNode
proc parseIntLit(p: var Parser): PNode
proc parseBool(p: var Parser): PNode
proc parseFunctionLit(p: var Parser): PNode
proc parseFunctionParameters(p: var Parser): seq[PNode]
proc parsePrefixExpression(p: var Parser): PNode
proc parseInfixExpression(p: var Parser, left: PNode): PNode
proc parseGroupedExpression(p: var Parser): PNode
proc parseBlockStatement(p: var Parser): PNode
proc parseIfExpression(p: var Parser): PNode
proc parseCallArguments(p: var Parser): seq[PNode]
proc parseCallExpression(p: var Parser, function: PNode): PNode
proc parseStringLit(p: var Parser): PNode
proc nextToken*(p: var Parser)
proc expectPeek(p: var Parser, t: TokType): bool
proc curTokenIs(p: Parser, t: TokType): bool
proc peekTokenIs(p: Parser, t: TokType): bool
proc peekError(p: var Parser, t: TokType)
proc registerPrefix(p: var Parser, tokType: TokType, fn: prefixParseFn) {.inline.}
proc registerInfix(p: var Parser, tokType: TokType, fn: infixParseFn)
proc noPrefixParseFnError(p: var Parser, t: TokType)
proc curPrecedence(p: Parser): Precedence
proc peekPrecedence(p: Parser): Precedence


proc initParser(L: Lexer): Parser =
  result.registerPrefix(tkIdent, parseIdent)
  result.registerPrefix(tkIntLit, parseIntLit)
  result.registerPrefix(tkBang, parsePrefixExpression)
  result.registerPrefix(tkMinus, parsePrefixExpression)
  result.registerPrefix(tkTrue, parseBool)
  result.registerPrefix(tkFalse, parseBool)
  result.registerPrefix(tkLparen, parseGroupedExpression)
  result.registerPrefix(tkIf, parseIfExpression)
  result.registerPrefix(tkElse, parseIfExpression)
  result.registerPrefix(tkFunction, parseFunctionLit)
  result.registerPrefix(tkString, parseStringLit)

  result.registerInfix(tkPlus, parseInfixExpression)
  result.registerInfix(tkMinus, parseInfixExpression)
  result.registerInfix(tkAsterisk, parseInfixExpression)
  result.registerInfix(tkSlash, parseInfixExpression)
  result.registerInfix(tkGt, parseInfixExpression)
  result.registerInfix(tkLt, parseInfixExpression)
  result.registerInfix(tkEq, parseInfixExpression)
  result.registerInfix(tkNotEq, parseInfixExpression)
  result.registerInfix(tkLparen, parseCallExpression)

  result.lexer = L
  result.nextToken()
  result.nextToken()

proc parseStringLit(p: var Parser): PNode =
  new(result)
  result.kind = nkStringLit
  result.tok = p.curToken
  result.stringVal = p.curToken.literal

proc parseCallExpression(p: var Parser, function: PNode): PNode =
  when defined(trace): TRACE("parseCallExpression")
  new(result)
  result.kind = nkCallExpression
  result.tok = p.curToken
  result.callExpFunction = function

  result.callExpArguments = p.parseCallArguments()
  return result

proc parseCallArguments(p: var Parser): seq[PNode] =
  when defined(trace): TRACE("parseCallArguments")
  if p.peekTokenIs(tkRparen):
      p.nextToken()
      return result

  p.nextToken()
  result.add(p.parseExpression(Lowest))

  while p.peekTokenIs(tkComma):
    p.nextToken()
    p.nextToken()
    result.add(p.parseExpression(Lowest))

  if not p.expectPeek(tkRparen): return @[]

  return result

proc parseFunctionParameters(p: var Parser): seq[PNode] =
  when defined(trace): TRACE("parseFunctionParameters")
  if p.peekTokenIs(tkRparen):
    p.nextToken()
    return result

  p.nextToken()

  var n: PNode
  new(n)
  n.kind = nkIdent
  n.tok = p.curToken
  n.identVal = p.curToken.literal
  result.add(n)

  while p.peekTokenIs(tkComma):
    p.nextToken()
    p.nextToken()

    n.kind = nkIdent
    n.tok = p.curToken
    n.identVal = p.curToken.literal
    result.add(n)

  if not p.expectPeek(tkRparen): return @[]

  return result

proc parseFunctionLit(p: var Parser): PNode =
  when defined(trace): TRACE("parseFunctionLit")
  new(result)
  result.kind = nkFunctionLit
  result.tok = p.curToken

  if not p.expectPeek(tkLparen): return nil

  result.fnParameters = p.parseFunctionParameters()

  if not p.expectPeek(tkLbrace): return nil

  result.fnBody = p.parseBlockStatement()

  return result

proc parseProgram(p: var Parser): PNode =
  when defined(trace): TRACE("parseProgram")
  result = PNode(kind: nkProgram)
  result.statements = newSeq[PNode]()

  while p.curToken.tokType != tkEof:
    let statement = p.parseStatement()
    if statement != nil:
      result.statements.add(statement)
    p.nextToken()

  if p.errors.len != 0:
    return nil
  return result

proc parseStatement(p: var Parser): PNode =
  when defined(trace): TRACE("parseStatement")
  case p.curToken.tokType
  of tkLet:
    return p.parseLetStatement()
  of tkReturn:
    return p.parseReturnStatement()
  else:
    return p.parseExpressionStatement()

proc parseExpressionStatement(p: var Parser): PNode =
  when defined(trace): TRACE("parseExpressionStatement")
  new(result)
  result.kind = nkExpressionStatement
  result.tok = p.curToken

  result.expression = p.parseExpression(Lowest)

  if p.peekTokenIs(tkSemiColon):
    p.nextToken()

proc parseReturnStatement(p: var Parser): PNode =
  when defined(trace): TRACE("parseReturnStatement")
  new(result)
  result.kind = nkReturnStatement
  result.tok = p.curToken

  p.nextToken()
  result.returnVal = p.parseExpression(Lowest)

  if p.peekTokenIs(tkSemiColon): p.nextToken()

  return result

proc parseLetStatement(p: var Parser): PNode =
  when defined(trace): TRACE("parseLetStatement")
  new(result)
  result.kind = nkLetStatement
  result.tok = p.curToken

  if not p.expectPeek(tkIdent): return nil

  var n: PNode
  new(n)
  n.kind = nkIdent
  n.tok = p.curToken
  n.identVal = p.curToken.literal
  result.letIdent = n

  if not p.expectPeek(tkAssign): return nil

  p.nextToken()
  result.letVal = p.parseExpression(Lowest)

  if p.peekTokenIs(tkSemiColon): p.nextToken()

  return result

proc parseIdent(p: var Parser): PNode =
  when defined(trace): TRACE("parseIdent")
  new(result)
  result.kind = nkIdent
  result.tok = p.curToken
  result.identVal = p.curToken.literal

proc parseBool(p: var Parser): PNode =
  when defined(trace): TRACE("parseBool")
  new(result)
  result.kind = nkBoolLit
  result.tok = p.curToken
  result.boolVal = p.curTokenIs(tkTrue)

proc parseIntLit(p: var Parser): PNode =
  when defined(trace): TRACE("parseIntLit")
  var inum: int
  try:
    inum = parseInt(p.curToken.literal)
  except ValueError:
    p.errors.add("invalid integer: " & p.curToken.literal)
  new(result)
  result.kind = nkIntLit
  result.tok = p.curToken
  result.intVal = inum

proc parsePrefixExpression(p: var Parser): PNode =
  when defined(trace): TRACE("parsePrefixExpression")
  new(result)
  result.kind = nkPrefixExpression
  result.tok = p.curToken
  result.preExpOpVal = p.curToken.literal
  p.nextToken()
  result.preExpRight = p.parseExpression(Prefix)

proc parseInfixExpression(p: var Parser, left: PNode): PNode =
  when defined(trace): TRACE("parseInfixExpression")
  new(result)
  result.kind = nkInfixExpression
  result.tok = p.curToken
  result.inExpOpVal = p.curToken.literal

  let precedence = p.curPrecedence()
  p.nextToken()
  result.inExpRight = p.parseExpression(precedence)
  return result

proc parseGroupedExpression(p: var Parser): PNode =
  when defined(trace): TRACE("parseGroupedExpression")
  p.nextToken()
  result = p.parseExpression(LOWEST)
  if not p.expectPeek(tkRparen): return nil
  return result

proc parseIfExpression(p: var Parser): PNode =
  when defined(trace): TRACE("parseIfExpression")
  new(result)
  result.kind = nkIfExpression
  result.tok = p.curToken

  if not p.expectPeek(tkLparen): return nil

  p.nextToken()
  result.ifExpCondition = p.parseExpression(Lowest)

  if not p.expectPeek(tkRparen): return nil
  if not p.expectPeek(tkLbrace): return nil

  result.ifExpConsequence = p.parseBlockStatement()

  if p.peekTokenIs(tkElse):
    p.nextToken()
    if not p.expectPeek(tkLbrace): return nil
    result.ifExpAlternative = p.parseBlockStatement()

  return result

proc parseBlockStatement(p: var Parser): PNode =
  when defined(trace): TRACE("parseBlockStatement")
  new(result)
  result.kind = nkBlockStatement
  result.tok = p.curToken

  p.nextToken()
  while not p.curTokenIs(tkRbrace):
    let stmt = p.parseStatement()
    if stmt != nil:
      result.blockStatements.add(stmt)
    p.nextToken()
  return result

proc parseExpression(p: var Parser, precedence: Precedence): PNode =
  when defined(trace): TRACE("parseExpression")
  if not p.prefixParseFns.hasKey(p.curToken.tokType):
    p.noPrefixParseFnError(p.curToken.tokType)
    return nil
  let prefix = p.prefixParseFns[p.curToken.tokType]

  var leftExp = prefix(p)
  while not p.peekTokenIs(tkSemiColon) and precedence < p.peekPrecedence():
    if not p.infixParseFns.hasKey(p.peekToken.tokType):
      return leftExp

    let infix = p.infixParseFns[p.peekToken.tokType]
    p.nextToken()
    leftExp = infix(p, leftExp)
  return leftExp

proc nextToken(p: var Parser) =
  p.curToken = p.peekToken
  p.peekToken = p.lexer.nextToken()

proc expectPeek(p: var Parser, t: TokType): bool =
  if p.peekTokenIs(t):
    p.nextToken()
    return true
  p.peekError(t)
  return false

proc curTokenIs(p: Parser, t: TokType): bool =
  return p.curToken.tokType == t

proc peekTokenIs(p: Parser, t: TokType): bool =
  return p.peekToken.tokType == t

proc peekError(p: var Parser, t: TokType) =
  p.errors.add(fmt"expected next token to be `{t}`, got `{p.peekToken.tokType}` instead")

proc registerPrefix(p: var Parser, tokType: TokType, fn: prefixParseFn) {.inline.} =
  p.prefixParseFns[tokType] = fn

proc registerInfix(p: var Parser, tokType: TokType, fn: infixParseFn) =
  p.infixParseFns[tokType] = fn

proc noPrefixParseFnError(p: var Parser, t: TokType) =
  p.errors.add(fmt"no prefix parse function for `{t}` found")


proc peekPrecedence(p: Parser): Precedence =
  if precedences.hasKey(p.peekToken.tokType):
    return precedences[p.peekToken.tokType]
  return Lowest

proc curPrecedence(p: Parser): Precedence =
  if precedences.hasKey(p.curToken.tokType):
    return precedences[p.curToken.tokType]
  return Lowest
