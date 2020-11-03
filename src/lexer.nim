import token

type
  Lexer* = object
    input: string
    position: int     # current position in input (points to current char)
    readPosition: int # current reading position in input (after current char)
    ch: char          # current char under examination

proc initLexer*(input: string): Lexer
proc nextToken*(l: var Lexer): Token
proc eatChar(l: var Lexer)
proc isLetter(ch: char): bool
proc readIdentifier(l: var Lexer): string
proc skipWhitespace(l: var Lexer)
func isDigit(ch: char): bool
proc readNumber(l: var Lexer): string
proc peekChar(l: Lexer): char
proc readString(l: var Lexer): string

proc initLexer(input: string): Lexer =
  result.input = input
  result.eatChar()

proc eatChar(l: var Lexer) =
  if l.readPosition >= l.input.len:
    l.ch = '\0'
  else:
    l.ch = l.input[l.readPosition]
  l.position = l.readPosition
  l.readPosition += 1

proc nextToken*(l: var Lexer): Token =
  l.skipWhitespace()
  case l.ch:
  of '=':
    if l.peekChar == '=':
      l.eatChar()
      result.tokType = tkEq
      result.literal = $tkEq
    else:
      result.tokType = tkAssign
      result.literal = $tkAssign
  of '+':
    result.tokType = tkPlus
    result.literal = $tkPlus
  of '-':
    result.tokType = tkMinus
    result.literal = $tkMinus
  of '!':
    if l.peekChar == '=':
      l.eatChar()
      result.tokType = tkNotEq
      result.literal = $tkNotEq
    else:
      result.tokType = tkBang
      result.literal = $tkBang
  of '*':
    result.tokType = tkAsterisk
    result.literal = $tkAsterisk
  of '/':
    result.tokType = tkSlash
    result.literal = $tkSlash
  of '<':
    result.tokType = tkLt
    result.literal = $tkLt
  of '>':
    result.tokType = tkGt
    result.literal = $tkGt
  of ';':
    result.tokType = tkSemiColon
    result.literal = $tkSemiColon
  of '(':
    result.tokType = tkLparen
    result.literal = $tkLparen
  of ')':
    result.tokType = tkRparen
    result.literal = $tkRparen
  of ',':
    result.tokType = tkComma
    result.literal = $tkComma
  of '{':
    result.tokType = tkLbrace
    result.literal = $tkLbrace
  of '}':
    result.tokType = tkRbrace
    result.literal = $tkRbrace
  of '\0':
    result.tokType = tkEof
    result.literal = $tkEof
  of '"':
    result.tokType = tkString
    result.literal = l.readString()
  else:
    if isLetter(l.ch):
      let ident = l.readIdentifier()
      result.tokType = LookupIdent(ident)
      result.literal = ident
      return result
    elif isDigit(l.ch):
      let number = l.readNumber()
      result.tokType = tkIntLit
      result.literal = number
      return result
    else:
      result.tokType = tkIllegal
      result.literal = $l.ch
  l.eatChar()

proc readString(l: var Lexer): string =
  let position = l.position + 1
  while true:
    l.eatChar()
    if l.ch == '"':
      break
  result = l.input[position ..< l.position]

proc readIdentifier(l: var Lexer): string =
  let position = l.position
  while isLetter(l.ch):
    l.eatChar()
  result = l.input[position ..< l.position]

proc readNumber(l: var Lexer): string =
  let position = l.position
  while isDigit(l.ch):
    l.eatChar()
  result = l.input[position ..< l.position]

proc isLetter(ch: char): bool =
  'a' <= ch and ch <= 'z' or
  'A' <= ch and ch <= 'Z' or
  '_' == ch

func isDigit(ch: char): bool =
  '0' <= ch and ch <= '9'

proc skipWhitespace(l: var Lexer) =
  while l.ch == ' ' or l.ch == '\t' or l.ch == '\n' or l.ch == '\r':
    l.eatChar()

proc peekChar(l: Lexer): char =
  if l.readPosition >= l.input.len:
    return '\0'
  else:
    return l.input[l.readPosition]
