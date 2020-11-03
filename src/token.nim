type
  TokType* = enum
    tkIllegal
    tkIdent # add, foobar, x, y, ...
    tkIntLit  # 1343456
    tkString
    tkEof = "\0"

      # Operators
    tkAssign = "="
    tkPlus = "+"
    tkMinus = "-"
    tkBang = "!"
    tkAsterisk = "*"
    tkSlash = "/"

    tkLt = "<"
    tkGt = ">"
    tkEq = "=="
    tkNotEq = "!="

    # Delimiters
    tkComma = ","
    tkSemiColon = ";"

    tkLparen = "("
    tkRparen = ")"
    tkLbrace = "{"
    tkRbrace = "}"

    # Keywords
    tkFunction = "fn"
    tkLet = "let"
    tkTrue = "true"
    tkFalse = "false"
    tkIf = "if"
    tkElse = "else"
    tkReturn = "return"

  Token* = object
    tokType*: TokType
    literal*: string

proc LookupIdent*(ident: string): TokType =
  result = case ident
  of $tkFunction: tkFunction
  of $tkLet: tkLet
  of $tkTrue: tkTrue
  of $tkFalse: tkFalse
  of $tkIf: tkIf
  of $tkElse: tkElse
  of $tkReturn: tkReturn
  else: tkIdent
