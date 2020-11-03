import ../src/[token, lexer]
import ./utils

let input = """let five = 5;
let ten = 10;

let add = fn(x, y) {
x + y;
};

let result = add(five, ten);

!-/*5;
5 < 10 > 5;

if (5 < 10) {
  return true;
} else {
  return false;
}

10 == 10;
10 != 9;

"foobar"
"foo bar"
"""

let tests = [
    # let five = 5;
    # let ten = 10;
    (tkLet, "let"), (tkIdent, "five"), (tkAssign, "="), (tkIntLit, "5"),
    (tkSemiColon, ";"), (tkLet, "let"), (tkIdent, "ten"), (tkAssign, "="),
    (tkIntLit, "10"), (tkSemiColon, ";"),

    # let add = fn(x, y) {
    #   x + y;
    # };
    (tkLet, "let"), (tkIdent, "add"), (tkAssign, "="), (tkFunction, "fn"),
    (tkLparen, "("), (tkIdent, "x"), (tkComma, ","), (tkIdent, "y"),
    (tkRparen, ")"), (tkLbrace, "{"), (tkIdent, "x"), (tkPlus, "+"),
    (tkIdent, "y"), (tkSemiColon, ";"), (tkRbrace, "}"), (tkSemiColon, ";"),

    # let result = add(five, ten);
    (tkLet, "let"), (tkIdent, "result"), (tkAssign, "="), (tkIdent, "add"),
    (tkLparen, "("), (tkIdent, "five"), (tkComma, ","), (tkIdent, "ten"),
    (tkRparen, ")"), (tkSemiColon, ";"),

    # !-/*5;
    # 5 < 10 > 5;
    (tkBang, "!"), (tkMinus, "-"), (tkSlash, "/"), (tkAsterisk, "*"),
    (tkIntLit, "5"), (tkSemiColon, ";"), (tkIntLit, "5"), (tkLt, "<"),
    (tkIntLit, "10"), (tkGt, ">"), (tkIntLit, "5"), (tkSemiColon, ";"),

    # if (5 < 10) {
    #     return true;
    # } else {
    #     return false;
    # }
    (tkIf, "if"), (tkLparen, "("), (tkIntLit, "5"), (tkLt, "<"),
    (tkIntLit, "10"), (tkRparen, ")"), (tkLbrace, "{"), (tkReturn, "return"),
    (tkTrue, "true"), (tkSemiColon, ";"), (tkRbrace, "}"), (tkElse, "else"),
    (tkLbrace, "{"), (tkReturn, "return"), (tkFalse, "false"), (tkSemiColon, ";"),
    (tkRbrace, "}"),

    # 10 == 10;
    # 10 != 9;
    (tkIntLit, "10"), (tkEq, "=="), (tkIntLit, "10"), (tkSemiColon, ";"),
    (tkIntLit, "10"), (tkNotEq, "!="), (tkIntLit, "9"), (tkSemiColon, ";"),

    # "foobar"
    # "foo bar"
    (tkString, "foobar"),
    (tkString, "foo bar"),

    # EOF
    (tkEof, "\0"),
    ]

var L = initLexer(input)
for (expectedType, expectedLiteral) in tests:
  let tok = L.nextToken()
  EQ(tok.tokType, expectedType)
  EQ(tok.literal, expectedLiteral)
