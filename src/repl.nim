import rdstdin
import ./lexer, ./evaluator, ./parser, ./obj

const PROMPT = ">>> "

proc start*(input: File, output: File) =
  var noteof = true
  var env = newEnvironment()
  # while not endOfFile(stdin):
  while true:
    # output.write(PROMPT)
    # var line = readLine(input)
    var userResponse: string
    noteof = readLineFromStdin(PROMPT, line = userResponse)
    if not noteof:
      break
    var L = initLexer(userResponse)
    var P = initParser(L)
    let program = P.parseProgram()

    if P.errors.len != 0 :
      for e in P.errors:
        output.writeLine(e)
      continue

    let evaluated = eval(program, env)
    if evaluated != nil:
      output.writeLine(evaluated.inspectValue())
