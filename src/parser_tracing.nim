import strutils

# Go version: https://github.com/prologic/monkey-lang/blob/master/parser/parser_tracing.go
#
# when defined(trace): import ./parser_tracing
# [...]
# proc parseProgram(p: var Parser): PNode =
#   when defined(trace): TRACE("parseProgram")
#
# $ nim r -d:trace src/parser.nim

var traceLevel: int
const traceIdentPlaceholder = "  "

proc identLevel(): string =
  return traceIdentPlaceholder.repeat(traceLevel-1)

proc tracePrint(msg: string) =
  echo identLevel() & msg

proc incIdent() = traceLevel = traceLevel + 1
proc decIdent() = traceLevel = traceLevel - 1

proc trace*(msg: string): string =
  incIdent()
  tracePrint("BEGIN " & msg)
  return msg

proc untrace*(msg: string) =
  tracePrint("END " & msg)
  decIdent()

template TRACE*(msg: string) =
  let traceMsg = trace(msg)
  defer: untrace(traceMsg)
