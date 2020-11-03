type
  ErrInfo = object
    got*: string
    expected*: string

var errinfo*: ErrInfo

template EQ*(got, expected: untyped): untyped =
  if got != expected:
    stderr.writeline("Line     = `" & $instantiationInfo().line & "`")
    stderr.writeline("Got      = `" & $got & "`")
    stderr.writeline("Expected = `" & $expected & "`")
    quit(QuitFailure)

template TRUE*(fn: untyped, args: varargs[untyped]): untyped =
  if not fn(args):
    stderr.writeline("Line     = `" & $instantiationInfo().line & "`")
    stderr.writeline("Got      = `" & errInfo.got & "`")
    stderr.writeline("Expected = `" & errInfo.expected & "`")
    quit(QuitFailure)
