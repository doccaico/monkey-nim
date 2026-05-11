import std/[tables, strutils]

import ./[ast]


type
  ObjectKind* = enum
    okInteger,
    okBool,
    okNull,
    okReturnVal,
    okError,
    okFunction,
    okString,
    okBuiltin,

type
  BuiltinFunction = proc(args: varargs[PObject]): PObject

  PObject* = ref object
    case kind*: ObjectKind
    of okInteger:
      intVal*: int
    of okBool:
      boolVal*: bool
    of okReturnVal:
      returnVal*: PObject
    of okError:
      errorVal*: string
    of okFunction:
      fnParameters*: seq[PNode]
      fnBody*: PNode
      fnEnv*: PEnvironment
    of okString:
      stringVal*: string
    of okBuiltin:
      builtinFn*: BuiltinFunction
    of okNull:
      discard

  PEnvironment* = ref object
    store: TableRef[string, PObject]
    outer: PEnvironment


proc inspectValue*(o: PObject): string =
  case o.kind:
  of okInteger:
    result = $o.intVal
  of okBool:
    result = $o.boolVal
  of okReturnVal:
    result = $o.returnVal.inspectValue()
  of okError:
    result = "ERROR: " & o.errorVal
  of okFunction:
    var params: seq[string]
    for p in o.fnParameters:
      params.add(p.astToString())
    result.add("fn")
    result.add("(")
    result.add(params.join(", "))
    result.add(") {\n")
    result.add(o.fnBody.astToString())
    result.add("\n}")
  of okString:
    result = o.stringVal
  of okBuiltin:
    result = "builtin function"
  of okNull:
    result = "null"

proc inspectType*(o: PObject): string =
  case o.kind:
  of okInteger:
    result = "INTEGER"
  of okBool:
    result = "BOOL"
  of okReturnVal:
    result = "RETURN_VALUE"
  of okError:
    result = "ERROR"
  of okFunction:
    result = "FUNCTION"
  of okString:
    result = "STRING"
  of okBuiltin:
    result = "BUILTIN"
  of okNull:
    result = "NULL"

proc newEnvironment*(): PEnvironment =
  result = PEnvironment(store: newTable[string, PObject](), outer: nil)

proc newEnclosedEnvironment*(outer: PEnvironment): PEnvironment =
  result = newEnvironment()
  result.outer = outer

proc getVal*(e: PEnvironment, name: string): (PObject, bool) =
  if e.store.hasKey(name):
    return (e.store[name], true)
  else:
    if e.outer != nil:
      return e.outer.getVal(name)
  result = (PObject(kind: okNull), false)

proc setVal*(e: PEnvironment, name: string, val: PObject): PObject {.discardable.} =
  e.store[name] = val
  result = val
