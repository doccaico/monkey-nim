import ./ast

import tables, strutils

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

  BuiltinFunction = proc(args: varargs[Object]): Object

  Object* = ref object
    case kind*: ObjectKind
    of okInteger:
      intVal*: int
    of okBool:
      boolVal*: bool
    of okReturnVal:
      returnVal*: Object
    of okError:
      errorVal*: string
    of okFunction:
      fnParameters*: seq[PNode]
      fnBody*: PNode
      fnEnv*: Environment
    of okString:
      stringVal*: string
    of okBuiltin:
      builtinFn*: BuiltinFunction
    of okNull:
      discard

  Environment* = ref object
    store: TableRef[string, Object]
    outer: Environment


proc inspectValue*(o: Object): string =
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

proc inspectType*(o: Object): string =
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

proc newEnvironment*(): Environment =
  return Environment(store: newTable[string, Object](), outer: nil)

proc newEnclosedEnvironment*(outer: Environment): Environment =
  var env = newEnvironment()
  env.outer = outer
  return env

proc getVal*(e: Environment, name: string): (Object, bool) =
  if e.store.hasKey(name):
    return (e.store[name], true)
  else:
    if e.outer != nil:
      return e.outer.getVal(name)
  return (Object(kind: okNull), false)

proc setVal*(e: Environment, name: string, val: Object): Object {.discardable.} =
  e.store[name] = val
  return val
