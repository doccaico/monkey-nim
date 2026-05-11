import ./ast, ./obj

import strutils, tables

proc eval*(n: PNode, env: Environment): Object
proc evalBangopExpression(right: Object): Object
proc evalPrefixExpression(op: string, right: Object): Object
proc nativeBoolToBooleanObject(input: bool): Object
proc evalMinusopExpression(right: Object): Object
proc evalInfixExpression(op: string, left: Object, right: Object): Object
proc evalIntegerInfixExpression(op: string, left: Object, right: Object): Object
proc evalIfExpression(n: PNode, env: Environment): Object
proc isTruthy(o: Object): bool
proc evalBlockStatement(blk: seq[PNode], env: Environment): Object
proc evalProgram(program: PNode, env: Environment): Object
proc newError*(errMsg: string): Object
proc isError(o: Object): bool
proc evalIdent(n: PNode, env: Environment): Object
proc evalExpressions(exps: seq[PNode], env: Environment): seq[Object]
proc applyFunction(fn: Object, args: seq[Object]): Object
proc extendFunctionEnv(fn: Object, args: seq[Object]): Environment
proc unwrapReturnValue(obj: Object): Object
proc evalStringInfixExpression(op: string, left: Object, right: Object): Object

proc builtinLen(args: varargs[Object]): Object

let
  builtins* = {
    "len": Object(kind: okBuiltin, builtinFn: builtinLen),
    }.toTable
  trueObj = Object(kind: okBool, boolVal: true)
  falseObj = Object(kind: okBool, boolVal: false)
  nullObj* = Object(kind: okNull)


proc eval(n: PNode, env: Environment): Object =
  case n.kind
  of nkProgram:
    return evalProgram(n, env)
  of nkExpressionStatement:
    return eval(n.expression, env)
  of nkIntLit:
    return Object(kind: okInteger, intVal: n.intVal)
  of nkBoolLit:
    return nativeBoolToBooleanObject(n.boolVal)
  of nkPrefixExpression:
    let right = eval(n.preExpRight, env)
    if right.isError(): return right
    return evalPrefixExpression(n.preExpOpVal, right)
  of nkInfixExpression:
    let left = eval(n.inExpLeft, env)
    if left.isError(): return left
    let right = eval(n.inExpRight, env)
    if right.isError(): return right
    return evalInfixExpression(n.inExpOpVal, left, right)
  of nkBlockStatement:
    return evalBlockStatement(n.blockStatements, env)
  of nkIfExpression:
    return evalIfExpression(n, env)
  of nkReturnStatement:
    let val = eval(n.returnVal, env)
    if val.isError(): return val
    return Object(kind: okReturnVal, returnVal: val)
  of nkLetStatement:
    let val = eval(n.letVal, env)
    if val.isError(): return val
    env.setVal(n.letIdent.tokenLiteral(), val)
  of nkIdent:
    return evalIdent(n, env)
  of nkFunctionLit:
    let params = n.fnParameters
    let body = n.fnBody
    return Object(kind: okFunction, fnParameters: params, fnBody: body, fnEnv: env)
  of nkCallExpression:
    let function = eval(n.callExpFunction, env)
    if function.isError():
      return function
    let args = evalExpressions(n.callExpArguments, env)
    if args.len == 1 and args[0].isError():
      return args[0]
    return applyFunction(function, args)
  of nkStringLit:
    return Object(kind: okString, stringVal: n.stringVal)
  of nkNull:
    return nullObj

  return nil

proc evalIdent(n: PNode, env: Environment): Object =
  let (val, ok) = env.getVal(n.identVal)
  if ok:
    return val
  if builtins.hasKey(n.identVal):
    return builtins[n.identVal]
  return newError("identifier not found: " & n.identVal)

proc evalProgram(program: PNode, env: Environment): Object =
  for stmt in program.statements:
    result = eval(stmt, env)
    if result != nil:
      case result.kind
      of okReturnVal:
        return result.returnVal
      of okError:
        return result
      else:
        discard
  return result

proc evalPrefixExpression(op: string, right: Object): Object =
  case op
  of "!":
    return evalBangopExpression(right)
  of "-":
    return evalMinusopExpression(right)
  else:
    return newError("unknown operator: $1$2" % [op, right.inspectType()])

proc nativeBoolToBooleanObject(input: bool): Object =
  if input:
    return trueObj
  return falseObj

proc evalBangopExpression(right: Object): Object =
  case right.kind
  of okBool:
    if right.boolVal:
      return falseObj
    else:
      return trueObj
  of okNull:
    return trueObj
  else:
    return falseObj

proc evalMinusopExpression(right: Object): Object =
  case right.kind
  of okInteger:
    return Object(kind: okInteger, intVal: -(right.intVal))
  else:
    return newError("unknown operator: -$1" % [right.inspectType()])

proc evalInfixExpression(op: string, left: Object, right: Object): Object =
  if left.kind == okInteger and right.kind == okInteger:
    return evalIntegerInfixExpression(op, left, right)
  elif left.kind == okString and right.kind == okString:
    return evalStringInfixExpression(op, left, right)
  elif left.kind == okBool and right.kind == okBool:
    case op
    of "==":
      return nativeBoolToBooleanObject(left.boolVal == right.boolVal)
    of "!=":
      return nativeBoolToBooleanObject(left.boolVal != right.boolVal)
    else:
      return newError("unknown operator: $1 $2 $3" %
          [left.inspectType(), op, right.inspectType()])
  else:
    return newError("type mismatch: $1 $2 $3" %
        [left.inspectType(), op, right.inspectType()])

proc evalIntegerInfixExpression(op: string, left: Object, right: Object): Object =
  let leftVal = left.intVal
  let rightVal = right.intVal
  case op
  of "+":
    return Object(kind: okInteger, intVal: leftVal + rightVal)
  of "-":
    return Object(kind: okInteger, intVal: leftVal - rightVal)
  of "*":
    return Object(kind: okInteger, intVal: leftVal * rightVal)
  of "/":
    return Object(kind: okInteger, intVal: leftVal div rightVal)
  of "<":
    return nativeBoolToBooleanObject(leftVal < rightVal)
  of ">":
    return nativeBoolToBooleanObject(leftVal > rightVal)
  of "==":
    return nativeBoolToBooleanObject(leftVal == rightVal)
  of "!=":
    return nativeBoolToBooleanObject(leftVal != rightVal)
  else:
    return newError("unknown operator: $1 $2 $3" %
        [left.inspectType(), op, right.inspectType()])

proc evalIfExpression(n: PNode, env: Environment): Object =
  let condition = eval(n.ifExpCondition, env)
  if condition.isError(): return condition
  if isTruthy(condition):
    return eval(n.ifExpConsequence, env)
  elif n.ifExpAlternative != nil:
    return eval(n.ifExpAlternative, env)
  else:
    return nullObj

proc isTruthy(o: Object): bool =
  case o.kind
  of okNull:
    return false
  of okBool:
    return o.boolVal
  else:
    return true

proc evalBlockStatement(blk: seq[PNode], env: Environment): Object =
  for stmt in blk:
    result = eval(stmt, env)
    if result != nil:
      if result.kind == okReturnVal or result.kind == okError:
        return result
  return result

proc newError(errMsg: string): Object =
  return Object(kind: okError, errorVal: errMsg)

proc isError(o: Object): bool =
  return o.kind == okError

proc evalExpressions(exps: seq[PNode], env: Environment): seq[Object] =
  for e in exps:
    let evaluated = eval(e, env)
    if evaluated.isError():
        result.add(evaluated)
        return result
    result.add(evaluated)
  return result

proc applyFunction(fn: Object, args: seq[Object]): Object =
  case fn.kind
  of okFunction:
    let extendedEnv = extendFunctionEnv(fn, args)
    let evaluated = eval(fn.fnBody, extendedEnv)
    return unwrapReturnValue(evaluated)
  of okBuiltin:
    return fn.builtinFn(args)
  else:
    return newError("not a function: " & fn.inspectType())

proc extendFunctionEnv(fn: Object, args: seq[Object]): Environment =
  let env = newEnclosedEnvironment(fn.fnEnv)
  for paramIdx, param in fn.fnParameters:
    env.setVal(param.identVal, args[paramIdx])
  return env

proc unwrapReturnValue(obj: Object): Object =
  if obj.kind == okReturnVal:
    return obj.returnVal
  return obj

proc evalStringInfixExpression(op: string, left: Object, right: Object): Object =
  case op
  of "+":
    return Object(kind: okString, stringVal: left.stringVal & right.stringVal)
  of "==":
    return Object(kind: okBool, boolVal: left.stringVal == right.stringVal)
  of "!=":
    return Object(kind: okBool, boolVal: left.stringVal != right.stringVal)
  else:
    return newError("unknown operator: $1 $2 $3" %
        [left.inspectType(), op, right.inspectType()])


# builtins

proc builtinLen(args: varargs[Object]): Object =
  if args.len != 1:
    return newError("wrong number of arguments. got=$1, want=1" % $args.len)
  case args[0].kind
  of okString:
    return Object(kind: okInteger, intVal: args[0].stringVal.len)
  else:
    return newError("argument to `len` not supported, got " & args[0].inspectType())
