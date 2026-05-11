import ./ast, ./obj

import strutils, tables

proc eval*(n: PNode, env: Environment): PObject
proc evalBangopExpression(right: PObject): PObject
proc evalPrefixExpression(op: string, right: PObject): PObject
proc nativeBoolToBooleanObject(input: bool): PObject
proc evalMinusopExpression(right: PObject): PObject
proc evalInfixExpression(op: string, left: PObject, right: PObject): PObject
proc evalIntegerInfixExpression(op: string, left: PObject, right: PObject): PObject
proc evalIfExpression(n: PNode, env: Environment): PObject
proc isTruthy(o: PObject): bool
proc evalBlockStatement(blk: seq[PNode], env: Environment): PObject
proc evalProgram(program: PNode, env: Environment): PObject
proc newError*(errMsg: string): PObject
proc isError(o: PObject): bool
proc evalIdent(n: PNode, env: Environment): PObject
proc evalExpressions(exps: seq[PNode], env: Environment): seq[PObject]
proc applyFunction(fn: PObject, args: seq[PObject]): PObject
proc extendFunctionEnv(fn: PObject, args: seq[PObject]): Environment
proc unwrapReturnValue(obj: PObject): PObject
proc evalStringInfixExpression(op: string, left: PObject, right: PObject): PObject

proc builtinLen(args: varargs[PObject]): PObject

let
  builtins* = {
    "len": PObject(kind: okBuiltin, builtinFn: builtinLen),
    }.toTable
  trueObj = PObject(kind: okBool, boolVal: true)
  falseObj = PObject(kind: okBool, boolVal: false)
  nullObj* = PObject(kind: okNull)


proc eval(n: PNode, env: Environment): PObject =
  case n.kind
  of nkProgram:
    return evalProgram(n, env)
  of nkExpressionStatement:
    return eval(n.expression, env)
  of nkIntLit:
    return PObject(kind: okInteger, intVal: n.intVal)
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
    return PObject(kind: okReturnVal, returnVal: val)
  of nkLetStatement:
    let val = eval(n.letVal, env)
    if val.isError(): return val
    env.setVal(n.letIdent.tokenLiteral(), val)
  of nkIdent:
    return evalIdent(n, env)
  of nkFunctionLit:
    let params = n.fnParameters
    let body = n.fnBody
    return PObject(kind: okFunction, fnParameters: params, fnBody: body, fnEnv: env)
  of nkCallExpression:
    let function = eval(n.callExpFunction, env)
    if function.isError():
      return function
    let args = evalExpressions(n.callExpArguments, env)
    if args.len == 1 and args[0].isError():
      return args[0]
    return applyFunction(function, args)
  of nkStringLit:
    return PObject(kind: okString, stringVal: n.stringVal)
  of nkNull:
    return nullObj

  return nil

proc evalIdent(n: PNode, env: Environment): PObject =
  let (val, ok) = env.getVal(n.identVal)
  if ok:
    return val
  if builtins.hasKey(n.identVal):
    return builtins[n.identVal]
  return newError("identifier not found: " & n.identVal)

proc evalProgram(program: PNode, env: Environment): PObject =
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

proc evalPrefixExpression(op: string, right: PObject): PObject =
  case op
  of "!":
    return evalBangopExpression(right)
  of "-":
    return evalMinusopExpression(right)
  else:
    return newError("unknown operator: $1$2" % [op, right.inspectType()])

proc nativeBoolToBooleanObject(input: bool): PObject =
  if input:
    return trueObj
  return falseObj

proc evalBangopExpression(right: PObject): PObject =
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

proc evalMinusopExpression(right: PObject): PObject =
  case right.kind
  of okInteger:
    return PObject(kind: okInteger, intVal: -(right.intVal))
  else:
    return newError("unknown operator: -$1" % [right.inspectType()])

proc evalInfixExpression(op: string, left: PObject, right: PObject): PObject =
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

proc evalIntegerInfixExpression(op: string, left: PObject, right: PObject): PObject =
  let leftVal = left.intVal
  let rightVal = right.intVal
  case op
  of "+":
    return PObject(kind: okInteger, intVal: leftVal + rightVal)
  of "-":
    return PObject(kind: okInteger, intVal: leftVal - rightVal)
  of "*":
    return PObject(kind: okInteger, intVal: leftVal * rightVal)
  of "/":
    return PObject(kind: okInteger, intVal: leftVal div rightVal)
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

proc evalIfExpression(n: PNode, env: Environment): PObject =
  let condition = eval(n.ifExpCondition, env)
  if condition.isError(): return condition
  if isTruthy(condition):
    return eval(n.ifExpConsequence, env)
  elif n.ifExpAlternative != nil:
    return eval(n.ifExpAlternative, env)
  else:
    return nullObj

proc isTruthy(o: PObject): bool =
  case o.kind
  of okNull:
    return false
  of okBool:
    return o.boolVal
  else:
    return true

proc evalBlockStatement(blk: seq[PNode], env: Environment): PObject =
  for stmt in blk:
    result = eval(stmt, env)
    if result != nil:
      if result.kind == okReturnVal or result.kind == okError:
        return result
  return result

proc newError(errMsg: string): PObject =
  return PObject(kind: okError, errorVal: errMsg)

proc isError(o: PObject): bool =
  return o.kind == okError

proc evalExpressions(exps: seq[PNode], env: Environment): seq[PObject] =
  for e in exps:
    let evaluated = eval(e, env)
    if evaluated.isError():
        result.add(evaluated)
        return result
    result.add(evaluated)
  return result

proc applyFunction(fn: PObject, args: seq[PObject]): PObject =
  case fn.kind
  of okFunction:
    let extendedEnv = extendFunctionEnv(fn, args)
    let evaluated = eval(fn.fnBody, extendedEnv)
    return unwrapReturnValue(evaluated)
  of okBuiltin:
    return fn.builtinFn(args)
  else:
    return newError("not a function: " & fn.inspectType())

proc extendFunctionEnv(fn: PObject, args: seq[PObject]): Environment =
  let env = newEnclosedEnvironment(fn.fnEnv)
  for paramIdx, param in fn.fnParameters:
    env.setVal(param.identVal, args[paramIdx])
  return env

proc unwrapReturnValue(obj: PObject): PObject =
  if obj.kind == okReturnVal:
    return obj.returnVal
  return obj

proc evalStringInfixExpression(op: string, left: PObject, right: PObject): PObject =
  case op
  of "+":
    return PObject(kind: okString, stringVal: left.stringVal & right.stringVal)
  of "==":
    return PObject(kind: okBool, boolVal: left.stringVal == right.stringVal)
  of "!=":
    return PObject(kind: okBool, boolVal: left.stringVal != right.stringVal)
  else:
    return newError("unknown operator: $1 $2 $3" %
        [left.inspectType(), op, right.inspectType()])


# builtins

proc builtinLen(args: varargs[PObject]): PObject =
  if args.len != 1:
    return newError("wrong number of arguments. got=$1, want=1" % $args.len)
  case args[0].kind
  of okString:
    return PObject(kind: okInteger, intVal: args[0].stringVal.len)
  else:
    return newError("argument to `len` not supported, got " & args[0].inspectType())
