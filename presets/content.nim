## module for defining and storing 'content' ref objects: blocks, items, enemy archetypes, etc

import macros, strutils, sets, tables, ../futils

#holds default values for fields of types
#maps type name, field name to initialization node
var defaultValues {.compileTime.}: Table[string, Table[string, NimNode]]

## base content class, should be extended
type Content* = ref object of RootObj
  name*: string
  id*: uint32

proc postStrVal(node: NimNode): string =
  if node.kind == nnkPostfix: node[1].strVal else: node.strVal

macro defineContentTypes*(body: untyped): untyped =
  result = body
  
  for typeSec in result:
    for typeD in typeSec:
      let name = typeD[0].postStrVal

      if typeD[2][0].kind != nnkEmpty:
        for i in typeD[2][0][2]:
          if i[2].kind != nnkEmpty:
            if not defaultValues.hasKey(name):
              defaultValues[name] = initTable[string, NimNode]()
            #save default value
            defaultValues[name][i[0].postStrVal] = i[2]

            i[2] = newEmptyNode()
      

proc deCapitalizeAscii(s: string): string =
  var s = s
  s[0] = s[0].toLowerAscii
  s

proc parseSection(name, body: NimNode, id: var int, all, vars, inits: var NimNode) =
  let
    listName = ident(name.strVal.deCapitalizeAscii & "List")
    listValues = newNimNode(nnkBracket)
  
  template colon(a, b: NimNode): NimNode =
    newTree(nnkExprColonExpr, a, b)

  template assign(a, b: NimNode) =
    inits.add newTree(nnkAsgn, a, b)
  
  template decl(aName: NimNode, name = name): NimNode =
    newTree(nnkIdentDefs, newTree(nnkPostfix, ident("*"), aName), name, newEmptyNode())
  
  template newConstr(id: var int): NimNode =
    id.inc()
    newTree(nnkObjConstr, name, ident("id").colon(newLit(id-1))) 
  
  for o in body:
    var constr = newConstr(id)
    
    template addName(aName: NimNode) =
      all.add aName
      listValues.add aName
      vars.add decl(aName)
      constr.add ident("name").colon(newLit(aName.strVal))

    case o.kind:
    of nnkIdent:  
      addName o
      o.assign constr
    of nnkCall, nnkCommand:
      let
        oName = o[0]
        maybeBody = o[^1]
        maybeInit = o[1]

      oName.check nnkIdent

      addName oName

      if maybeBody.kind == nnkStmtList:
        for f in maybeBody:
          f.check nnkCall
          f.check 2

          let
            fName = f[0]
            fValue = f[1]
          
          fName.check nnkIdent
          fValue.check nnkStmtList
          fValue.check 1

          constr.add(fName.colon(fValue[0]))

      
      oName.assign constr

      case maybeInit.kind:
      of nnkStmtList:
        discard
      of nnkCall:
        maybeInit.insert(1, oName)
        inits.add maybeInit
      of nnkIdent:
        inits.add newTree(nnkCall, maybeInit, oName)
      else:
        error "illegal initializer", oName
    else:
      error "illegal object declaration", o
  
  inits.add quote do: 
    `listName`.add `listValues`
  vars.add decl(listName, quote do: seq[`name`])

## Syntax sugar for defining and initializing content of the game.
## Way you define objects inside a macro reduces amount of text you 
## have to write and makes content more readable. example:
## 
##  ..block::nim
##  makeContent:
##    CustomTypeName:
##      objectName
##      ## or
##      objectName initializer
##      ## or
##      objectName initializer(argument)
##      ## or 
##      objectName:
##        fieldName: value
##      ## or case 2 and 3 combined with 4
## 
##  
## TODO: The syntax does not propagate into nested expression.
## All content is stored in its own list and also all content goes to 
## `all` list. Every structure you include has to inherit `Content`
macro makeContent*(body: untyped): untyped =
  body.check nnkStmtList

  var 
    id: int
    inits = newStmtList()
    all = newNimNode(nnkBracket)
    vars = newNimNode(nnkVarSection)

  result = newStmtList()

  for s in body:
    s.check nnkCall
    s.check 2

    let
      sName = s[0]
      sBody = s[1]
    
    sName.check nnkIdent
    sBody.check nnkStmtList

    parseSection(sName, sBody, id, all, vars, inits)
  
  let a = ident("all") # macro is buggy
  inits.add quote do: 
    `a`.extend `all`
  
  result.add vars
  result.add quote do: 
    var all {.inject.}: seq[Content]
  result.add quote do:
    template initContent =
      `inits`
  
  echo result.repr

func extend*[L, T](t: var seq[Content], elems: array[L, T]) =
  t.setLen(elems.len)
  for i, e in elems:
    t[i] = e

when isMainModule:
  type
    Something = ref object of Content
      x, y, z: int
      foo: string

  func initializer(s: var Something, p, k = 0, nm = "Something") =
    s.x += p
    s.y += k
    s.foo &= nm

  expandMacros:
    makeContent:
      Something: # type block
        something: # object declaration
          x: 1
          y: 2
          z: 6
          foo: "hell"
        somethingElse # empty constructor
        somethingInitialized initializer(2, 5, "moo") # additional initializer call
        somethingSpecial initializer: # combined, initializer with no parameters
          x: 2
          y: 4
          z: 6
          foo: "pop"
  
  initContent()
