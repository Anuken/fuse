import macros, tables

var eventHandlers* {.compileTime} = newTable[string, seq[NimNode]]()

## copies an array into a seq, element by element.
macro minsert*(dest: untyped, index: int, data: untyped): untyped =
  result = newStmtList()
  
  if data.kind == nnkBracket:
    for i in 0..<data.len:
      result.add newAssignment(newNimNode(nnkBracketExpr).add(dest).add(infix(index, "+", newIntLitNode(i))), data[i])
  else:
    error("Insertion data must be array!", data)

## exports all types/variables in the macro body
macro exportAll*(body: untyped) =
  proc traverse(parent: NimNode) =
    if parent.kind == nnkTypeDef:
      if parent[0].kind == nnkIdent:
        parent[0] = postfix(parent[0], "*")
    
    if parent.kind in [nnkProcDef, nnkTemplateDef, nnkMacroDef]:
      if parent[0].kind == nnkIdent:
        parent[0] = postfix(parent[0], "*")

    if parent.kind in [nnkVarSection, nnkLetSection, nnkConstSection, nnkRecList]:
      for defs in parent:
        for (index, node) in defs.pairs:
          if node.kind == nnkIdent and index < defs.len - 2:
            defs[index] = postfix(node, "*")

    for node in parent:
      traverse(node)

  traverse(body)

  result = body

## macro to import all files in the current directory non-recursively
template importAll*(): untyped =
  macro importAllDef(filename: static[string]): untyped =
    result = newNimNode(nnkImportStmt)
    
    for f in walkDir("src", true):
      if f.kind == pcFile :
        let split = f.path.splitFile()
        if split.ext == ".nim" and split.name != filename[0..^5]: result.add ident(split.name)
  
  importAllDef(instantiationInfo().filename)

## registers an event to be handled with `onEventName:`
macro event*(tname: untyped, args: varargs[untyped]): untyped =
  result = newStmtList()

  let td = quote do:
    type `tname`* = object

  let rec = newNimNode(nnkRecList)
  td[0][2][2] = rec

  for arg in args:
    rec.add(newIdentDefs(postfix(arg[0], "*"), arg[1], newEmptyNode()))
  
  result.add td

  let
    namestr = newLit(tname.repr)
    listenName = ident($tname.repr & "Proc")
    handleName = ident("on" & $tname.repr)
    fireName = ident("fire" & $tname.repr)

  result.add quote do:
    type `listenName`* = proc(event: `tname`)
    var `fireName`*: `listenName` = proc(event: `tname`) = discard
    proc fire*(event: `tname`) = `fireName`(event)
    macro `handleName`*(body: untyped) =
      eventHandlers.mgetOrPut(`namestr`, newSeq[NimNode]()).add(body)
  
## finishes building events - this must be called before any events are used!
macro buildEvents*() =
  result = newStmtList()
  for key, val in eventHandlers.pairs:
    let 
      fireName = ident("fire" & key)
      tname = ident(key)
    var sts = newStmtList()
    for node in val:
      sts.add quote do:
        block:
          `node`

    result.add quote do:
      `fireName` = proc(event {.inject.}: `tname`) =
        `sts`

## asserts anything
func check*(node: NimNode, cond: bool,  message: string) =
  if cond:
    error(message, node)

## asserts length of node
func check*(node: NimNode, len: int, message: string = "") =
  check(node, node.len != len, message & "(lon != " & $len & ")")

## asserts kind of node
func check*(node: NimNode, kind: NimNodeKind, message: string = "") =
  check(node, node.kind != kind, message & "(kind != " & $kind & ")")   