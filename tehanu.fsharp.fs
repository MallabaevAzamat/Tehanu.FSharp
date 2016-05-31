module Tehanu.FSharp

open Tehanu.Core            
open Tehanu.Core.Generators
open Tehanu.Core.Patterns
open Tehanu.Attributes.Parser
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range

let Ident2Text (ident: Ident) =
  match ident.idText with  
  | "op_Addition" -> "+"
  | "op_Subtraction" -> "-" 
  | "op_Multiply" -> "*" 
  | "op_Division" -> "*" 
  | "op_GreaterThan" -> ">"
  | "op_LessThan" -> "<"
  | text -> text

let LongIdent2Text (lid: LongIdent) =
  String.concat "." [for i in lid -> Ident2Text i]

let LongIdentWithDots2Text (liwd: LongIdentWithDots) =
  LongIdent2Text liwd.Lid

let SynType2Type (typ: SynType):Tree =
  match typ with
  | SynType.LongIdent(li) -> genTypeId (LongIdentWithDots2Text li)
  | _ -> Atom ("not supported type " + (string typ))

let rec SynExpr2Expr (expr: SynExpr):Tree =
  match expr with                                                                    
  | SynExpr.Const(SynConst.Unit, _) -> genExprConstUnit ()
  | SynExpr.Const(SynConst.String(s, _), _) -> genExprConstString s
  | SynExpr.Const(SynConst.Int32(i), _) -> genExprConstInt i
  | SynExpr.Paren(e, _, _, _) -> SynExpr2Expr e
  | SynExpr.Ident(i) -> genExprId <| Ident2Text i
  | SynExpr.LongIdent(_, i, _, _) -> genExprId <| LongIdentWithDots2Text i
  | SynExpr.App(_, _, l, r, _) -> genExprApp (SynExpr2Expr l) (SynExpr2Expr r)
  | SynExpr.IfThenElse(c, t, Some(e), _, _, _, _) -> genExprIf (SynExpr2Expr c) (SynExpr2Expr t) (SynExpr2Expr e)
  | SynExpr.Typed(expr, typ, _) -> SynExpr2Expr expr
  | _ -> Atom ("not supported expr " + (string expr))

let parseAttr (attr: Tree) =
  match attr with
  | Attr (name, ExprConstString txt) ->                
    genAttr name (parseAttribute txt)
  | _ -> Atom ("not supported attr " + string attr)

let SynArgInfo2Arg (sai: SynArgInfo): Tree =
  match sai with                                            
  | SynArgInfo(_, _, Some(i)) -> genArg (Ident2Text i) (genTypeId "int")
  | SynArgInfo(_, _, None) -> genArg "_" (genTypeId "int")

let SynValData2ValData (svd: SynValData): Tree =
  match svd with
  | SynValData(_, SynValInfo(argss, ret), _) ->
    Pair(ref <| genList [for args in argss -> genList [for arg in args -> SynArgInfo2Arg arg]], ref <| SynArgInfo2Arg ret)

let SynModuleDecl2Decl (decl: SynModuleDecl): Tree  =
  match decl with
  | SynModuleDecl.Let(_, [Binding(access, kind, inlin, mutabl, attrs, xmlDoc, data, SynPat.LongIdent(liwd, _, _, _, _, _), Some(SynBindingReturnInfo(rettype, _, _)), body, m, sp)], _) -> 
    genLet (genList [for attr in attrs -> parseAttr (genAttr (LongIdentWithDots2Text attr.TypeName) (SynExpr2Expr <| attr.ArgExpr))]) (LongIdentWithDots2Text liwd) (SynValData2ValData data) (SynExpr2Expr body)
  | _ -> Atom ("not supported decl " + string decl)

let SynModule2Module (modul: SynModuleOrNamespace): Tree =
  match modul with
  | SynModuleOrNamespace(lid, true, decls, _, _, _, _) ->
    genModule (LongIdent2Text lid) (genList [for decl in decls -> SynModuleDecl2Decl decl])
  | _ -> Atom ("not supported modul " + string modul)

let checker = FSharpChecker.Create()

let getUntypedTree (file, input) =
  // Get compiler options for the 'project' implied by a single script file
  let projOptions =
    checker.GetProjectOptionsFromScript(file, input)
    |> Async.RunSynchronously

  // Run the first phase (untyped parsing) of the compiler
  let parseFileResults =
    checker.ParseFileInProject(file, input, projOptions)
    |> Async.RunSynchronously

  match parseFileResults.ParseTree with
  | Some tree -> tree
  | None -> failwith "Something went wrong during parsing!"

let toTree file input =
  let tree = getUntypedTree(file, input)
  // Extract implementation file details
  match tree with
  | ParsedInput.ImplFile(ParsedImplFileInput(fn, script, name, _, _, modules, _)) ->
    genList [for modul in modules -> SynModule2Module modul]
  | _ -> Atom ("not supported parse input " + string tree)
