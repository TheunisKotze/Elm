{-# OPTIONS_GHC -Wall #-}
module Transform.Validate (Validated, expression, declarations) where

import Control.Applicative ((<$>))
import Control.Monad.Error
import qualified Control.Monad.State as State
import AST.Annotation ( Annotated(A), Region )
import qualified AST.Pattern as P
import qualified AST.Type as T
import qualified AST.Declaration as D
import qualified AST.Expression.General as G
import qualified AST.Expression.Source as Source
import qualified AST.Expression.Valid as Valid
import qualified AST.Variable as Var
import qualified Transform.Crawl as Crawl

type Validated a = ErrorT String (State.State Int) a

expression :: Source.Expr -> Validated Valid.RawExpr
expression =
    Crawl.crawl definitions extras

definitions :: [Source.Def] -> Validated [Valid.RawDef]
definitions defs =
    case defs of
      Source.TypeAnnotation name tipe : rest ->
          case rest of
            Source.Definition pat@(P.Var name') expr : rest'
                | name == name' ->
                    do expr' <- expression expr
                       let def = Valid.Definition pat expr' (Just tipe)
                       (:) def <$> definitions rest'

            _ -> throwError $
                 "Formatting Error: The type annotation for '" ++ name ++
                 "' must be directly above its definition."

      Source.Definition pat expr : rest ->
          do expr' <- expression expr
             let def = Valid.Definition pat expr' Nothing
             (:) def <$> definitions rest

      [] -> return []

extras :: Region -> Source.Cmds -> Validated Valid.RawExpr
extras region (Source.Cmds implementation commands) =
  do impl <- expression implementation
     name <- freshName
     with name impl <$> go name commands
  where
    with name impl body = ann (G.Let [def] body)
        where
          ann = A region
          def = Valid.Definition (P.Var name) (ann (G.Access impl "andThen")) Nothing {-(Just tipe)
          tipe = let cmd x = T.App (T.Var "cmd") [x]
                     a = T.Var "a"
                     b = T.Var "b"
                 in  T.Lambda (cmd a) (T.Lambda (T.Lambda a (cmd b)) (cmd b))-}

    freshName = do
      n <- lift State.get
      lift (State.modify (+1))
      return ("_andThen" ++ show n)

    go name cmds =
      case cmds of
        Source.Do expr : [] ->
            expression expr

        Source.Do expr : cs ->
            let andThen = makeAndThen region name in
            do part1 <- expression expr
               part2 <- go name cs
               return (part1 `andThen` A region (G.Lambda P.Anything part2))

        Source.CmdLet _defs : [] ->
            throwError "Error: Command syntax cannot end with variable assignement.\n\
                       \It must end with an expression."

        Source.CmdLet defs : cs ->
            do body <- go name cs
               validateCmdDefs region name body defs

        [] -> throwError "Command syntax must have at least one command!"

validateCmdDefs :: Region -> String -> Valid.RawExpr -> [Source.CmdDef]
                -> Validated Valid.RawExpr
validateCmdDefs region andThenName body cmdDefs =
    go [] cmdDefs
  where
    andThen = makeAndThen region andThenName

    go defs cs =
      case cs of
        Source.TypeAnn name tipe : rest ->
            case rest of
              Source.Assign pat@(P.Var name') expr : rest' | name == name' ->
                  assign pat expr (Just tipe) rest'

              Source.AndThen pat@(P.Var name') expr : rest' | name == name' ->
                  bind pat expr (Just tipe) rest'

              _ -> throwError $
                   "Formatting Error: The type annotation for '" ++ name ++
                   "' must be directly above its definition in command syntax."

        Source.Assign pat expr : rest ->
            assign pat expr Nothing rest

        Source.AndThen pat expr : rest ->
            bind pat expr Nothing rest

        [] -> return (makeLet defs body)

      where
        assign pat expr tipe rest = do
          expr' <- expression expr
          let def = Valid.Definition pat expr' tipe
          go (def:defs) rest

        bind pattern e tipe rest = do
          expr <- expression e
          callback <- go [] rest
          return $ makeLet defs (expr `andThen` A region (G.Lambda pattern callback))

        makeLet ds expr =
            case ds of
              [] -> expr
              _  -> A region (G.Let ds expr)

makeAndThen :: Region -> String -> Valid.RawExpr -> Valid.RawExpr -> Valid.RawExpr
makeAndThen region name cmd callback =
    app (app (var (Var.Raw name)) cmd) callback
    where
      app f v = A region (G.App f v)
      var x = A region (G.Var x)

declarations :: [D.SourceDecl] -> Validated [D.ValidDecl]
declarations decls =
  let go = declarations
      msg x = "Formatting Error: The type annotation for '" ++ x ++
              "' must be directly above its definition."
  in
  case decls of
    -- simple cases, pass them through with no changes
    [] -> return []

    D.Datatype name tvars ctors : rest ->
        (:) (D.Datatype name tvars ctors) <$> go rest

    D.TypeAlias name tvars alias : rest ->
        (:) (D.TypeAlias name tvars alias) <$> go rest

    D.Fixity assoc prec op : rest ->
        (:) (D.Fixity assoc prec op) <$> go rest

    -- combine definitions
    D.Definition def : defRest ->
        case def of
          Source.Definition pat expr ->
              do expr' <- expression expr
                 let def' = Valid.Definition pat expr' Nothing
                 (:) (D.Definition def') <$> go defRest

          Source.TypeAnnotation name tipe ->
              case defRest of
                D.Definition (Source.Definition pat@(P.Var name') expr) : rest
                    | name == name' ->
                        do expr' <- expression expr
                           let def' = Valid.Definition pat expr' (Just tipe)
                           (:) (D.Definition def') <$> go rest

                _ -> throwError (msg name)

    -- combine ports
    D.Port port : portRest ->
        case port of
          D.PPDef name _ -> throwError (msg name)
          D.PPAnnotation name tipe ->
              case portRest of
                D.Port (D.PPDef name' expr) : rest | name == name' ->
                    do expr' <- expression expr
                       (:) (D.Port (D.Out name expr' tipe)) <$> go rest

                _ -> (:) (D.Port (D.In name tipe)) <$> go portRest

