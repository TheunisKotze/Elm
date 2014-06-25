{-# OPTIONS_GHC -Wall #-}
module Transform.Definition where

import Control.Applicative ((<$>))
import Control.Monad.Error
import qualified Control.Monad.State as State
import AST.Annotation ( Annotated(A), Region )
import qualified AST.Pattern as P
import qualified AST.Type as T
import qualified AST.Expression.General as G
import qualified AST.Expression.Source as Source
import qualified AST.Expression.Valid as Valid
import qualified AST.Variable as Var
import qualified Transform.Expression as Crawl

type Validated a = ErrorT String (State.State Int) a

validateExpr :: Source.Expr -> Validated Valid.RawExpr
validateExpr =
    Crawl.crawl validateDefs validateExtras

validateDefs :: [Source.Def] -> Validated [Valid.RawDef]
validateDefs defs =
    case defs of
      Source.TypeAnnotation name tipe : rest ->
          case rest of
            Source.Definition pat@(P.Var name') expr : rest'
                | name == name' ->
                    do expr' <- validateExpr expr
                       let def = Valid.Definition pat expr' (Just tipe)
                       (:) def <$> validateDefs rest'

            _ -> throwError $
                 "Formatting Error: The type annotation for '" ++ name ++
                 "' must be directly above its definition."

      Source.Definition pat expr : rest ->
          do expr' <- validateExpr expr
             let def = Valid.Definition pat expr' Nothing
             (:) def <$> validateDefs rest

      [] -> return []

validateExtras :: Region -> Source.Cmds -> Validated Valid.RawExpr
validateExtras region (Source.Cmds implementation commands) =
  do impl <- validateExpr implementation
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
            validateExpr expr

        Source.Do expr : cs ->
            let andThen = makeAndThen region name in
            do part1 <- validateExpr expr
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
          expr' <- validateExpr expr
          let def = Valid.Definition pat expr' tipe
          go (def:defs) rest

        bind pattern expression tipe rest = do
          expr <- validateExpr expression
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

