{-# OPTIONS_GHC -Wall #-}

module AST.Expression.Valid where

import AST.PrettyPrint
import Text.PrettyPrint as P
import qualified AST.Expression.General as General
import AST.Type (Type)
import qualified AST.Variable as Var
import qualified AST.Annotation as Annotation
import qualified AST.Pattern as Pattern

type RawExpr = Expr Var.Raw
type RawExpr' = Expr' Var.Raw

type CanonicalExpr = Expr Var.Canonical
type CanonicalExpr' = Expr' Var.Canonical

type RawDef = Def Var.Raw
type CanonicalDef = Def Var.Canonical

{-| "Valid" expressions. When the compiler checks that type annotations and
ports are all paired with definitions in the appropriate order, it collapses
them into Defs and Cmds that are easier to work with in later phases of
compilation.
-}
type Expr  var = General.Expr  Annotation.Region (Def var) (Ports var) var
type Expr' var = General.Expr' Annotation.Region (Def var) (Ports var) var

data Def var = Definition (Pattern.Pattern var) (Expr var) (Maybe (Type var))
    deriving (Show)

data Ports var
    = PortIn String (Type var)
    | PortOut String (Type var) (Expr var)
    deriving (Show)

instance Var.ToString var => Pretty (Def var) where
  pretty (Definition pattern expr maybeTipe) =
      prettyDef "=" pattern expr maybeTipe

prettyDef :: Var.ToString var =>
             String -> Pattern.Pattern var -> Expr var -> Maybe (Type var) -> Doc
prettyDef eq pattern expr maybeTipe =
    P.vcat [ annotation, definition ]
  where
    definition =
        pretty pattern <+> P.text eq <+> pretty expr

    annotation =
        case maybeTipe of
          Nothing -> P.empty
          Just tipe -> pretty pattern <+> P.colon <+> pretty tipe

instance Var.ToString var => Pretty (Ports var) where
  pretty port =
      case port of
        PortIn name _ ->
            P.text $ "<port:" ++ name ++ ">"

        PortOut _ _ signal ->
            pretty signal