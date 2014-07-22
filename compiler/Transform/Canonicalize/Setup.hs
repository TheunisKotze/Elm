{-# OPTIONS_GHC -Wall #-}
module Transform.Canonicalize.Setup (environment) where

import Control.Arrow (first)
import Control.Monad (foldM)
import Control.Monad.Error (throwError)
import qualified Data.Graph as Graph
import qualified Data.List as List
import qualified Data.Map as Map

import qualified AST.Expression.Valid as Valid

import AST.Module (Interface(iAdts, iTypes, iAliases))
import qualified AST.Module as Module
import qualified AST.Type as Type
import qualified AST.Variable as Var
import qualified AST.Declaration as D
import AST.PrettyPrint (pretty, eightyCharLines)
import qualified AST.Pattern as P
import Text.PrettyPrint as P

import Transform.Canonicalize.Environment as Env
import qualified Transform.Canonicalize.Type as Canonicalize

environment :: Module.Interfaces -> Module.ValidModule -> Canonicalizer [Doc] Environment
environment interfaces modul@(Module.Module _ _ _ imports _ decls) =
  do () <- allImportsAvailable
     let moduleName = Module.getName modul
     nonLocalEnv <- foldM (addImports moduleName interfaces) (builtIns moduleName) imports
     let (aliases, env) = List.foldl' (addDecl moduleName) ([], nonLocalEnv) $ map snd decls
     addTypeAliases moduleName aliases env
  where
    allImportsAvailable :: Canonicalizer [Doc] ()
    allImportsAvailable =
        case filter (not . found) modules of
          [] -> return ()
          missings -> throwError [ P.text (missingModuleError missings) ]
        where
          modules = map fst imports

          found m = Map.member m interfaces || List.isPrefixOf "Native." m

          missingModuleError missings =
              concat [ "The following imports were not found: "
                     , List.intercalate ", " missings
                     , "\n    You may need to compile with the --make "
                     , "flag to detect modules you have written." ]

addImports :: String -> Module.Interfaces -> Environment -> (String, Module.ImportMethod)
           -> Canonicalizer [Doc] Environment
addImports moduleName interfaces environ (name,method)
    | List.isPrefixOf "Native." name = return environ
    | otherwise =
        case method of
          Module.As name' ->
              return (updateEnviron (name' ++ "."))

          Module.Open (Var.Listing vs open)
              | open -> return (updateEnviron "")
              | otherwise -> foldM (addValue name interface) environ vs
    where
      interface = (Map.!) interfaces name

      updateEnviron prefix =
          let dict' = dict . map (first (prefix++)) in
          merge environ $
          Env { _home     = moduleName
              , _values   = dict' $ map pair (Map.keys (iTypes interface)) ++ ctors
              , _adts     = dict' $ map pair (Map.keys (iAdts interface))
              , _aliases  = dict' $ map alias (Map.toList (iAliases interface))
              , _patterns = dict' $ ctors
              }

      canonical :: String -> Var.Canonical
      canonical = Var.Canonical (Var.Module name)

      pair :: String -> (String, Var.Canonical)
      pair key = (key, canonical key)

      alias (x,(tvars,tipe)) = (x, (canonical x, tvars, tipe))

      ctors = concatMap (map (pair . fst) . snd . snd) (Map.toList (iAdts interface))

addValue :: String -> Module.Interface -> Environment -> Var.Value
         -> Canonicalizer [Doc] Environment
addValue name interface env value =
    let insert' x = insert x (Var.Canonical (Var.Module name) x)
        msg x = "Import Error: Could not import value '" ++ name ++ "." ++ x ++
                "'.\n    It is not exported by module " ++ name ++ "."
        notFound x = throwError [ P.text (msg x) ]
    in
    case value of
      Var.Value x
          | Map.notMember x (iTypes interface) -> notFound x
          | otherwise ->
              return $ env { _values = insert' x (_values env) }

      Var.Alias x ->
          case Map.lookup x (iAliases interface) of
            Just (tvars, t) ->
                let v = (Var.Canonical (Var.Module name) x, tvars, t) in
                return $ env { _aliases = insert x v (_aliases env) }
            Nothing ->
                case Map.lookup x (iAdts interface) of
                  Nothing -> notFound x
                  Just (_,_) ->
                      return $ env { _adts = insert' x (_adts env) }

      Var.ADT x (Var.Listing xs open) ->
          case Map.lookup x (iAdts interface) of
            Nothing -> notFound x
            Just (_tvars, ctors) ->
                do ctors' <- filterNames (map fst ctors)
                   return $ env { _adts = insert' x (_adts env)
                                , _values = foldr insert' (_values env) ctors'
                                , _patterns = foldr insert' (_patterns env) ctors'
                                }
                where
                  filterNames names
                      | open = return names
                      | otherwise =
                          case filter (`notElem` names) xs of
                            [] -> return names
                            c:_ -> notFound c

type Node = ((String, [String], Type.RawType), String, [String])

node :: String -> [String] -> Type.RawType -> Node
node name tvars alias = ((name, tvars, alias), name, edges alias)
    where
      edges tipe =
          case tipe of
            Type.Lambda t1 t2 -> edges t1 ++ edges t2
            Type.Var _ -> []
            Type.Type (Var.Raw x) -> [x]
            Type.App t ts -> edges t ++ concatMap edges ts
            Type.Record fs ext -> maybe [] edges ext ++ concatMap (edges . snd) fs
            Type.Aliased _ t -> edges t

addTypeAliases :: String -> [Node] -> Environment -> Canonicalizer [Doc] Environment
addTypeAliases moduleName nodes environ =
    foldM addTypeAlias environ (Graph.stronglyConnComp nodes)
  where
    addTypeAlias :: Environment -> Graph.SCC (String, [String], Type.RawType)
                 -> Canonicalizer [Doc] Environment
    addTypeAlias env scc =
      case Graph.flattenSCC scc of
        [(name, tvars, alias)] ->
            do alias' <- Env.onError throw (Canonicalize.tipe env alias)
               let value = (Var.Canonical (Var.Module moduleName) name, tvars, alias')
               return $ env { _aliases = insert name value (_aliases env) }
            where
              throw err =
                  let msg = "Problem with type alias '" ++ name ++ "':"
                  in  P.vcat [ P.text msg, P.text err ]

        aliases ->
            throwError [ P.vcat [ P.text (eightyCharLines 0 msg1)
                                , indented (map typeAlias aliases)
                                , P.text (eightyCharLines 0 msg2)
                                , indented (map datatype aliases)
                                , P.text (eightyCharLines 0 msg3)
                                ]
                       ]

    typeAlias (n,ts,t) = D.TypeAlias n ts t
    datatype (n,ts,t) = D.Datatype n ts [(n,[t])]

    indented :: [D.ValidDecl'] -> Doc
    indented decls = P.vcat (map prty decls) <> P.text "\n"
        where
          prty decl = P.text "\n    " <> pretty decl

    msg1 = "The following type aliases are mutually recursive, forming an \
           \infinite type. When you expand them, they just keep getting bigger:"
    msg2 = "Instead, you can try something like this:"
    msg3 = "It looks very similar, but an algebraic data type (ADT) \
           \actually creates a new type. Unlike with a type alias, this \
           \freshly created type is meaningful on its own, so an ADT \
           \does not need to be expanded."


-- When canonicalizing, all _values should be Local, but all _adts and _patterns
-- should be fully namespaced. With _adts, they may appear in types that can
-- escape the module.
addDecl :: String -> ([Node], Environment) -> D.ValidDecl' -> ([Node], Environment)
addDecl moduleName info@(nodes,env) decl =
    let namespacedVar     = Var.Canonical (Var.Module moduleName)
        addLocal      x e = insert x (Var.local     x) e
        addNamespaced x e = insert x (namespacedVar x) e
    in
    case decl of
      D.Definition (Valid.Definition pattern _ _) ->
          (,) nodes $ env
           { _values = foldr addLocal (_values env) (P.boundVarList pattern) }

      D.Datatype name _ ctors ->
          (,) nodes $ env
           { _values   = addCtors addLocal (_values env)
           , _adts     = addNamespaced name (_adts env)
           , _patterns = addCtors addNamespaced (_patterns env)
           }
        where
          addCtors how e = foldr how e (map fst ctors)

      D.TypeAlias name tvars alias ->
          (,) (node name tvars alias : nodes) $ env
           { _values = case alias of
                         Type.Record _ _ -> addLocal name (_values env)
                         _               -> _values env
           }

      D.Port port ->
          let portName = case port of
                           D.Out name _ _ -> name
                           D.In name _    -> name
          in
              (,) nodes $ env { _values = addLocal portName (_values env) }

      D.Fixity{} -> info

      D.DocComment{} -> error "Error: DocComment in valid declaration"
