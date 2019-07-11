{-# OPTIONS_GHC -Wall #-}
module AST.DeclarationUsages
  ( AFunction
  , DeclarationUsages
  , writeUsagesToFile
  ) where

import           AST.Canonical        (CaseBranch (..), Decls (..), Def (..),
                                       Expr, Expr_ (..), FieldUpdate (..),
                                       Module)
import qualified AST.Canonical        as Can
import           Data.Function        ((&))
import qualified Data.Map             as Map
import           Data.Name            (Name)
import qualified Data.Name
import qualified Elm.ModuleName       as ModuleName
import qualified Elm.Package          as Pkg
import qualified Reporting.Annotation as A


data AFunction = AFunction
    { _f_package :: String
    , _f_module  :: String
    , _f_name    :: String
    } deriving Eq


-- "Given function" uses "these other functions" in its declaration
data DeclarationUsages =
    DeclarationUsages AFunction [AFunction] deriving (Eq)


getDeclarationUsages :: Module -> [DeclarationUsages]
getDeclarationUsages modul =
    [ DeclarationUsages
        (extractFunction (Can._name modul) (getDefName def))
        (getFunctionsUsedInDef def)
    | def <- getDefsFromDecls $ Can._decls modul
    ]


extractFunction :: ModuleName.Canonical -> Name -> AFunction
extractFunction canName name = AFunction
    (Pkg.toChars $ ModuleName._package canName)
    (Data.Name.toChars $ ModuleName._module canName)
    (Data.Name.toChars name)


serialize :: DeclarationUsages -> ((String, String, String), [(String, String, String)])
serialize (DeclarationUsages declaredFunction usages) =
    (toTriple declaredFunction, fmap toTriple usages)
  where
    toTriple (AFunction "author/project" m n) = ("", m, n)
    toTriple (AFunction p m n)                = (p, m, n)

writeUsagesToFile :: Module -> IO ()
writeUsagesToFile modul =
   -- putStrLn $ "Writing declaration usages to " <> outFile
   getDeclarationUsages modul
     & fmap (show . serialize)
     & unlines
     & writeFile outFile
  where
    outFile = getModuleName modul <> ".usages"

getDefName :: Def -> Name
getDefName def =  A.toValue $ case def of
    Def locatedName _ _          -> locatedName
    TypedDef locatedName _ _ _ _ -> locatedName


getDefsFromDecls :: Decls -> [Def]
getDefsFromDecls decls = case decls of
    Declare def decls1         -> def : getDefsFromDecls decls1
    DeclareRec def defs decls1 -> def : defs <> getDefsFromDecls decls1
    SaveTheEnvironment         -> []


getFunctionsUsedInDef :: Def -> [AFunction]
getFunctionsUsedInDef def = case def of
    Def _ _ expr          -> getFunctionsUsedInExpr expr
    TypedDef _ _ _ expr _ -> getFunctionsUsedInExpr expr


getFunctionsUsedInExpr :: Expr -> [AFunction]
getFunctionsUsedInExpr expr = case A.toValue expr of
    VarLocal _Name -> []
    VarTopLevel canName name -> [extractFunction canName name]
    VarKernel _Name1 _Name2 -> []
    VarForeign canName name _Annotation -> [extractFunction canName name]
    VarCtor _CtorOpts _ModuleName_Canonical _Name _In_dex_ZeroBased _Annotation -> []
    VarDebug _ModuleName_Canonical _Name _Annotation -> []
    VarOperator _Name1 _ModuleName_Canonical _Name2 _Annotation  -> []
    Chr _ES_String -> []
    Str _ES_String -> []
    Int _In_t -> []
    Float _EF_Float -> []
    List exprs -> concatMap getFunctionsUsedInExpr exprs
    Negate expr1 -> getFunctionsUsedInExpr expr1
    Binop _Name1 _ModuleName_Canonical _Name2 _Annotation expr1 expr2  -> getFunctionsUsedInExpr expr1 <> getFunctionsUsedInExpr expr2
    Lambda _Patterns expr1 -> getFunctionsUsedInExpr expr1
    Call expr1 exprs -> getFunctionsUsedInExpr expr1 <> concatMap getFunctionsUsedInExpr exprs
    If pairsOfExprs expr1 -> concatMap (\(e1,e2) -> getFunctionsUsedInExpr e1 <> getFunctionsUsedInExpr e2) pairsOfExprs <> getFunctionsUsedInExpr expr1
    Let def1 expr1 -> getFunctionsUsedInDef def1 <> getFunctionsUsedInExpr expr1
    LetRec defs expr1 -> concatMap getFunctionsUsedInDef defs <> getFunctionsUsedInExpr expr1
    LetDestruct _Pattern expr1 expr2 -> getFunctionsUsedInExpr expr1 <> getFunctionsUsedInExpr expr2
    Case expr1 caseBranches -> getFunctionsUsedInExpr expr1 <> concatMap getFunctionsUsedInCaseBranch caseBranches
    Accessor _Name -> []
    Access expr1 _Located_Name -> getFunctionsUsedInExpr expr1
    Update _Name expr1 fieldUpdateMap -> getFunctionsUsedInExpr expr1 <> concatMap getFunctionsUsedFieldUpdate (Map.elems fieldUpdateMap)
    Record exprsMap  -> concatMap getFunctionsUsedInExpr (Map.elems exprsMap)
    Unit -> []
    Tuple expr1 expr2 maybeExpr -> getFunctionsUsedInExpr expr1 <> getFunctionsUsedInExpr expr2 <> foldMap getFunctionsUsedInExpr maybeExpr
    Shader _Shader_Source _Shader_Types   -> []


getFunctionsUsedInCaseBranch :: CaseBranch -> [AFunction]
getFunctionsUsedInCaseBranch (CaseBranch _ expr) =
    getFunctionsUsedInExpr expr


getFunctionsUsedFieldUpdate :: FieldUpdate -> [AFunction]
getFunctionsUsedFieldUpdate (FieldUpdate _ expr) =
    getFunctionsUsedInExpr expr


getModuleName :: Module -> String
getModuleName = Data.Name.toChars . ModuleName._module . Can._name
