
module Types.Unify (unify) where

import Control.Arrow (second)
import Control.Monad (liftM)
import Data.List (foldl')
import qualified Data.Set as Set
import qualified Data.Map as Map
import Guid
import Types
import Types.Constrain
import Types.Substitutions

import Control.DeepSeq (deepseq)
--import System.IO.Unsafe

prints xs v = v --} unsafePerformIO (putStrLn "----------" >> mapM print xs) `seq` v

unify hints modul = run $ do
  constraints <- constrain hints modul
  case constraints of
    Left msg -> return (Left msg)
    Right (escapees, cs) ->
        do subs <- solver cs Map.empty
           return ((,) escapees `liftM` subs)

eq ctx t1 t2 = Context ctx (t1 :=: t2)


solver [] subs = prints (Map.toList subs) $ return $ Right subs

--------  Destruct Type-constructors  --------

solver ((Context ctx (t1@(ADT n1 ts1) :=: t2@(ADT n2 ts2))) : cs) subs =
    if n1 /= n2 then uniError ctx t1 t2 else
        solver (zipWith (eq ctx) ts1 ts2 ++ cs) subs
solver ((Context ctx (LambdaT t1 t2 :=: LambdaT t1' t2')) : cs) subs =
    solver ([ eq ctx t1 t1', eq ctx t2 t2' ] ++ cs) subs

--------  Type-equality  --------

solver (Context ctx (VarT x :=: VarT y) : cs) subs
    | x == y    = solver cs subs
    | otherwise =
        case (Map.lookup x subs, Map.lookup y subs) of
          (Just (Super xts), Just (Super yts)) ->
             let ts = Set.intersection xts yts
                 setXY t = Map.insert x t . Map.insert y t
             in  case Set.toList ts of
                   []  -> unionError ctx xts yts
                   [t] -> let cs1 = map (\c -> cSub y t c `seq` cSub x t (cSub y t c)) cs in
                          cs1 `seq` solver cs1 (setXY t subs)
                   _   -> solver cs $ setXY (Super ts) subs
          (Just (Super xts), _) ->
              let cs2 = map (cSub y (VarT x)) cs in
              cs2 `seq` solver cs2 $ Map.insert y (VarT x) subs
          (_, _) ->
              let cs3 = map (cSub x (VarT y)) cs in
              cs3 `deepseq` solver cs3 $ Map.insert x (VarT y) subs
solver (Context ctx (VarT x :=: t) : cs) subs = do
  if x `occursIn` t then occursError ctx (VarT x) t else
      (case Map.lookup x subs of
         Nothing -> let cs4 = map (cSub x t) cs in
                    cs4 `deepseq` (solver cs4 . Map.map (tSub x t) $ Map.insert x t subs)
         Just (Super ts) ->
             let ts' = Set.intersection ts (Set.singleton t) in
             case Set.toList ts' of
               []   -> solver (Context ctx (t :<: Super ts) : cs) subs
               [t'] -> let cs5 = map (cSub x t) cs in
                       cs5 `seq` solver cs5 $ Map.insert x t' subs
               _    -> solver cs $ Map.insert x (Super ts') subs
         Just t' -> solver (Context ctx (t' :=: t) : cs) subs
      )
solver ((Context ctx (t :=: VarT x)) : cs) subs =
    solver ((Context ctx (VarT x :=: t)) : cs) subs
solver ((Context ctx (t1 :=: t2)) : cs) subs
    | t1 == t2  = solver cs subs
    | otherwise = uniError ctx t1 t2


--------  subtypes  --------

solver (Context ctx (VarT x :<: Super ts) : cs) subs =
    case Map.lookup x subs of
      Nothing -> solver cs $ Map.insert x (Super ts) subs
      Just (Super ts') ->
          case Set.toList $ Set.intersection ts ts' of
            []   -> unionError ctx ts ts'
            [t]  -> solver (map (cSub x t) cs) $ Map.insert x t subs
            ts'' -> solver cs $ Map.insert x (Super $ Set.fromList ts'') subs

solver (Context ctx (ADT "List" [t] :<: Super ts) : cs) subs
    | any f (Set.toList ts) = solver cs subs
    | otherwise = subtypeError ctx (ADT "List" [t]) (Super ts)
        where f (ADT "List" [VarT _]) = True
              f (ADT "List" [t']) = t == t'
              f _ = False

solver (Context ctx (t :<: Super ts) : cs) subs
    | Set.member t ts = solver cs subs
    | otherwise = subtypeError ctx t (Super ts)

solver (Context ctx (x :<<: s) : cs) subs
    | any (hasVarC x) cs =
        do cs6 <- concat `liftM` mapM (schemeSub x s) cs
           solver cs6 subs
    | otherwise =
        do (t,cs7) <- concretize s
           let cs'' = (cs ++ Context ctx (VarT x :=: t) : map (extendCtx ctx) cs7)
           solver cs'' subs


occursError ctx t1 t2 =
    return . Left $ "Type error: Occurs check: cannot construct the infinite type: " ++ show t1 ++
                    " = " ++ show t2 ++
                    " in context " ++ ctx
uniError ctx t1 t2 =
    return . Left $ "Type error: " ++ show t1 ++
                    " is not equal to " ++ show t2 ++
                    " in context " ++ ctx
unionError ctx ts ts' =
    return . Left $ concat [ "Type error: There are no types in both "
                           , show (Super ts), " and ", show (Super ts')
                           , " in context ", ctx ]
subtypeError ctx t s =
    return . Left $ concat [ "Type error: ", show t, " is not a ", show s
                           , " in context ", ctx ]

occursIn x t = x `elem` freeVars t