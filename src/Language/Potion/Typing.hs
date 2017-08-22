{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Potion.Typing where

import Control.Monad.State
import Control.Monad.Except

import Data.List
import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set

import Language.Potion.Syntax

newtype TVar
  = TV Name
  deriving (Eq, Ord)

instance Show TVar where
  show (TV name) = name

data Type
  = TVar TVar
  | TCon Name
  | TList Type
  | TMap Type Type
  | TTuple [Type]
  | TFunc Type Type
  deriving Eq

instance Show Type where
  show (TVar var) = show var
  show (TCon name) = name
  show (TList a) = "List(" ++ show a ++ ")"
  show (TMap a b) = "Map(" ++ show a ++ ", " ++ show b ++ ")"
  show (TTuple ts) = "(" ++ intercalate ", " (map show ts) ++ ")"
  show (TFunc a b) = show a ++ " -> " ++ show b

data Scheme
  = Forall [TVar] Type
  deriving Eq

instance Show Scheme where
  show (Forall [] ty) = show ty
  show (Forall vars ty) = "Ɐ" ++ intercalate "," (map show vars) ++ ". " ++ show ty

newtype Context
  = Context (Map Name Scheme)
  deriving (Show, Monoid)

emptyContext :: Context
emptyContext = Context $ Map.fromList
  [ ("+", Forall [av] (TFunc aa a))
  , ("-", Forall [av] (TFunc aa a))
  , ("*", Forall [av] (TFunc aa a))
  , ("/", Forall [av] (TFunc aa a))
  , ("%", Forall [av] (TFunc aa a))
  , ("#", Forall [av, bv] (TFunc a b))
  , ("x", Forall [] b)
  , ("y", Forall [] c)
  ]
  where
    aa = TTuple [a, a]
    a = TVar av
    b = TVar bv
    c = TVar cv
    av = TV "a"
    bv = TV "b"
    cv = TV "c"

extend :: Context -> (Name, Scheme) -> Context
extend (Context ctx) (name, scheme) = Context $ Map.insert name scheme ctx

typeof :: Context -> Name -> Maybe Scheme
typeof (Context ctx) name = Map.lookup name ctx

type Substitution = Map TVar Type

emptySub :: Substitution
emptySub = Map.empty

compose :: Substitution -> Substitution -> Substitution
compose s1 s2 = Map.map (apply s1) (Map.union s2 s1)

class Substitutable a where
  apply :: Substitution -> a -> a
  free :: a -> Set TVar

instance Substitutable Type where
  apply sub ty@(TCon _) = ty
  apply sub ty@(TVar a) = Map.findWithDefault ty a sub
  apply sub (TList a) = TList (apply sub a)
  apply sub (TMap a b) = TMap (apply sub a) (apply sub b)
  apply sub (TTuple ts) = TTuple (map (apply sub) ts)
  apply sub (TFunc a b) = TFunc (apply sub a) (apply sub b)

  free (TCon _) = Set.empty
  free (TVar a) = Set.singleton a
  free (TList a) = free a
  free (TMap a b) = Set.union (free a) (free b)
  free (TTuple ts) = Set.unions (map free ts)
  free (TFunc a b) = Set.union (free a) (free b)

instance Substitutable Scheme where
  apply sub (Forall vars t) = Forall vars $ apply sub' t
    where
      sub' = foldr Map.delete sub vars

  free (Forall vars t) = Set.difference (free t) (Set.fromList vars)

instance Substitutable Context where
  apply sub (Context ctx) = Context $ Map.map (apply sub) ctx
  free (Context ctx) = free $ Map.elems ctx

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  free = foldr (Set.union . free) Set.empty

occursIn :: Substitutable a => TVar -> a -> Bool
occursIn var ty = var `Set.member` free ty

data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable String
  deriving (Eq, Show)

newtype Unique
  = Unique { count :: Int }

type Infer
  = ExceptT TypeError (State Unique)

runInfer :: Infer (Substitution, Type) -> Either TypeError Scheme
runInfer m
  = case evalState (runExceptT m) unique of
      Left err  -> Left err
      Right res -> Right $ closeOver res
  where
    unique = Unique { count = 0 }

    closeOver :: (Map TVar Type, Type) -> Scheme
    closeOver (subs, t) = normalize $ generalize emptyContext $ apply subs t

letters :: [String]
letters = [1..] >>= flip replicateM ['α'..'ω']

normalize :: Scheme -> Scheme
normalize (Forall vars t)
  = Forall (fmap snd ord) (norm t)
  where
    ord = zip (Set.elems $ free t) (fmap TV letters)

    norm (TList a) = TList (norm a)
    norm (TFunc a b) = TFunc (norm a) (norm b)
    norm (TMap a b) = TMap (norm a) (norm b)
    norm (TTuple ts) = TTuple (map norm ts)
    norm (TCon a) = TCon a
    norm (TVar a) =
      case lookup a ord of
        Just x  -> TVar x
        Nothing -> error "type variable not in signature"

generalize :: Context -> Type -> Scheme
generalize ctx t
  = Forall vars t
  where
    vars = Set.toList $ free t `Set.difference` free ctx

bind :: TVar -> Type -> Infer Substitution
bind x ty
  | ty == TVar x  = return emptySub
  | occursIn x ty = throwError $ InfiniteType x ty
  | otherwise     = return $ Map.singleton x ty

unify :: Type -> Type -> Infer Substitution
unify (TVar x) ty
  = bind x ty
unify ty (TVar x)
  = bind x ty
unify (TCon a) (TCon b)
  | a == b = return emptySub
unify (TTuple [a]) (TCon b)
  = unify a (TCon b)
unify (TCon a) (TTuple [b])
  = unify (TCon a) b
unify (TTuple as) (TTuple bs)
  | length as == length bs = foldM unifyStep emptySub (zip as bs)
  where
    unifyStep :: Substitution -> (Type, Type) -> Infer Substitution
    unifyStep sub (a, b) =
      do
        sub' <- unify (apply sub a) (apply sub b)
        return $ sub' `compose` sub
unify (TFunc a b) (TFunc c d)
  = do
    s1 <- unify a c
    s2 <- unify (apply s1 b) (apply s1 d)
    return $ compose s1 s2
unify a b
  = throwError $ UnificationFail a b

infer :: Context -> Expression -> Infer (Substitution, Type)
infer ctx (Lit lit)
  = return (emptySub, litteralType lit)
infer ctx (Var x)
  = lookupT ctx x
infer ctx (Op op)
  = lookupT ctx op
infer ctx (Func [name] expr)
  = do
    var <- fresh
    let ctx' = extend ctx (name, Forall [] var)
    (sub, t) <- infer ctx' expr
    return (sub, apply sub $ TFunc var t)
infer ctx (Func names expr)
  = do
    vars <- mapM (const fresh) names
    let schemes = zipWith (\name tv -> (name, Forall [] tv)) names vars
    let ctx' = foldl extend ctx schemes
    (sub, t) <- infer ctx' expr
    return (sub, apply sub $ TFunc (TTuple vars) t)
infer ctx (Tuple exprs)
  = do
    (sub, ts) <- foldM inferStep (emptySub, []) exprs
    return (sub, apply sub $ TTuple $ reverse ts)
  where
    inferStep (sub, ts) expr
      = do
        (sub', t) <- infer (apply sub ctx) expr
        return (sub' `compose` sub, t:ts)
infer ctx (App f args)
  = do
    tOut       <- fresh
    (sf, tf)   <- infer ctx f
    (sIn, tIn) <- infer (apply sf ctx) (Tuple args)
    sOut       <- unify (apply sIn tf) (TFunc tIn tOut)
    return (sOut `compose` sIn `compose` sf, apply sOut tOut)

lookupT :: Context -> Name -> Infer (Substitution, Type)
lookupT (Context ctx) x
  = case Map.lookup x ctx of
    Nothing ->
      throwError $ UnboundVariable (show x)
    Just scheme ->
      do
        ty <- instantiate scheme
        return (emptySub, ty)

instantiate :: Scheme -> Infer Type
instantiate (Forall vars ty)
  = do
    vars' <- mapM (const fresh) vars
    let subs = Map.fromList $ zip vars vars'
    return $ apply subs ty

fresh :: Infer Type
fresh
  = do
    state <- get
    put state{count = count state + 1}
    return $ TVar $ TV (letters !! count state)

litteralType :: Literal -> Type
litteralType (LB _) = TCon "Bool"
litteralType (LI _) = TCon "Int"
litteralType (LF _) = TCon "Float"
litteralType (LC _) = TCon "Char"
litteralType (LS _) = TCon "String"

inferDecl :: Context -> [(Name, Expression)] -> Either TypeError Context
inferDecl ctx []
  = Right ctx
inferDecl ctx ((name, expr):rest)
  = case inferExpr ctx expr of
    Left err -> Left err
    Right ty -> inferDecl (extend ctx (name, ty)) rest
  where
    inferExpr ctx = runInfer . infer ctx

