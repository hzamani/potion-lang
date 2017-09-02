{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Potion.Type.Infer where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity

import Data.Char (isUpper)
import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Language.Potion.Syntax
import Language.Potion.Type
import Language.Potion.Type.Context
import Language.Potion.Type.Substitution
import qualified Language.Potion.Type.Substitution as Sb

-- | Inference monad
type Infer a
  = (ReaderT
      Context                -- typing context
      (StateT
        InferState           -- inference state
        (Except InferError)) -- inference errors
      a                      -- result
    )

newtype InferState
  = InferState { count :: Int }

data InferError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable String
  | Ambigious [Constraint]
  | UnificationMismatch [Type] [Type]
  deriving Show

type Constraint
  = (Type, Type)

instance Substitutable Constraint where
   apply sub (a, b) = (apply sub a, apply sub b)
   free (a, b) = free a `Set.union` free b

type Unifier
  = (Substitution, [Constraint])

type Solve a
  = ExceptT InferError Identity a

-- | Run the inference monad
runInfer :: Context -> Infer (Type, [Constraint]) -> Either InferError (Type, [Constraint])
runInfer ctx m
  = runExcept $ evalStateT (runReaderT m ctx) InferState{ count = 0 }

-- | Solve for the toplevel type of an expression in a given context
inferExpr :: Context -> Expression -> Either InferError Scheme
inferExpr ctx expr
  = case runInfer ctx (infer expr) of
    Left err       -> Left err
    Right (ty, cs) ->
      case runSolve cs of
        Left err  -> Left err
        Right sub -> Right $ closeOver $ apply sub ty

-- | Canonicalize and return the polymorphic toplevel type
closeOver :: Type -> Scheme
closeOver = normalize . generalize base

-- | Return the internal constraints used in solving for the type of an expression
constraintsExpr :: Context -> Expression -> Either InferError (Type, [Constraint])
constraintsExpr ctx expr = runInfer ctx (infer expr)

-- | Extend type context
with :: (Name, Scheme) -> Infer a -> Infer a
with (name, scheme)
  = local scope
  where
    scope ctx = remove ctx name `extend` (name, scheme)

withSchemes :: [(Name, Scheme)] -> Infer a -> Infer a
withSchemes schemes
  = local scope
  where
    scope ctx = foldl' ext ctx schemes
    ext ctx (name, scheme) = remove ctx name `extend` (name, scheme)

lookupContext :: Name -> Infer Type
lookupContext name
  = do
    (Context schemes) <- ask
    case Map.lookup name schemes of
      Nothing     -> throwError $ UnboundVariable name
      Just scheme -> instantiate scheme

-- TODO: optimaize name generation
letters :: [String]
letters = [1..] >>= flip replicateM ['α'..'ω']

fresh :: Infer Type
fresh
  = do
    state <- get
    put state{count = count state + 1}
    return $ TV $ TVar (letters !! count state)

instantiate :: Scheme -> Infer Type
instantiate (Forall vars ty)
  = do
    vars' <- mapM (const fresh) vars
    let sub = Sub $ Map.fromList $ zip vars vars'
    return $ apply sub ty

generalize :: Context -> Type -> Scheme
generalize ctx t
  = Forall vars t
  where
    vars = Set.toList $ free t `Set.difference` free ctx

normalize :: Scheme -> Scheme
normalize (Forall vars t)
  = Forall (fmap snd ord) (norm t)
  where
    ord = zip (Set.elems $ free t) (fmap TVar letters)

    norm (TApp f as) = TApp (norm f) (map norm as)
    norm (TN a)      = TN a
    norm (TV a)      =
      case Prelude.lookup a ord of
        Just x  -> TV x
        Nothing -> error "type variable not in signature"

infer :: Expression -> Infer (Type, [Constraint])
infer (EL lit)
  = return (litteralType lit, [])

infer EPlace
  = do
    tv <- fresh
    return (tv, [])

infer (EN x)
  = if isType x
    then return (TN x, [])
    else look x
  where
    look x = do
      ty <- lookupContext x
      return (ty, [])

    isType (x:_) = isUpper x
    isType _ = False

infer (EFun idents expr)
  = do
    vars <- mapM (const fresh) idents
    let schemes = zipWith (\ident tv -> (toName ident, Forall [] tv)) idents vars
    (out, c) <- withSchemes schemes (infer expr)
    return (tFun (tTuple vars) out, c)
  where
    toName (EN n) = n
    toName _ = "_"

infer (ELet pat val expr)
  = do
    let names = patNames pat
    vars <- mapM (const fresh) names
    let schemes = zipWith (\name var -> (name, Forall [] var)) names vars
    (tp, cp) <- withSchemes schemes (infer pat)
    (tv, cv) <- withSchemes schemes (infer val)
    (te, ce) <- withSchemes schemes (infer expr)
    return (te, cp ++ cv ++ ce ++ [(tp, tv)])

infer (EApp (EN "do") (x:xs))
  = do
    (t1, c1) <- infer x
    foldM inferStep (t1, c1) xs
  where
    inferStep (t, c) expr
      = do
        (ti, ci) <- infer expr
        return (ti, c ++ ci)

infer (EApp (EN "[]") [])
  = do
    ty <- fresh
    return (tList ty, [])

infer (EApp (EN "[]") (x:xs))
  = do
    (ty, c) <- infer x
    (_, cs) <- foldM inferStep (ty, c) xs
    return (tList ty, cs)
  where
    inferStep (t, c) expr
      = do
        (ty, cs) <- infer expr
        return (t, c ++ cs ++ [(ty, t)])

infer (EApp (EN "()") exprs)
  = do
    (ts, cs) <- foldM inferStep ([], []) exprs
    return (tTuple $ reverse ts, cs)
  where
    inferStep (ts, cs) expr
      = do
        (t, cs') <- infer expr
        return (t:ts, cs ++ cs')

infer (EApp f args)
  = do
    (tf, cf)   <- infer f
    (tIn, cIn) <- infer (tuple args)
    tOut       <- fresh
    return (tOut, cf ++ cIn ++ [(tf, tFun tIn tOut)])

infer (EMatch expr branches)
  = do
    (te, ce)     <- infer expr
    tv           <- fresh
    (_, cOut) <- foldM inferStep (tTuple [te, tv], ce) branches
    return (tv, cOut)
  where
    inferStep (tt@(TApp (TN "Tuple") [tIn, tOut]), cs) (pat, when, expr)
      = do
        let names = patNames pat
        vars <- mapM (const fresh) names
        let schemes = zipWith (\name tv -> (name, Forall [] tv)) names vars
        (tp, cp) <- withSchemes schemes (infer pat)
        (tw, cw) <- withSchemes schemes (infer when)
        (te, ce) <- withSchemes schemes (infer expr)
        return (tt, cs ++ cp ++ ce ++ [(tp, tIn), (tw, TN "Bool"), (te, tOut)])

tuple = EApp (EN "()")

patNames :: Expression -> [Name]
patNames EPlace = []
patNames (EL _) = []
patNames (EN name) = [name]
patNames (EApp _ xs) = concatMap patNames xs

inferDecl :: Context -> [(Name, Expression)] -> Either InferError Context
inferDecl ctx []
  = Right ctx
inferDecl ctx ((name, expr):rest)
  = case inferExpr ctx expr of
    Left err -> Left err
    Right ty -> inferDecl (extend ctx (name, ty)) rest

-- | Run the constraint solver
runSolve :: [Constraint] -> Either InferError Substitution
runSolve cs
  = runIdentity $ runExceptT $ solver st
  where st = (Sb.empty, cs)

unifyMany :: [Type] -> [Type] -> Solve Substitution
unifyMany as bs
  | length as == length bs = foldM unifyStep Sb.empty (zip as bs)
  where
    unifyStep :: Substitution -> (Type, Type) -> Solve Substitution
    unifyStep sub (a, b) =
      do
        sub' <- unifies (apply sub a) (apply sub b)
        return $ sub' `compose` sub
unifyMany as bs = throwError $ UnificationMismatch as bs

unifies :: Type -> Type -> Solve Substitution
unifies t1 t2 | t1 == t2 = return Sb.empty
-- unifies (TN a) (TN b) = autoConvert a b -- TODO: add auto convertion
unifies (TV v) t = v `bind` t
unifies t (TV v) = v `bind` t
unifies (TApp (TN "Tuple") [a]) (TN b) = unifies a (TN b)
unifies (TN a) (TApp (TN "Tuple") [b]) = unifies (TN a) b
unifies (TApp f as) (TApp f' as') = unifyMany (f:as) (f':as')
unifies a b = throwError $ UnificationFail a b

bind ::  TVar -> Type -> Solve Substitution
bind a ty
  | ty == TV a    = return Sb.empty
  | occursIn a ty = throwError $ InfiniteType a ty
  | otherwise     = return (Sub $ Map.singleton a ty)

-- Unification solver
solver :: Unifier -> Solve Substitution
solver (subs, cs) =
  case cs of
    [] -> return subs
    ((a, b):rest) -> do
      sub <- unifies a b
      solver (sub `compose` subs, apply sub rest)
