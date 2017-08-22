{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Potion.Type.Infer where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity

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

letters :: [String]
letters = [1..] >>= flip replicateM ['α'..'ω']

fresh :: Infer Type
fresh
  = do
    state <- get
    put state{count = count state + 1}
    return $ TVar $ TV (letters !! count state)

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
    ord = zip (Set.elems $ free t) (fmap TV letters)

    norm (TList a)   = TList (norm a)
    norm (TFunc a b) = TFunc (norm a) (norm b)
    norm (TMap a b)  = TMap (norm a) (norm b)
    norm (TTuple ts) = TTuple (map norm ts)
    norm (TCon a)    = TCon a
    norm (TVar a)    =
      case Prelude.lookup a ord of
        Just x  -> TVar x
        Nothing -> error "type variable not in signature"

infer :: Expression -> Infer (Type, [Constraint])
infer (Lit lit)
  = return (litteralType lit, [])

infer (Var x)
  = do
    ty <- lookupContext x
    return (ty, [])

infer (Op op)
  = do
    ty <- lookupContext op
    return (ty, [])

infer (Func names expr)
  = do
    vars <- mapM (const fresh) names
    let schemes = zipWith (\name tv -> (name, Forall [] tv)) names vars
    (out, c) <- withSchemes schemes (infer expr)
    return (TFunc (TTuple vars) out, c)

infer (Tuple exprs)
  = do
    (ts, cs) <- foldM inferStep ([], []) exprs
    return (TTuple $ reverse ts, cs)
  where
    inferStep (ts, cs) expr
      = do
        (t, cs') <- infer expr
        return (t:ts, cs ++ cs')

infer (App f args)
  = do
    (tf, cf)   <- infer f
    (tIn, cIn) <- infer (Tuple args)
    tOut       <- fresh
    return (tOut, cf ++ cIn ++ [(tf, TFunc tIn tOut)])

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
unifies (TVar v) t = v `bind` t
unifies t (TVar v) = v `bind` t
unifies (TTuple [a]) (TCon b) = unifies a (TCon b)
unifies (TCon a) (TTuple [b]) = unifies (TCon a) b
unifies (TFunc in1 out1) (TFunc in2 out2) = unifyMany [in1, out1] [in2, out2]
unifies (TTuple as) (TTuple bs) = unifyMany as bs
unifies a b = throwError $ UnificationFail a b

bind ::  TVar -> Type -> Solve Substitution
bind a ty
  | ty == TVar a  = return Sb.empty
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
