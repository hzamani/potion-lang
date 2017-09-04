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
runInfer :: Context -> Infer (Type, Apps, [Constraint]) -> Either InferError (Type, Apps, [Constraint])
runInfer ctx m
  = runExcept $ evalStateT (runReaderT m ctx) InferState{ count = 0 }

-- | Solve for the toplevel type of an expression in a given context
inferExpr :: Context -> Expression -> Either InferError (Scheme, Apps)
inferExpr ctx expr
  = case runInfer ctx (infer expr) of
    Left err       -> Left err
    Right (ty, as, cs) ->
      case runSolve cs of
        Left err  -> Left err
        Right sub -> Right (closeOver $ apply sub ty, apply sub as)

-- | Canonicalize and return the polymorphic toplevel type
closeOver :: Type -> Scheme
closeOver = {- normalize . -} generalize base

-- | Return the internal constraints used in solving for the type of an expression
constraintsExpr :: Context -> Expression -> Either InferError (Type, Apps, [Constraint])
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

lookupContext :: Name -> Infer (Type, Apps)
lookupContext name
  = do
    (Context funs) <- ask
    case Map.lookup name funs of
      Nothing         -> throwError $ UnboundVariable name
      Just (s, as, _) -> instantiate name s as

-- TODO: optimaize name generation
letters :: [String]
letters = [1..] >>= flip replicateM ['α'..'ω']

fresh :: Infer Type
fresh
  = do
    state <- get
    put state{count = count state + 1}
    return $ TV $ TVar (letters !! count state)

instantiate :: Name -> Scheme -> Apps -> Infer (Type, Apps)
instantiate name scheme@(Forall [] ty) apps = return (ty, apps)
instantiate name scheme@(Forall vars ty) apps
  = do
    vars' <- mapM (const fresh) vars
    let sub = Sub $ Map.fromList $ zip vars vars'
    let apps' = insertApp name ty apps
    return (apply sub ty, apply sub apps')

generalize :: Context -> Type -> Scheme
generalize ctx ty
  = Forall vars ty
  where
    vars = Set.toList $ free ty `Set.difference` free ctx

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

noApp :: Apps
noApp = Map.empty

infer :: Expression -> Infer (Type, Apps, [Constraint])
infer (EL lit)
  = return (litteralType lit, noApp, [])

infer EPlace
  = do
    tv <- fresh
    return (tv, noApp, [])

infer (EN x)
  = if isType x
      then return (TN x, noApp, [])
      else look x
  where
    look x = do
      (ty, as) <- lookupContext x
      return (ty, as, [])

    isType (x:_) = isUpper x
    isType _ = False

infer (EFun idents expr)
  = do
    vars <- mapM (const fresh) idents
    let schemes = zipWith (\ident tv -> (toName ident, Forall [] tv)) idents vars
    (out, apps, c) <- withSchemes schemes (infer expr)
    return (tFun (tTuple vars) out, apps, c)
  where
    toName (EN n) = n
    toName _ = "_"

infer (ELet pat val expr)
  = do
    let names = patNames pat
    vars <- mapM (const fresh) names
    let schemes = zipWith (\name var -> (name, Forall [] var)) names vars
    (tp, ap, cp) <- withSchemes schemes (infer pat)
    (tv, av, cv) <- withSchemes schemes (infer val)
    (te, ae, ce) <- withSchemes schemes (infer expr)
    return (te, muns [ap, av, ae], cp ++ cv ++ ce ++ [(tp, tv)])

infer (EApp (EN "do") (x:xs))
  = do
    (t1, a1, c1) <- infer x
    foldM inferStep (t1, a1, c1) xs
  where
    inferStep (t, a, c) expr
      = do
        (ti, ai, ci) <- infer expr
        return (ti, a `mun` ai, c ++ ci)

infer (EApp (EN "[]") [])
  = do
    ty <- fresh
    return (tList ty, noApp, [])

infer (EApp (EN "[]") (x:xs))
  = do
    (t1, a1, c1) <- infer x
    (_, a, cs) <- foldM inferStep (t1, a1, c1) xs
    return (tList t1, a, cs)
  where
    inferStep (t, a, c) expr
      = do
        (ty, as, cs) <- infer expr
        return (t, a `mun` as, c ++ cs ++ [(ty, t)])

infer (EApp (EN "()") exprs)
  = do
    (ts, as, cs) <- foldM inferStep ([], noApp, []) exprs
    return (tTuple $ reverse ts, as, cs)
  where
    inferStep (ts, as, cs) expr
      = do
        (t, as', cs') <- infer expr
        return (t:ts, as `mun` as', cs ++ cs')

infer (EApp f args)
  = do
    (tf, af, cf)    <- infer f
    (tIn, aIn, cIn) <- infer (tuple args)
    tOut            <- fresh
    -- let app = case f of
    --             (EN name) -> singletonApp name tf
    --             _ -> af
    return (tOut, af `mun` aIn, cf ++ cIn ++ [(tf, tFun tIn tOut)])

infer (EMatch expr branches)
  = do
    (te, ae, ce) <- infer expr
    tv           <- fresh
    (_, as, cs)  <- foldM inferStep ([te, tv], ae, ce) branches
    return (tv, as, cs)
  where
    inferStep (tt@[tIn, tOut], as, cs) (pat, when, expr)
      = do
        let names = patNames pat
        vars <- mapM (const fresh) names
        let schemes = zipWith (\name tv -> (name, Forall [] tv)) names vars
        (tp, ap, cp) <- withSchemes schemes (infer pat)
        (tw, aw, cw) <- withSchemes schemes (infer when)
        (te, ae, ce) <- withSchemes schemes (infer expr)
        return (tt, muns [ap, aw, ae], cs ++ cp ++ ce ++ [(tp, tIn), (tw, TN "Bool"), (te, tOut)])

tuple = EApp (EN "()")

patNames :: Expression -> [Name]
patNames EPlace      = []
patNames (EL _)      = []
patNames (EN name)   = [name]
patNames (EApp _ xs) = concatMap patNames xs

inferDecl :: Context -> [(Name, Expression)] -> Either InferError Context
inferDecl ctx []
  = Right ctx
inferDecl ctx ((name, expr):rest)
  = case inferExpr ctx expr of
    Left err         -> Left err
    Right (ty, apps) -> inferDecl (extendApps ctx (name, ty, apps)) rest

extendApps :: Context -> (Name, Scheme, Apps) -> Context
extendApps (Context funs) (name, scheme, apps)
  = Context $ Map.insert name (scheme, apps, Set.empty) funsWithVariants
  where
    funsWithVariants = Map.foldlWithKey insertVariants funs apps

    insertVariants :: Map Name Fun -> Name -> Set Type -> Map Name Fun
    insertVariants funs name ty = Map.adjust (insertVariant ty) name funs

    insertVariant :: Set Type -> Fun -> Fun
    insertVariant ty (sc, as, vs) = (sc, as, Set.filter noFree ty `Set.union` vs)

    noFree = (0 ==) . Set.size . free

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

