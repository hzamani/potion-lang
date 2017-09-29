{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Potion.Type.Infer where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity

import Data.Char (isAlpha)
import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Language.Potion.Syntax
import Language.Potion.Type
import Language.Potion.Type.Context
import qualified Language.Potion.Type.Context as Context
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
runInfer :: Context -> Infer (Expression, Apps, [Constraint]) -> Either InferError (Expression, Apps, [Constraint])
runInfer ctx m
  = runExcept $ evalStateT (runReaderT m ctx) InferState{ count = 0 }

-- | Solve for the toplevel type of an expression in a given context
inferExp :: Context -> Expression -> Either InferError (Expression, Scheme, Apps)
inferExp ctx expr
  = case runInfer ctx (infer expr) of
    Left err       -> Left err
    Right (expr, as, cs) ->
      case runSolve cs of
        Left err  -> Left err
        Right sub -> Right $ closeOver (apply sub expr, apply sub as)

-- | Canonicalize and return the polymorphic toplevel type
closeOver :: (Expression, Apps) -> (Expression, Scheme, Apps)
closeOver (ET exp ty, apps)
  = normalize (exp, generalize base ty, apps)

-- | Return the internal constraints used in solving for the type of an expression
constraintsExpr :: Context -> Expression -> Either InferError (Expression, Apps, [Constraint])
constraintsExpr ctx expr = runInfer ctx (infer expr)

-- | Extend type context
with :: (UserName, Scheme) -> Infer a -> Infer a
with (name, scheme)
  = local scope
  where
    scope ctx = remove ctx name `extend` (name, scheme)

withSchemes :: [(UserName, Scheme)] -> Infer a -> Infer a
withSchemes schemes
  = local scope
  where
    scope ctx = foldl' ext ctx schemes
    ext ctx (name, scheme) = remove ctx name `extend` (name, scheme)

lookupContext :: UserName -> Infer (Type, Apps)
lookupContext name
  = do
    (Context funs pacs) <- ask
    case Map.lookup name funs of
      Just (s, as, _) -> instantiate name s as
      Nothing         ->
        case Map.lookup name pacs of
          Just (_, _, s) -> instantiate name s noApp
          Nothing        -> throwError $ UnboundVariable name

-- TODO: optimaize name generation
letters :: [String]
letters = [1..] >>= flip replicateM ['α'..'ω']

fresh :: Infer Type
fresh
  = do
    state <- get
    put state{count = count state + 1}
    return $ TV $ TVar (letters !! count state)

instantiate :: UserName -> Scheme -> Apps -> Infer (Type, Apps)
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

normalize :: (Expression, Scheme, Apps) -> (Expression, Scheme, Apps)
normalize (expr, Forall vars ty, apps)
  = (expr', scheme, apps')
  where
    scheme = Forall (snd <$> cs) (norm ty)
    apps'  = Map.map (Set.map norm) apps
    expr'  = walk normExp expr

    fs = free ty
    cs = zip (Set.elems fs) (fmap TVar letters)

    normExp (ET e t) = ET e (norm t)
    normExp e        = e

    norm (TApp f as) = TApp (norm f) (map norm as)
    norm (TN a)      = TN a
    norm TUnknown    = TUnknown
    norm (TV a)      =
      case Prelude.lookup a cs of
        Just x  -> TV x
        Nothing -> error $ "type variable not in signature: " ++ show (a, cs)

infer :: Expression -> Infer (Expression, Apps, [Constraint])
infer e@(EL lit)
  = return (ET e $ litteralType lit, noApp, [])

infer e@EPlace
  = do
    tv <- fresh
    return (ET e tv, noApp, [])

infer e@(EN (UN name))
  | isTypeName name
  = return (ET e $ TN name, noApp, [])

infer e@(EN (UN name))
  = do
    (ty, as) <- lookupContext name
    return (ET e ty, as, [])

infer (EFun idents expr)
  = do
    vars <- mapM (const fresh) idents
    fun <- fresh
    let tIdents = zipWith ET idents vars
    let sIdents = map (\(ET ident ty) -> (toName ident, Forall [] ty)) tIdents
    let schemes = ("recur", Forall [] fun) : sIdents
    (tExpr, apps, c) <- withSchemes schemes (infer expr)
    let out = typeof tExpr
    let funTy = tFun (tTuple vars) out
    let typed = EFun tIdents tExpr
    return (ET typed funTy, apps, c ++ [(fun, funTy)])
  where
    toName (EN (UN n)) = n
    toName (EN (PN n _)) = n
    toName _ = "_"

infer (EApp (EN (UN "%let%")) [pat, val, expr])
  = do
    schemes      <- patternSchemes pat
    (tp, ap, cp) <- withSchemes schemes (infer pat)
    (tv, av, cv) <- withSchemes schemes (infer val)
    (te, ae, ce) <- withSchemes schemes (infer expr)
    let typed = EApp (en "%let%") [tp, tv, te]
    return (ET typed (typeof te), muns [ap, av, ae], concat [cp, cv, ce, [(typeof tp, typeof tv)]])

infer (EApp (EN (UN "%block%")) (x:xs))
  = do
    (e1, a1, c1) <- infer x
    (es@(e:_), as, cs) <- foldM inferStep ([e1], a1, c1) xs
    let typed = EApp (en "%block%") (reverse es)
    return (ET typed (typeof e), as, cs)
  where
    inferStep (es, a, c) expr
      = do
        (ei, ai, ci) <- infer expr
        return (ei:es, a `mun` ai, c ++ ci)

infer e@(EApp (EN (UN "[]")) [])
  = do
    ty <- fresh
    return (ET (eArrayT ty []) (tArray ty), noApp, [])

infer (EApp (EN (UN "[]")) (x:xs))
  = do
    (e1, a1, c1) <- infer x
    let ty = typeof e1
    (es, _, as, cs) <- foldM inferStep ([e1], ty, a1, c1) xs
    let typed = eArrayT ty (reverse es)
    return (ET typed (tArray ty), as, cs)
  where
    inferStep (es, ty, as, cs) exp
      = do
        (ei, ai, ci) <- infer exp
        return (ei:es, ty, ai `mun` as, ci ++ cs ++ [(ty, typeof ei)])

infer (EApp (EN (UN "()")) exps)
  = do
    (es, ts, as, cs) <- foldM inferStep ([], [], noApp, []) exps
    let ty = tTuple $ reverse ts
    let typed = eTuple $ reverse es
    return (ET typed ty, as, cs)
  where
    inferStep (es, ts, as, cs) exp
      = do
        (ei, ai, ci) <- infer exp
        return (ei:es, typeof ei:ts, as `mun` ai, cs ++ ci)

infer (EApp f args)
  = do
    (tf, af, cf)                    <- infer f
    (ET (EApp _ eIn) tIn, aIn, cIn) <- infer (eTuple args)
    tOut                            <- fresh
    let typed = EApp tf eIn
    return (ET typed tOut, af `mun` aIn, concat [cf, cIn, [(typeof tf, tFun tIn tOut)]])

infer (EMatch exp branches)
  = do
    (te, ae, ce)    <- infer exp
    tv              <- fresh
    (_, bs, as, cs) <- foldM inferStep ([typeof te, tv], [], ae, ce) branches
    let typed = EMatch te $ reverse bs
    return (ET typed tv, as, cs)
  where
    inferStep (tt@[tIn, tOut], bs, as, cs) (pat, when, exp)
      = do
        schemes      <- patternSchemes pat
        (tp, ap, cp) <- withSchemes schemes (infer pat)
        (tw, aw, cw) <- withSchemes schemes (infer when)
        (te, ae, ce) <- withSchemes schemes (infer exp)
        return (tt, (tp, tw, te):bs, muns [as, ap, aw, ae], concat [cs, cp, cw, ce, [(typeof tp, tIn), (typeof tw, TN "Bool"), (typeof te, tOut)]])

patternSchemes :: Expression -> Infer [(UserName, Scheme)]
patternSchemes pat
  = do
    let names = patNames pat
    vars <- mapM (const fresh) names
    return $ zipWith (\name tv -> (name, Forall [] tv)) names vars
  where
    patNames :: Expression -> [UserName]
    patNames EPlace           = []
    patNames (EL _)           = []
    patNames (EN (UN name))   = [name]
    patNames (EN (PN name _)) = [name]
    patNames (EApp _ xs)      = concatMap patNames xs

unifyDefs :: Context -> [Definition] -> [Definition]
unifyDefs ctx
  = concatMap unified
  where
    unified (name, exp)
      = map (unifyApps . unifyDef name exp) $ defConstraints $ un name

    unifyDef name exp (scheme, ty)
      = case runSolve [(scheme, ty)] of
          Left err  -> error (show err)
          Right sub -> (rename sub name, ET (apply sub exp) ty)

    unifyApps (name, exp) = (name, walk unifyApp exp)
    unifyApp e@(ET (EN (UN name@(h:_))) ty)
      | isAlpha h
      = case Context.lookup ctx name of
          Just (Forall _ ty', _, _) ->
            case runSolve [(ty', ty)] of
              Left err -> error (show err)
              Right sub -> ET (EN (rename sub (UN name))) ty
          Nothing -> e
    unifyApp exp = exp

    defConstraints name
      = case Context.lookup ctx name of
          Just (Forall [] ty, _, variants) ->
            map (\t -> (ty, t)) $ ty : Set.toList variants
          Just (Forall _ ty, _, variants) ->
            map (\t -> (ty, t)) $ Set.toList variants
          Nothing -> []

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

