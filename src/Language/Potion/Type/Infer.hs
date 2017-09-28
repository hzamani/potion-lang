{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Potion.Type.Infer where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity

import Data.Either.Combinators (mapRight)
import Data.Char (isUpper)
import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Language.Potion.Expand
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
inferExpr :: Context -> Expression -> Either InferError (Expression, Scheme, Apps)
inferExpr ctx expr
  = case runInfer ctx (infer expr) of
    Left err       -> Left err
    Right (expr, as, cs) ->
      case runSolve cs of
        Left err  -> Left err
        Right sub -> Right $ closeOver (apply sub expr, apply sub as)
        -- Right sub -> Right (closeOver $ apply sub ty, apply sub as)

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
    scheme = Forall (fmap snd cs) (norm ty)
    apps' = Map.map (Set.map norm) apps
    expr' = normExp expr

    fs = free ty
    cs = zip (Set.elems fs) (fmap TVar letters)

    normExp (ET e t) = ET (normExp e) (norm t)
    normExp e = e

    norm (TApp f as) = TApp (norm f) (map norm as)
    norm (TN a)      = TN a
    norm (TV a)      =
      case Prelude.lookup a cs of
        Just x  -> TV x
        Nothing -> error "type variable not in signature"

noApp :: Apps
noApp = Map.empty

isType :: String -> Bool
isType (x:_) = isUpper x
isType _ = False

infer :: Expression -> Infer (Expression, Apps, [Constraint])
infer e@(EL lit)
  = return (ET e $ litteralType lit, noApp, [])

infer e@EPlace
  = do
    tv <- fresh
    return (ET e tv, noApp, [])

infer e@(EN (UN name))
  | isType name
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
    (t1, a1, c1) <- infer x
    (ts@(t:_), as, cs) <- foldM inferStep ([t1], a1, c1) xs
    let typed = EApp (en "%block%") (reverse ts)
    return (ET typed (typeof t), as, cs)
  where
    inferStep (ts, a, c) expr
      = do
        (ti, ai, ci) <- infer expr
        return (ti:ts, a `mun` ai, c ++ ci)

-- infer (EApp (EN "[]") [])
--   = do
--     ty <- fresh
--     return (tList ty, noApp, [])

-- infer (EApp (EN "[]") (x:xs))
--   = do
--     (t1, a1, c1) <- infer x
--     (_, a, cs) <- foldM inferStep (t1, a1, c1) xs
--     return (tList t1, a, cs)
--   where
--     inferStep (t, a, c) expr
--       = do
--         (ty, as, cs) <- infer expr
--         return (t, a `mun` as, c ++ cs ++ [(ty, t)])

infer (EApp (EN (UN "()")) exprs)
  = do
    (es, ts, as, cs) <- foldM inferStep ([], [], noApp, []) exprs
    let ty = tTuple $ reverse ts
    let typed = EApp (en "()") (reverse es)
    return (ET typed ty, as, cs)
  where
    inferStep (es, ts, as, cs) expr
      = do
        (e, as', cs') <- infer expr
        return (e:es, typeof e:ts, as `mun` as', cs ++ cs')

infer (EApp f args)
  = do
    (tf, af, cf)                    <- infer f
    (ET (EApp _ eIn) tIn, aIn, cIn) <- infer (tuple args)
    tOut                            <- fresh
    let typed = EApp tf eIn
    return (ET typed tOut, af `mun` aIn, concat [cf, cIn, [(typeof tf, tFun tIn tOut)]])

infer (EMatch expr branches)
  = do
    (te, ae, ce) <- infer expr
    tv           <- fresh
    (_, bs, as, cs)  <- foldM inferStep ([typeof te, tv], [], ae, ce) branches
    let typed = EMatch te $ reverse bs
    return (ET typed tv, as, cs)
  where
    inferStep (tt@[tIn, tOut], bs, as, cs) (pat, when, expr)
      = do
        schemes      <- patternSchemes pat
        (tp, ap, cp) <- withSchemes schemes (infer pat)
        (tw, aw, cw) <- withSchemes schemes (infer when)
        (te, ae, ce) <- withSchemes schemes (infer expr)
        return (tt, (tp, tw, te):bs, muns [as, ap, aw, ae], concat [cs, cp, cw, ce, [(typeof tp, tIn), (typeof tw, TN "Bool"), (typeof te, tOut)]])

tuple = EApp (en "()")

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

inferDecl :: Context -> [Declaration] -> Either InferError (Context, [Definition])
inferDecl ctx
  = mapRight finalize . foldl step (Right (ctx, []))
  where
    finalize (ctx, defs) = (ctx, unifyDefs ctx $ reverse defs)
    step (Right (ctx, defs)) (DDef name params body)
      = case inferExpr ctx $ expand $ EFun params body of
          Left err -> Left err
          Right (expr, scheme, apps) -> Right (extendApps ctx (un name, scheme, apps), (name, expr):defs)
    step left _ = left

unifyDefs :: Context -> [Definition] -> [Definition]
unifyDefs ctx
  = concatMap unified
  where
    unified (name, exp)
      = map (unifyDef name $ unifyApps exp) $ defConstraints $ un name

    unifyDef name exp (scheme, ty)
      = case runSolve [(scheme, ty)] of
          Left err  -> error (show err)
          Right sub -> (rename sub name, ET (apply sub exp) ty)

    unifyApps = walk unifyApp
    unifyApp e@(ET (EN (UN name)) ty)
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

extendApps :: Context -> (UserName, Scheme, Apps) -> Context
extendApps (Context funs) (name, scheme, apps)
  = Context $ Map.insert name (scheme, apps, Set.empty) funsWithVariants
  where
    funsWithVariants = Map.foldlWithKey insertVariants funs apps

    insertVariants :: Map UserName Fun -> UserName -> Set Type -> Map UserName Fun
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

