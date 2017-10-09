module Language.Potion.Type.Infer
  ( inferExp
  ) where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity

import Data.Char (isAlpha)
import Data.Either.Combinators (mapRight)
import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Language.Potion.Core
import Language.Potion.Syntax hiding (putPos)
import Language.Potion.Type
import Language.Potion.Type.Context
import Language.Potion.Type.Substitution

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
  = InferState { varCount :: Int }

data InferError
  = UnificationFail Pos Type Type
  | InfiniteType Pos TVar Type
  | UnboundVariable Pos String
  | ExpectingVariable Expression
  | Ambigious [Constraint]
  | UnificationMismatch Pos [Type] [Type]
  deriving Show

data Constraint
  = CT Pos Type Type -- Position Expected Actual
  deriving (Eq, Show)

instance Substitutable Constraint where
  apply sub (CT p a b) = CT p (apply sub a) (apply sub b)
  free (CT p a b) = free a `Set.union` free b

type Unifier
  = (Substitution, [Constraint])

type Solve a
  = ExceptT InferError Identity a

inferExp :: Expression -> Context -> Either InferError (CExp, Scheme)
inferExp exp ctx
  = mapRight (canonicalize ctx) $ solve $ runInfer ctx $ infer exp

runInfer :: Context -> Infer (CExp, [Constraint]) -> Either InferError (CExp, [Constraint])
runInfer ctx m
  = runExcept $ evalStateT (runReaderT m ctx) InferState{ varCount = 0 }

infer :: Expression -> Infer (CExp, [Constraint])
infer (EHole p)
  = do
    t <- fresh
    let meta = noMeta{ mPos = p, mType = t }
    return (CHole meta, [])

infer (ELit p lit)
  = do
    let meta = noMeta{ mPos = p, mType = lType lit }
    return (CLit meta lit, [])

infer (EName p name)
  | isTypeName name
  = do
    let meta = noMeta{ mPos = p, mType = TN name }
    return (CName meta name, [])

infer (EName p name)
  = do
    m <- lookupContext name
    case m of
      Just meta ->
        return (CName (putPos p meta) name, [])
      Nothing ->
        throwError $ UnboundVariable p name

infer (EFun p params exp)
  = do
    fun <- fresh
    vars <- mapM inferVar params
    (body, cs) <- withVars (recur p fun : vars) (infer exp)
    let ty = tFun (tTuple $ map cType vars) (cType body)
    let meta = noMeta{ mPos = p, mType = fun, mApps = cApps body }
    return (CFun meta vars body, cs ++ [CT p ty fun])

infer (EApp p f args)
  = do
    (cF, fCs) <- infer f
    (cArgs, argsCs) <- inferMany args
    tOut <- fresh
    let tIn = tTuple $ map cType cArgs
    let meta = noMeta{ mPos = p, mType = tOut, mApps = unionApps $ cF : cArgs }
    return (CApp meta cF cArgs, fCs ++ argsCs ++ [CT p (tFun tIn tOut) (cType cF)])

inferMany :: [Expression] -> Infer ([CExp], [Constraint])
inferMany
  = foldM inferStep ([], [])
  where
    inferStep (cexps, cs) exp
      = do
        (cexp, ci) <- infer exp
        return (cexp:cexps, cs ++ ci)

inferVar :: Expression -> Infer CExp
inferVar (EName p name)
  = do
    t <- fresh
    let meta = noMeta{ mPos = p, mType = t }
    return $ CName meta name
inferVar (EHole p)
  = do
    t <- fresh
    let meta = noMeta{ mPos = p, mType = t }
    return $ CHole meta
inferVar exp
  = throwError $ ExpectingVariable exp

recur :: Pos -> Type -> CExp
recur pos ty
  = CName noMeta{ mPos = pos, mType = ty } "recur"

withVars :: [CExp] -> Infer a -> Infer a
withVars vars
  = local scope
  where
    scope ctx = foldl' ext ctx vars
    ext ctx (CName meta name) = (name, Forall [] (mType meta)) `extend` ctx
    ext ctx _ = ctx

lookupContext :: Name -> Infer (Maybe Meta)
lookupContext name
  = do
    ctx <- ask
    case Map.lookup name ctx of
      Just Info{ scheme = s@(Forall _ t), apps = as } -> do
        sub <- instantiator s
        return $ Just noMeta{ mType = apply sub t, mApps = apply sub as }
      Nothing ->
        return Nothing

instantiator :: Scheme -> Infer Substitution
instantiator (Forall [] ty)
  = return mempty
instantiator (Forall vars ty)
  = do
    vars' <- mapM (const fresh) vars
    return $ Sub $ Map.fromList $ zip vars vars'

canonicalize :: Context -> CExp -> (CExp, Scheme)
canonicalize ctx exp
  = normalize (exp, generalize ctx $ cType exp)

normalize :: (CExp, Scheme) -> (CExp, Scheme)
normalize (exp, Forall vars ty)
  = (apply sub exp, scheme)
  where
    scheme = Forall (snd <$> cs) (apply sub ty)
    fs = free ty
    cs = zip (Set.elems fs) (fmap TVar letters)
    sub = Sub $ Map.map TV $ Map.fromList cs

generalize :: Context -> Type -> Scheme
generalize ctx ty
  = Forall vars ty
  where
    vars = Set.toList $ free ty `Set.difference` free ctx

-- TODO: optimaize name generation
letters :: [String]
letters = [1..] >>= flip replicateM ['α'..'ω']

fresh :: Infer Type
fresh
  = do
    state <- get
    put state{varCount = varCount state + 1}
    return $ TV $ TVar (letters !! varCount state)

solve :: Either InferError (CExp, [Constraint]) -> Either InferError CExp
solve (Left e) = Left e
solve (Right (exp, cs))
  = case runSolve cs of
      Left e    -> Left e
      Right sub -> Right $ apply sub exp

runSolve :: [Constraint] -> Either InferError Substitution
runSolve cs
  = runIdentity $ runExceptT $ solver (mempty, cs)

solver :: Unifier -> Solve Substitution
solver (subs, [])
  = return subs
solver (subs, CT p a b : rest)
  = do
    sub <- unifies p a b
    solver (sub `compose` subs, apply sub rest)

unifyMany :: Pos -> [Type] -> [Type] -> Solve Substitution
unifyMany p as bs
  | length as == length bs = foldM unifyStep mempty (zip as bs)
  where
    unifyStep :: Substitution -> (Type, Type) -> Solve Substitution
    unifyStep sub (a, b)
      = do
        sub' <- unifies p (apply sub a) (apply sub b)
        return $ sub' `compose` sub
unifyMany p as bs
  = throwError $ UnificationMismatch p as bs

unifies :: Pos -> Type -> Type -> Solve Substitution
unifies _ t1 t2
  | t1 == t2 = return mempty
-- unifies (TN a) (TN b)
--   = autoConvert a b -- TODO: add auto convertion
unifies p (TV v) t
  = bind p v t
unifies p t (TV v)
  = bind p v t
unifies p (TApp (TN "Tuple") [a]) b@(TN _)
  = unifies p a b
unifies p a@(TN _) (TApp (TN "Tuple") [b])
  = unifies p a b
unifies p (TApp f as) (TApp f' as')
  = unifyMany p (f:as) (f':as')
unifies p a b
  = throwError $ UnificationFail p a b

bind :: Pos -> TVar -> Type -> Solve Substitution
bind pos v ty
  | ty == TV v    = return mempty
  | occursIn v ty = throwError $ InfiniteType pos v ty
  | otherwise     = return (Sub $ Map.singleton v ty)
