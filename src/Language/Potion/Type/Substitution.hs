{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Potion.Type.Substitution where

import Data.Char (isAlpha)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Language.Potion.Core
import Language.Potion.Syntax
import Language.Potion.Type

newtype Substitution
  = Sub (Map TVar Type)
  deriving (Eq, Ord, Show, Monoid)

empty :: Substitution
empty = mempty

compose :: Substitution -> Substitution -> Substitution
compose s1@(Sub m1) (Sub m2) =
  Sub $ Map.map (apply s1) (m2 `Map.union` m1)

class Substitutable a where
  apply :: Substitution -> a -> a
  free :: a -> Set TVar

instance Substitutable Type where
  apply (Sub s) ty@(TV a) = Map.findWithDefault ty a s
  apply sub (TApp f as) = TApp (apply sub f) (map (apply sub) as)
  apply sub ty = ty

  free (TV a) = Set.singleton a
  free (TApp f as) = Set.unions $ map free (f:as)
  free _ = Set.empty

instance Substitutable Scheme where
  apply (Sub s) (Forall vars t) = Forall vars $ apply sub t
    where
      sub = Sub $ foldr Map.delete s vars

  free (Forall vars t) = Set.difference (free t) (Set.fromList vars)

instance Substitutable CExp where
  apply sub (CApp meta f args)
    = CApp (apply sub meta) (apply sub f) (apply sub args)
  apply sub (CFun meta params exp)
    = CFun (apply sub meta) (apply sub params) (apply sub exp)
  apply sub (CMatch meta exp branches)
    = CMatch (apply sub meta) (apply sub exp) (apply sub branches)
  apply sub (CCase meta with when exp)
    = CCase (apply sub meta) (apply sub with) (apply sub when) (apply sub exp)
  apply sub (CLit meta lit)
    = CLit (apply sub meta) lit
  apply sub (CName meta name)
    = CName (apply sub meta) name
  apply sub (CHole meta)
    = CHole (apply sub meta)

  free exp = free $ cMeta exp

instance Substitutable Meta where
  apply sub meta = meta{ mType = apply sub $ mType meta, mApps = apply sub $ mApps meta }
  free meta = free $ mType meta

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  free = foldr (Set.union . free) Set.empty

instance Substitutable a => Substitutable (Map k a) where
  apply sub = Map.map (apply sub)
  free = Set.unions . map free . Map.elems

instance (Substitutable a, Ord a) => Substitutable (Set a) where
  apply sub = Set.map (apply sub)
  free = Set.unions . map free . Set.elems

occursIn :: Substitutable a => TVar -> a -> Bool
occursIn var ty = var `Set.member` free ty

