{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Potion.Type.Substitution where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Language.Potion.Type
import Language.Potion.Type.Context

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
  apply sub ty@(TCon _) = ty
  apply (Sub s) ty@(TVar a) = Map.findWithDefault ty a s
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
  apply (Sub s) (Forall vars t) = Forall vars $ apply sub t
    where
      sub = Sub $ foldr Map.delete s vars

  free (Forall vars t) = Set.difference (free t) (Set.fromList vars)

instance Substitutable Context where
  apply sub (Context ctx) = Context $ Map.map (apply sub) ctx
  free (Context ctx) = free $ Map.elems ctx

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  free = foldr (Set.union . free) Set.empty

occursIn :: Substitutable a => TVar -> a -> Bool
occursIn var ty = var `Set.member` free ty
