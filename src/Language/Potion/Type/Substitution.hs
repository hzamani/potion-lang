{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Potion.Type.Substitution where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Language.Potion.Syntax
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
  apply sub ty@(TN _) = ty
  apply (Sub s) ty@(TV a) = Map.findWithDefault ty a s
  apply sub (TApp f as) = TApp (apply sub f) (map (apply sub) as)

  free (TN _) = Set.empty
  free (TV a) = Set.singleton a
  free (TApp f as) = Set.unions $ map free (f:as)

instance Substitutable Scheme where
  apply (Sub s) (Forall vars t) = Forall vars $ apply sub t
    where
      sub = Sub $ foldr Map.delete s vars

  free (Forall vars t) = Set.difference (free t) (Set.fromList vars)

instance Substitutable Expression where
  apply sub (ET e t) = ET (apply sub e) (apply sub t)
  apply sub (EApp f args) = EApp (apply sub f) (apply sub args)
  apply sub (EFun params exp) = EFun (apply sub params) (apply sub exp)
  apply sub (EMatch exp branches) = EMatch (apply sub exp) (apply sub branches)
  apply sub e = e

  free (ET e t) = free e `Set.union` free t
  free (EApp f args) = free f `Set.union` free args
  free (EFun params exp) = free params `Set.union` free exp
  free (EMatch exp branches) = free exp `Set.union` free branches
  free e = Set.empty

instance Substitutable Context where
  apply sub (Context ctx) = Context $ Map.map (apply sub) ctx
  free (Context ctx) = free $ Map.elems ctx

instance (Substitutable a, Substitutable b, Substitutable c) => Substitutable (a, b, c) where
  apply sub (x, y, z) = (apply sub x, apply sub y, apply sub z)
  free (x, _, _) = free x

instance Substitutable a => Substitutable (Map k a) where
  apply sub = Map.map (apply sub)
  free = Set.unions . map free . Map.elems

instance (Substitutable a, Ord a) => Substitutable (Set a) where
  apply sub = Set.map (apply sub)
  free = Set.unions . map free . Set.elems

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  free = foldr (Set.union . free) Set.empty

occursIn :: Substitutable a => TVar -> a -> Bool
occursIn var ty = var `Set.member` free ty
