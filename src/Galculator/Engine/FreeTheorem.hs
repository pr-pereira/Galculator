
--{-# LANGUAGE TypeOperators, FlexibleContexts, PatternSignatures #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fglasgow-exts #-}

-------------------------------------------------------------------------------

{- |
Module      :  Galculator.Engine.FreeTheorem
Description :  Free theorem generation.
Copyright   :  (c) Paulo Silva
License     :  LGPL

Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable

-}

-------------------------------------------------------------------------------

module Galculator.Engine.FreeTheorem -- (
--  free
-- ) 
  where

import Control.MonadOr
import Control.Monad.Fresh
import Language.Law.Syntax
import Language.R.Syntax
import Language.Type.Constraint
import Language.Type.Equality
import Language.Type.Rewrite
import Language.Type.Syntax
import Language.Type.Utils

-------------------------------------------------------------------------------

free :: (MonadFresh [String] String m) 
     => Type (b :<-: a) -> R (b :<-: a) -> m Law
free (Fun b a) f = do
  undefined

-------------------------------------------------------------------------------

tt = Fun (Fun (Fun (TVar "a")
                   (List (TVar "a")))
              (Fun (TVar "a")
                   (Prod (TVar "a") (TVar "a"))))
         (TVar "a")

tt' = Fun (TVar "a") 
          (Fun (List (TVar "a")) 
               (Fun (Fun (TVar "a") (Prod (TVar "a") (TVar "a")))
                    (TVar "a")))

tt2 = Fun (Fun (List (Prod (Fun (TVar "a") (TVar "b")) (TVar "b")))
               (TVar "a"))
          (Fun (TVar "b")
               (TVar "a"))

tt2' = Fun (List (Prod (Fun (TVar "a") (TVar "b")) (TVar "b"))) 
           (Fun (TVar "a")
                (Fun (TVar "b")
                     (TVar "a")))

tSubst :: MonadOr m => ([Constraint], [Constraint]) -> Type t -> m TypeBox
tSubst (c1,c2) t = do
  let r1::[Rule] = map constraint2Rule c1
      r2::[Rule] = map constraint2Rule c2
  return . view2Box =<< bla (r1, r2) t

bla :: ([Rule], [Rule]) -> Rule
bla (r1,r2) (Fun u v) = do
  View u' <- bla (r1, r2) u
  View v' <- bla (r2, r1) v
  return $ View (Fun u' v')
bla (r1, r2) t = if (isTVar t) then try (seqRules r1) t else 
  Language.Type.Rewrite.all (bla (r1, r2)) t

test :: Maybe TypeBox
test = tSubst ([TVar "a" :=: Int], [TVar "a" :=: Bool]) tt

test2 :: Maybe TypeBox
test2 = tSubst ([TVar "a" :=: TVar "ordA", TVar "b" :=: TVar "ordB"],
                [TVar "a" :=: TVar "I", TVar "b" :=: TVar "J"]) tt2

rule :: Rule
rule (TVar _) = return $ View Int
rule x = mzero

type2Rel :: MonadOr m => Type a -> m (R (a :<->: a))
type2Rel (TVar v) = return $ Var v
type2Rel One = return ID
type2Rel Bool = return ID
type2Rel Char = return ID
type2Rel String = return ID
type2Rel Int = return ID
type2Rel Float = return ID
type2Rel (Prod a b) = do
  a' <- type2Rel a
  b' <- type2Rel b
  return $ PROD a' b'
type2Rel (Either a b) = do
  a' <- type2Rel a
  b' <- type2Rel b
  return $ EITHER a' b'
type2Rel (Maybe a) = do
  a' <- type2Rel a
  return $ MAYBE a'
type2Rel (List a) = do
  a' <- type2Rel a
  return $ LIST a'
type2Rel (Set a) = do
  a' <- type2Rel a
  return $ SET a'
type2Rel (Map _ b) = do
  b' <- type2Rel b
  return $ MAP b'
type2Rel (Fun a b) = do
  a' <- type2Rel a
  b' <- type2Rel b
  return $ REYNOLDS a' b'
type2Rel _ = mzero