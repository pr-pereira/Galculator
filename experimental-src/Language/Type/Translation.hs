 
-- {-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Language.Type.Translation
Description :  Translations between type representations.
Copyright   :  (c) Paulo Silva
License     :  LGPL

Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable

-}
 
-------------------------------------------------------------------------------
 
module Language.Type.Translation (
  gadt2adt,
  adt2gadt
 ) where

import Control.Monad
import Data.Existential
import qualified Language.Type.Syntax as GADT
import qualified Language.Type.SyntaxADT as ADT

-------------------------------------------------------------------------------

gadt2adt :: GADT.Type a -> ADT.Type
gadt2adt (GADT.TVar s)       = ADT.TVar s
gadt2adt GADT.One            = ADT.One 
gadt2adt GADT.Bool           = ADT.Bool
gadt2adt GADT.Char           = ADT.Char
gadt2adt GADT.String         = ADT.String
gadt2adt GADT.Int            = ADT.Int
gadt2adt GADT.Float          = ADT.Float
gadt2adt (GADT.Prod t1 t2)   = ADT.Prod (gadt2adt t1) (gadt2adt t2)
gadt2adt (GADT.Either t1 t2) = ADT.Either (gadt2adt t1) (gadt2adt t2)
gadt2adt (GADT.Maybe t)      = ADT.Maybe (gadt2adt t)
gadt2adt (GADT.List t)       = ADT.List (gadt2adt t)
gadt2adt (GADT.Set t)        = ADT.Set (gadt2adt t)
gadt2adt (GADT.Map t1 t2)    = ADT.Map (gadt2adt t1) (gadt2adt t2)
gadt2adt (GADT.Fun t1 t2)    = ADT.Fun (gadt2adt t1) (gadt2adt t2)
gadt2adt (GADT.Rel t1 t2)    = ADT.Rel (gadt2adt t1) (gadt2adt t2)
gadt2adt (GADT.Ord t)        = ADT.Ord (gadt2adt t)
gadt2adt (GADT.GC t1 t2)     = ADT.GC (gadt2adt t1) (gadt2adt t2)
gadt2adt (GADT.Expr t)       = ADT.Expr (gadt2adt t)

-------------------------------------------------------------------------------

adt2gadt :: Monad m => ADT.Type -> m GADT.TypeBox
adt2gadt (ADT.TVar s)       = return $ Hide $ GADT.TVar s
adt2gadt ADT.One            = return $ Hide $ GADT.One 
adt2gadt ADT.Bool           = return $ Hide $ GADT.Bool 
adt2gadt ADT.Char           = return $ Hide $ GADT.Char 
adt2gadt ADT.String         = return $ Hide $ GADT.String 
adt2gadt ADT.Int            = return $ Hide $ GADT.Int 
adt2gadt ADT.Float          = return $ Hide $ GADT.Float 
adt2gadt (ADT.Prod t1 t2)   = do
  Hide t1' <- adt2gadt t1
  Hide t2' <- adt2gadt t2
  return $ Hide $ GADT.Prod t1' t2'
adt2gadt (ADT.Either t1 t2) = do 
  Hide t1' <- adt2gadt t1
  Hide t2' <- adt2gadt t2
  return $ Hide $ (GADT.Either t1' t2')
adt2gadt (ADT.Maybe t)      = do 
  Hide t' <- adt2gadt t
  return $ Hide $ GADT.Maybe t'
adt2gadt (ADT.List t)       = do
  Hide t' <- adt2gadt t
  return $ Hide $ GADT.List t'
adt2gadt (ADT.Set t)        = do
  Hide t' <- adt2gadt t
  return $ Hide $ GADT.Set t'
adt2gadt (ADT.Map t1 t2)    = do
  Hide t1' <- adt2gadt t1
  Hide t2' <- adt2gadt t2
  return $ Hide $ GADT.Map t1' t2'
adt2gadt (ADT.Fun t1 t2)    = do
  Hide t1' <- adt2gadt t1
  Hide t2' <- adt2gadt t2
  return $ Hide $ GADT.Fun t1' t2'
adt2gadt (ADT.Rel t1 t2)    = do
  Hide t1' <- adt2gadt t1
  Hide t2' <- adt2gadt t2
  return $ Hide $ GADT.Rel t1' t2'
adt2gadt (ADT.Ord t)        = do
  Hide t' <- adt2gadt t
  return $ Hide $ GADT.Ord t'
adt2gadt (ADT.GC t1 t2)     = do
  Hide t1' <- adt2gadt t1
  Hide t2' <- adt2gadt t2
  return $ Hide $ GADT.GC t1' t2'
adt2gadt (ADT.Expr t)       = do
  Hide t' <- adt2gadt t
  return $ Hide $ GADT.Expr t'

-------------------------------------------------------------------------------
