
-------------------------------------------------------------------------------

{- |
Module      :  Language.Law.SyntaxADT
Description :  
Copyright   :  (c) Paulo Silva
License     :  LGPL

Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable

<description of the module>
-}

-------------------------------------------------------------------------------

module Language.Law.SyntaxADT (
  LawS(..),
  Theorem(..),
  getName,
  getNameT
 )where

-------------------------------------------------------------------------------

import Language.R.SyntaxADT
import Language.Step.Syntax
import Text.ParserCombinators.Parsec.Pos

-------------------------------------------------------------------------------

data LawS = 
    EquivS SourcePos String S S
  | ImplS SourcePos String S S
  deriving (Eq, Show)

-------------------------------------------------------------------------------

getName :: LawS -> String
getName (EquivS _ n _ _) = n
getName (ImplS _ n _ _) = n
-------------------------------------------------------------------------------

data Theorem =
    EquivT SourcePos String S S Step
  | ImplT SourcePos String S S Step
  deriving Show

getNameT :: Theorem -> String
getNameT (EquivT _ n _ _ _) = n
getNameT (ImplT _ n _ _ _) = n

