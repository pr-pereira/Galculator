{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Language.Type.SyntaxADT
Description :  Polymorphic type representation of the types used by Galculator,
               using regular Abstract Data Types.
Copyright   :  (c) Paulo Silva
License     :  LGPL

Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable

-}
 
-------------------------------------------------------------------------------
 
module Language.Type.SyntaxADT (
  Type(..)
 ) where

data Type =
    TVar String 
  | One    
  | Bool   
  | Char   
  | String 
  | Int    
  | Float  
  | Prod   Type Type
  | Either Type Type
  | Maybe  Type 
  | List   Type
  | Set    Type 
  | Map    Type Type
  | Fun    Type Type
  | Rel    Type Type
  | Ord    Type
  | GC     Type Type
  | Expr   Type
  deriving (Show, Eq)

