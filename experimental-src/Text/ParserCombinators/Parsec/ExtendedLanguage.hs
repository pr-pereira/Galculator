{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Text.ParserCombinators.Parsec.ExtendendLanguage
Description :  Useful functions for parsing.
Copyright   :  (c) Paulo Silva
License     :  LGPL

Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable

TODO: Misses: mondrianDef, haskell98Def, haskellDef, mondrian, haskell
-}
 
-------------------------------------------------------------------------------

module Text.ParserCombinators.Parsec.ExtendedLanguage (
  emptyDef,
  javaStyle,
  haskellStyle,
  galoisStyle
 ) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.ExtendedToken as T
import qualified Text.ParserCombinators.Parsec.Language as L

emptyDef :: T.LanguageDef st
emptyDef = T.LanguageDef {
  T.languageDef = L.emptyDef,
  T.variableLetter = L.identLetter L.emptyDef,
  T.variableStart = L.identStart L.emptyDef
 }

javaStyle :: T.LanguageDef st
javaStyle = T.LanguageDef {
  T.languageDef = L.javaStyle,
  T.variableLetter = L.identLetter L.javaStyle,
  T.variableStart = L.identStart L.javaStyle
 }

haskellStyle :: T.LanguageDef st
haskellStyle = T.LanguageDef {
  T.languageDef = L.haskellStyle,
  T.variableLetter = L.identLetter L.haskellStyle,
  T.variableStart = L.identStart L.haskellStyle
 }

galoisStyle :: T.LanguageDef st
galoisStyle = T.LanguageDef {
  T.languageDef = L.emptyDef { L.commentStart = "{-",
                               L.commentEnd = "-}",
                               L.commentLine = "--",
                               L.nestedComments = True,
                               L.identStart = upper},
  T.variableLetter = alphaNum <|> oneOf "_'",
  T.variableStart = lower
 }

