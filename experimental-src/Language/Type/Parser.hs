
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Language.Type.Parser
Description :  Parser of the type representation.
Copyright   :  (c) Paulo Silva
License     :  LGPL

Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable

-}

-------------------------------------------------------------------------------

module Language.Type.Parser (
 parser,
 parseType
 ) where

import Control.GalcError
import Control.Monad.Except
import Language.Type.SyntaxADT
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.ExtendedToken as P
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Utils

-------------------------------------------------------------------------------

parser :: MonadError GalcError m => String -> m Type
parser = either2error (ParsingError . show) . parse (wrapper lexer parseType) ""

-------------------------------------------------------------------------------

parseType :: Parser Type
parseType = buildExpressionParser table term
  <?> "type expression"

-------------------------------------------------------------------------------

term :: Parser Type
term =
  P.parens lexer parseType           <|>
  parseVariable lexer TVar  <|>
  parseKeyword lexer "One"    One    <|>
  parseKeyword lexer "Bool"   Bool   <|>
  parseKeyword lexer "Char"   Char   <|>
  parseKeyword lexer "String" String <|>
  parseKeyword lexer "Int"    Int    <|>
  parseKeyword lexer "Float"  Float  <?>
  "type expression"

-------------------------------------------------------------------------------

-- Some ambiguities arise from the use of equal precedences.
-- This does not follows the specification.
table :: Table Type
table = [
  [prefix lexer "Maybe" Maybe],
  [prefix lexer "Set" Set, prefix lexer "List" List],
  [binary lexer "<-|" Map AssocRight],
  [binary lexer "><" Prod AssocRight, binary lexer "-|-" Either AssocRight],
  [binary lexer "<-" Fun AssocRight,  binary lexer "<->" Rel AssocRight],
  [binary lexer "~~" GC AssocNone],
  [prefix lexer "Ord" Ord],
  [prefix lexer "Expr" Expr]
 ]

-------------------------------------------------------------------------------

lexer :: P.TokenParser st
lexer = galoisLexer reservedNames reservedOpNames
  where
    reservedNames = ["One", "Bool", "Char", "String", "Int", "Float",
                      "Set", "List", "Maybe", "Ord", "Expr"]
    reservedOpNames = ["><", "-|-", "<-", "<->", "<-|", "~~"]

-------------------------------------------------------------------------------
