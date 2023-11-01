{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Text.ParserCombinators.Parsec.Utils
Description :  Useful functions for parsing.
Copyright   :  (c) Paulo Silva
License     :  LGPL

Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable

-}
 
-------------------------------------------------------------------------------

module Text.ParserCombinators.Parsec.Utils (
  Table,
  binary,
  prefix,
  postfix,
  binaryP,
  prefixP,
  postfixP,
  wrapper,
  parseIdentifier,
  parseKeyword,
  parseVariable,
  parseIdentifierP,
  parseKeywordP,
  parseVariableP,
  galoisLexer
 ) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.ExtendedLanguage
import qualified Text.ParserCombinators.Parsec.ExtendedToken as P
import Text.ParserCombinators.Parsec.Expr

type Table a = forall st . [[Operator Char st a]]

galoisLexer :: [String] -> [String] -> P.TokenParser st
galoisLexer reservedNames reservedOpNames = P.makeTokenParser $ 
  P.setReservedNames reservedNames reservedOpNames galoisStyle

binary :: P.TokenParser st -> String -> (a -> a -> a) -> Assoc -> Operator Char st a
binary lexer name fun assoc = Infix (do {P.reservedOp lexer name; return fun}) assoc

binaryP :: P.TokenParser st -> String -> (SourcePos -> a -> a -> a) -> Assoc -> Operator Char st a
binaryP lexer name fun assoc = Infix (do 
  p <- getPosition
  P.reservedOp lexer name 
  return $ fun p) assoc

-------------------------------------------------------------------------------

prefix :: P.TokenParser st ->  String -> (a -> a) -> Operator Char st a
prefix lexer name fun = Prefix (do {P.reserved lexer name; return fun})

prefixP :: P.TokenParser st ->  String -> (SourcePos -> a -> a) -> Operator Char st a
prefixP lexer name fun = Prefix $ do 
  p <- getPosition
  P.reserved lexer name 
  return $ fun p

-------------------------------------------------------------------------------

postfix :: P.TokenParser st ->  String -> (a -> a) -> Operator Char st a
postfix lexer name fun = Postfix (do {P.reserved lexer name; return fun})

postfixP :: P.TokenParser st ->  String -> (SourcePos -> a -> a) -> Operator Char st a
postfixP lexer name fun = Postfix $ do 
  p <- getPosition
  P.reserved lexer name 
  return $ fun p
-------------------------------------------------------------------------------

wrapper :: P.TokenParser st -> GenParser Char st t -> GenParser Char st t
wrapper lexer prs = do
  P.whiteSpace lexer
  t <- prs
  eof
  return t

-------------------------------------------------------------------------------

parseIdentifier :: P.TokenParser st -> (String -> a) -> GenParser Char st a
parseIdentifier lexer fun = do
  tid <- P.identifier lexer
  return $ fun tid

parseIdentifierP :: P.TokenParser st -> (SourcePos -> String -> a) -> GenParser Char st a
parseIdentifierP lexer fun = do
  p <- getPosition
  tid <- P.identifier lexer
  return $ fun p tid

-------------------------------------------------------------------------------

parseKeyword :: P.TokenParser st -> String -> a -> GenParser Char st a
parseKeyword lexer name fun = do
  P.reserved lexer name
  return fun

parseKeywordP :: P.TokenParser st -> String -> (SourcePos -> a) -> GenParser Char st a
parseKeywordP lexer name fun = do
  p <- getPosition
  P.reserved lexer name
  return $ fun p

-------------------------------------------------------------------------------

parseVariable :: P.TokenParser st -> (String -> a) -> GenParser Char st a
parseVariable lexer fun = do
  tid <- P.variable lexer
  return $ fun tid

parseVariableP :: P.TokenParser st -> (SourcePos -> String -> a) -> GenParser Char st a
parseVariableP lexer fun = do
  p <- getPosition
  tid <- P.variable lexer
  return $ fun p tid

