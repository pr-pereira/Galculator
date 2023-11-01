
{-# OPTIONS_GHC -Wall #-}
 
-------------------------------------------------------------------------------

{- |
Module      :  Language.Law.Parser
Description :  
Copyright   :  (c) Paulo Silva
License     :  LGPL
  
Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable
 
<description of the module>
-}

-------------------------------------------------------------------------------

module Language.Law.Parser (
  parser,
  parseLaw,
  parseTheorem
 ) where

import Control.Monad
import Language.Law.SyntaxADT
import qualified Language.R.Parser as R
--import Language.R.SyntaxADT
import qualified Language.Step.Parser as S
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P

-------------------------------------------------------------------------------

type LawParser = Parser LawS

-------------------------------------------------------------------------------

reservNames :: [String]
reservNames = ["EQUIV", "IMPL"]

lexer :: P.TokenParser st
lexer = P.makeTokenParser $ emptyDef { P.reservedNames = reservNames }

-------------------------------------------------------------------------------

reserved :: String -> CharParser st ()
reserved = P.reserved lexer

whiteSpace :: CharParser st ()
whiteSpace = P.whiteSpace lexer

identifier :: CharParser st String
identifier = P.identifier lexer

-------------------------------------------------------------------------------

parser :: String -> Either ParseError LawS
parser = parse mainLawParser "" 

-------------------------------------------------------------------------------

mainLawParser :: LawParser 
mainLawParser = do
  whiteSpace
  l <- parseLaw
  eof
  return l

-------------------------------------------------------------------------------

parseLaw :: LawParser
parseLaw = 
  parseEquiv <|>
  parseImpl 

-------------------------------------------------------------------------------

parseEquiv :: LawParser
parseEquiv = do
  p <- getPosition
  reserved "EQUIV"
  ident <- identifier
  r1 <- R.parseR
  r2 <- R.parseR
  return $ EquivS p ident r1 r2
  
-------------------------------------------------------------------------------

parseImpl :: LawParser
parseImpl = do
  p <- getPosition
  reserved "IMPL"
  ident <- identifier
  r1 <- R.parseR
  r2 <- R.parseR
  return $ ImplS p ident r1 r2

-------------------------------------------------------------------------------

reservNamesT :: [String]
reservNamesT = ["Theorem", "EQUIV", "IMPL"]

type TheoremParser = Parser Theorem

lexerT :: P.TokenParser st
lexerT = P.makeTokenParser $ emptyDef { P.reservedNames = reservNamesT }

-------------------------------------------------------------------------------

reservedT :: String -> CharParser st ()
reservedT = P.reserved lexerT

whiteSpaceT :: CharParser st ()
whiteSpaceT = P.whiteSpace lexerT

identifierT :: CharParser st String
identifierT = P.identifier lexerT


mainTheoremParser :: TheoremParser
mainTheoremParser = do
  whiteSpaceT
  t <- parseTheorem
  eof
  return t

parseTheorem :: TheoremParser
parseTheorem =
  parseEquivTh <|>
  parseImplTh

parseEquivTh :: TheoremParser
parseEquivTh = do
  p <- getPosition
  reservedT "Theorem"
  ident <- identifierT
  reservedT "EQUIV"
  r1 <- R.parseR
  r2 <- R.parseR
  s <- S.parseStep
  return $ EquivT p ident r1 r2 s

parseImplTh :: TheoremParser
parseImplTh = do
  p <- getPosition
  reservedT "Theorem"
  ident <- identifierT
  reservedT "IMPL"
  r1 <- R.parseR
  r2 <- R.parseR
  s <- S.parseStep
  return $ ImplT p ident r1 r2 s

