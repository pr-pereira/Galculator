{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Text.ParserCombinators.Parsec.ExtendendToken
Description :  Useful functions for parsing.
Copyright   :  (c) Paulo Silva
License     :  LGPL

Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable

-}
 
-------------------------------------------------------------------------------

module Text.ParserCombinators.Parsec.ExtendedToken (
  TokenParser,
  LanguageDef(..),
  makeTokenParser,
  setReservedNames,
  variable,
  whiteSpace,
  lexeme,
  symbol,
  parens,
  braces,
  brackets,
  squares,
  semi,
  comma,
  colon,
  dot,
  semiSep,
  semiSep1,
  commaSep,
  commaSep1,
  charLiteral,
  stringLiteral,
  decimal,
  hexadecimal,
  octal,
  natural,
  integer,
  float,
  naturalOrFloat,
  identifier,
  reserved,
  operator,
  reservedOp,
  commentStart,
  commentEnd,
  commentLine,
  nestedComments,
  identStart,
  identLetter,
  opStart,
  opLetter,
  reservedNames,
  reservedOpNames,
  caseSensitive
 ) where

import Data.Char(toLower)
import Data.List(sort)
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P

-------------------------------------------------------------------------------

data TokenParser st = 
  TokenParser {
    tokenParser :: P.TokenParser st,
    variable    :: CharParser st String
  }

data LanguageDef st =
  LanguageDef {
    languageDef    :: P.LanguageDef st,
    variableLetter :: CharParser st Char,
    variableStart  :: CharParser st Char
  }

makeTokenParser :: LanguageDef st -> TokenParser st
makeTokenParser ldef = TokenParser {
  tokenParser = tokenParser',
  variable = variable' }
  where
    tokenParser' = P.makeTokenParser (languageDef ldef)
    
    variable' = P.lexeme tokenParser' (try (do
      name <- var
      if (isReservedName name)
        then unexpected ("reserved word " ++ show name)
        else return name))

    var = (do
      c <- variableStart ldef
      cs <- many (variableLetter ldef)
      return (c:cs))
      <?> "variable"
    
    isReservedName name = isReserved theReservedNames caseName
        where
          caseName      | caseSensitive ldef  = name
                        | otherwise           = map toLower name

    isReserved names name    
        = scan names
        where
          scan []       = False
          scan (r:rs)   = case (compare r name) of
                            LT  -> scan rs
                            EQ  -> True
                            GT  -> False

    theReservedNames
        | caseSensitive ldef  = sortedNames
        | otherwise           = map (map toLower) sortedNames
        where
          sortedNames   = sort (reservedNames ldef)

setReservedNames :: [String] -> [String] -> LanguageDef st -> LanguageDef st
setReservedNames reservedNames' reservedOpNames' ldef = ldef {
  languageDef = (languageDef ldef) { P.reservedNames = reservedNames',
                                     P.reservedOpNames = reservedOpNames' }}

whiteSpace :: TokenParser st -> CharParser st ()
whiteSpace = P.whiteSpace . tokenParser

lexeme :: TokenParser st -> CharParser st a -> CharParser st a
lexeme = --P.lexeme . tokenParser
  \tkp p -> do
    x <- p
    whiteSpace tkp
    return x

symbol :: TokenParser st -> String -> CharParser st String
symbol = P.symbol . tokenParser

parens :: TokenParser st -> CharParser st a -> CharParser st a
parens = --P.parens . tokenParser
  \tkp p -> do
    symbol tkp "("
    x <- p
    symbol tkp ")"
    return x

braces :: TokenParser st -> CharParser st a -> CharParser st a
braces = --P.braces . tokenParser
  \tkp p -> do
    symbol tkp "{"
    x <- p
    symbol tkp "}"
    return x

brackets :: TokenParser st -> CharParser st a -> CharParser st a
brackets = --P.brackets . tokenParser
  \tkp p -> do
    symbol tkp "["
    x <- p
    symbol tkp "]"
    return x

squares :: TokenParser st -> CharParser st a -> CharParser st a
squares = --P.squares . tokenParser
  \tkp p -> do
    symbol tkp "["
    x <- p
    symbol tkp "]"
    return x

semi :: TokenParser st -> CharParser st String
semi = P.semi . tokenParser

comma :: TokenParser st -> CharParser st String
comma = P.comma . tokenParser

colon :: TokenParser st -> CharParser st String
colon = P.colon . tokenParser

dot :: TokenParser st -> CharParser st String
dot = P.dot . tokenParser

semiSep :: TokenParser st -> CharParser st a -> CharParser st [a]
semiSep = --P.semiSep . tokenParser
  \tkp p -> do
    x <- p
    xs <- many (do
      symbol tkp ";"
      p)
    return (x:xs)

semiSep1 :: TokenParser st -> CharParser st a -> CharParser st [a]
semiSep1 = --P.semiSep1 . tokenParser
  \tkp p -> do
    x <- p
    xs <- many (do
      symbol tkp ";"
      p)
    return (x:xs)

commaSep :: TokenParser st -> CharParser st a -> CharParser st [a]
commaSep = --P.commaSep . tokenParser
  \tkp p -> do
    x <- p
    xs <- many (do
      symbol tkp ","
      p)
    return (x:xs)

commaSep1 :: TokenParser st -> CharParser st a -> CharParser st [a]
commaSep1 = --P.commaSep1 . tokenParser
  \tkp p -> do
    x <- p
    xs <- many (do
      symbol tkp ","
      p)
    return (x:xs)

charLiteral :: TokenParser st -> CharParser st Char
charLiteral = P.charLiteral . tokenParser

stringLiteral :: TokenParser st -> CharParser st String
stringLiteral = P.stringLiteral . tokenParser

decimal :: TokenParser st -> CharParser st Integer
decimal = P.decimal . tokenParser

hexadecimal :: TokenParser st -> CharParser st Integer 
hexadecimal = P.hexadecimal . tokenParser

octal :: TokenParser st -> CharParser st Integer
octal = P.octal . tokenParser

natural :: TokenParser st -> CharParser st Integer 
natural = P.natural . tokenParser

integer :: TokenParser st -> CharParser st Integer
integer = P.integer . tokenParser

float :: TokenParser st -> CharParser st Double
float = P.float . tokenParser

naturalOrFloat :: TokenParser st -> CharParser st (Either Integer Double)
naturalOrFloat = P.naturalOrFloat . tokenParser

identifier :: TokenParser st -> CharParser st String 
identifier = P.identifier . tokenParser

reserved :: TokenParser st -> String -> CharParser st ()
reserved tkp = P.reserved (tokenParser tkp)

operator :: TokenParser st -> CharParser st String
operator = P.operator . tokenParser

reservedOp :: TokenParser st -> String -> CharParser st ()
reservedOp tkp = P.reservedOp (tokenParser tkp)

commentStart :: LanguageDef st ->  String
commentStart = P.commentStart . languageDef

commentEnd :: LanguageDef st -> String
commentEnd = P.commentEnd . languageDef

commentLine :: LanguageDef st -> String
commentLine = P.commentLine . languageDef

nestedComments :: LanguageDef st -> Bool 
nestedComments = P.nestedComments . languageDef

identStart :: LanguageDef st -> CharParser st Char
identStart = P.identStart . languageDef

identLetter :: LanguageDef st ->CharParser st Char 
identLetter = P.identLetter . languageDef

opStart :: LanguageDef st -> CharParser st Char 
opStart = P.opStart . languageDef

opLetter :: LanguageDef st -> CharParser st Char
opLetter = P.opLetter . languageDef

reservedNames :: LanguageDef st -> [String] 
reservedNames = P.reservedNames . languageDef

reservedOpNames :: LanguageDef st -> [String] 
reservedOpNames = P.reservedOpNames . languageDef

caseSensitive :: LanguageDef st -> Bool 
caseSensitive = P.caseSensitive . languageDef

