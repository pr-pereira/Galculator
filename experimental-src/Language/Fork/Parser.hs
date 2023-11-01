
{-# LANGUAGE FlexibleContexts, GADTs #-}
{-# OPTIONS_GHC -Wall #-}

-------------------------------------------------------------------------------

{- |
Module      :  Language.R.Parser
Description :  Parser of the fork representation.
Copyright   :  (c) Paulo Silva
License     :  LGPL

Maintainer  :  paufil@di.uminho.pt
Stability   :  experimental
Portability :  portable

Restricting the reserved words to local languages improves reuse and modularity.
However, since keywords may be used as identifiers, some strange definitions
are possible, like Fun Fun.
Thus, a global lexer is used instead.
-}

-------------------------------------------------------------------------------

module Language.Fork.Parser (
  parser,
  parseFork
 ) where

import Control.GalcError
import Control.Monad.Except
import Language.R.SyntaxADT
import qualified Language.Type.Parser as TP
import qualified Language.Type.SyntaxADT as Type
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.ExtendedToken as P
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Utils

--import Language.Derivation.Syntax

-------------------------------------------------------------------------------

parser :: MonadError GalcError m => String -> m S
parser = either2error (ParsingError . show) . parse (wrapper lexer parseFork) ""

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


parseFork :: Parser S
parseFork = buildExpressionParser forkTable forkTerm
  <?> "Fork expression"

-------------------------------------------------------------------------------

forkTerm :: Parser S
forkTerm =
  P.parens lexer parseFork <|>
  parseIdentifierP lexer RefS <|>
  parseVariableP lexer VarS <|>
  parseKeywordP lexer "Id" IdS <|>
  parseKeywordP lexer "Top" TopS <|>
  parseKeywordP lexer "Bot" BotS <|>
  parseOrd <|>
  parseFun <?>
  "Fork simple expression"

-------------------------------------------------------------------------------

forkTable :: Table S
forkTable = [
  [postfixP lexer "*" ConvS],
  [prefixP lexer "~" NegS],
  [prefixP lexer "Maybe" MaybeS, prefixP lexer "List" ListS,
   prefixP lexer "Set" SetS, prefixP lexer "Map" MapS],
  [binaryP lexer "><" ProdS AssocLeft, binaryP lexer "-|-" EitherS AssocLeft],
  [binaryP lexer "/*\\" SplitS AssocLeft],
  [binaryP lexer "/\\" MeetS AssocLeft, binaryP lexer "\\/" JoinS AssocLeft],
  [binaryP lexer "." CompS AssocLeft]
 ]

-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

parseFunction :: Parser S
parseFunction = buildExpressionParser functionTable functionTerm
  <?> "Function expression"

functionTerm :: Parser S
functionTerm =
  P.parens lexer parseFunction <|>
  parseIdentifierP lexer RefS <|>
  parseVariableP lexer VarS <|>
  parseKeywordP lexer "Id" FIdS <?>
  "Function simple expression"

functionTable :: Table S
functionTable = [
  [binaryP lexer "." FCompS AssocLeft]
 ]
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

parseOrder :: Parser S
parseOrder = buildExpressionParser orderTable orderTerm
  <?> "Order expression"

orderTerm :: Parser S
orderTerm =
  P.parens lexer parseOrder <|>
  parseIdentifierP lexer RefS <|>
  parseVariableP lexer VarS <|>
  parseKeywordP lexer "Id" OIdS <?>
  "Order simple term"

orderTable :: Table S
orderTable = [
  [postfixP lexer "*" OConvS],
  [binaryP lexer "." OCompS AssocLeft]
 ]

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

parseGalois :: Parser S
parseGalois = buildExpressionParser galoisTable galoisTerm
  <?> "Galois expression"

galoisTerm :: Parser S
galoisTerm =
  P.parens lexer parseGalois <|>
  parseIdentifierP lexer RefS <|>
  parseVariableP lexer VarS <|>
  parseKeywordP lexer "Id" GIdS <?>
  "Galois simple term"

galoisTable :: Table S
galoisTable = [
  [postfixP lexer "*" GConvS],
  [binaryP lexer "." GCompS AssocLeft]
 ]
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

parseInterface :: Parser S
parseInterface = parseOrd <|> parseFun

parseOrd :: Parser S
parseOrd = do
  p <- getPosition
  P.reserved lexer "Ord"
  ord <- parseOrder
  return $ OrdS p ord

parseFun :: Parser S
parseFun = do
  p <- getPosition
  P.reserved lexer "Fun"
  fun <- parseFunction
  return $ FunS p fun

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

parseExpression :: Parser S
parseExpression = parseEqual <|> parseLess <?> "Expression"

parseEqual :: Parser S
parseEqual = try $ do
  f1 <- parseFork
  p <- getPosition
  P.reservedOp lexer "="
  f2 <- parseFork
  return $ EqualS p f1 f2

parseLess :: Parser S
parseLess = try $ do
  f1 <- parseFork
  p <- getPosition
  P.reservedOp lexer "<="
  f2 <- parseFork
  return $ LessS p f1 f2

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

parseBlock :: Parser Block
parseBlock =
  parseAxiom <|>
  parseTheorem <|>
  parseGaloisDef <|>
  parseDefinition <|>
  parseStrategy <?>
  "Block expression"

data Block =
    Axiom String S
  | Theorem String S [S] Step
  | Galois String S S S S
  | Definition String Type.Type (Maybe S)
  | Strategy String Step
  deriving (Eq, Show)

parseAxiom :: Parser Block
parseAxiom = do
  p <- getPosition
  P.reserved lexer "Axiom"
  ident <- P.identifier lexer
  P.symbol lexer ":="
  expr <- parseExpression
  return $ Axiom ident expr

parseTheorem :: Parser Block
parseTheorem = do
  p <- getPosition
  P.reserved lexer "Theorem"
  ident <- P.identifier lexer
  P.symbol lexer ":="
  expr <- parseExpression
  hyp <- option [] (P.squares lexer
    (parseExpression `sepEndBy` P.comma lexer))
  step <- P.braces lexer parseStep
  return $ Theorem ident expr hyp step

parseGaloisDef :: Parser Block
parseGaloisDef = do
  p <- getPosition
  P.reserved lexer "Galois"
  ident <- P.identifier lexer
  P.symbol lexer ":="
  f1 <- parseFunction
  f2 <- parseFunction
  o1 <- parseOrder
  o2 <- parseOrder
  return $ Galois ident f1 f2 o1 o2

parseDefinition :: Parser Block
parseDefinition = do
  p <- getPosition
  ident <- P.identifier lexer
  P.symbol lexer ":"
  typ <- TP.parseType
  def <- option Nothing (do
    P.symbol lexer ":="
    frk <- parseFork
    return $ Just frk)
  return $ Definition ident typ def


parseStrategy :: Parser Block
parseStrategy = do
  p <- getPosition
  P.reserved lexer "Strategy"
  ident <- P.identifier lexer
  P.symbol lexer ":="
  step <- parseStep
  return $ Strategy ident step

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

data Derivation =
    Inv Derivation
  | Shunt S
  | DistrLow S
  | DistrUp S
  | MonotUp S
  | MonotLow S
  | TopPreserv S
  | BotPreserv S
  | CancUp S
  | CancLow S
  | Free S
  | Apply String
  deriving (Eq, Show)

parseDerivation :: Parser Derivation
parseDerivation = parseInv <|> parseDerivation2 <?> "Derivation"

parseInv :: Parser Derivation
parseInv = do
  P.reserved lexer "inv"
  drv <- parseDerivation2
  return $ Inv drv

parseDerivation2 :: Parser Derivation
parseDerivation2 =
  parseIdentifier lexer Apply <|>
  parseDerivationAux "shunt" Shunt <|>
  parseDerivationAux "distr_low" DistrLow <|>
  parseDerivationAux "distr_up" DistrUp <|>
  parseDerivationAux "monot_up" MonotUp <|>
  parseDerivationAux "monot_low" MonotLow <|>
  parseDerivationAux "top_preserving" TopPreserv <|>
  parseDerivationAux "bot_preserving" BotPreserv <|>
  parseDerivationAux "cancel_up" CancUp <|>
  parseDerivationAux "cancel_low" CancLow <|>
  parseFree <?>
  "Derivation2 expression"

parseDerivationAux str fun = do
  P.reserved lexer str
  gal <- parseGalois
  return $ fun gal

parseFree :: Parser Derivation
parseFree = do
  P.reserved lexer "free"
  fun <- parseFunction
  return $ Free fun

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

data Combinator where
  Nop         :: Combinator
  Fail        :: Combinator
  Seq         :: Combinator -> Combinator -> Combinator
  Choice      :: Combinator -> Combinator -> Combinator
  LChoice     :: Combinator -> Combinator -> Combinator
  Many        :: Combinator -> Combinator
  Many1       :: Combinator -> Combinator
  Try         :: Combinator -> Combinator
  Once        :: Combinator -> Combinator
  Everywhere  :: Combinator -> Combinator
  Everywhere' :: Combinator -> Combinator
  Innermost   :: Combinator -> Combinator
  All         :: Combinator -> Combinator
  One         :: Combinator -> Combinator
  Rule        :: Derivation -> Combinator
  deriving (Eq, Show)

parseCombinator :: Parser Combinator
parseCombinator = buildExpressionParser combTable combTerm <?>
  "Combinator expression"

combTerm :: Parser Combinator
combTerm =
  P.parens lexer parseCombinator <|>
  parseKeyword lexer "nop" Nop <|>
  parseKeyword lexer "fail" Fail <|>
  (do drv <- parseDerivation; return $ Rule drv)
  <?>
  "Combinator simple term"

combTable :: Table Combinator
combTable = [
  [prefix lexer "many" Many, prefix lexer "many1" Many1,
   prefix lexer "try" Try, prefix lexer "once" Once,
   prefix lexer "top_down" Everywhere,
   prefix lexer "bottom_up" Everywhere',
   prefix lexer "innermost" Innermost,
   prefix lexer "all" All, prefix lexer "one" One],
  [binary lexer "|" Choice AssocRight,
   binary lexer "||" LChoice AssocRight],
  [binary lexer "&" Seq AssocRight]
 ]

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

data Step where
  Comb        :: Combinator -> Step
  Indirect    :: Either S S -> Step
  IndirectEnd :: Step
  LeftP       :: Step
  Qed         :: Step
  RightP      :: Step
  SeqC        :: Step -> Step -> Step
  deriving (Eq, Show)

parseStep :: Parser Step
parseStep = buildExpressionParser stepTable stepTerm <?> "Step expression"

stepTerm =
  P.parens lexer parseStep <|>
  parseKeyword lexer "left" LeftP <|>
  parseKeyword lexer "right" RightP <|>
  parseKeyword lexer "qed" Qed <|>
  parseIndLeft <|>
  parseIndRight <|>
  (do comb <- parseCombinator; return $ Comb comb) <?>
  "Simple step expression"

parseIndLeft :: Parser Step
parseIndLeft = do
  P.reserved lexer "ind_left"
  ord <- parseOrder
  return $ Indirect (Left ord)

parseIndRight :: Parser Step
parseIndRight = do
  P.reserved lexer "ind_right"
  ord <- parseOrder
  return $ Indirect (Right ord)

stepTable :: Table Step
stepTable = [
  [binary lexer ">" SeqC AssocRight]
 ]
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

data Module = Module String [String] [Block]
  deriving (Show, Eq)

parseModule :: Parser Module
parseModule = do
  P.reserved lexer "module"
  ident <- P.identifier lexer
  imps <- many (do
    P.reserved lexer "import"
    imp <- P.identifier lexer
    return imp)
  mod <- parseBlock `sepEndBy` P.semi lexer
  return $ Module ident imps mod

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
lexer :: P.TokenParser st
lexer = galoisLexer reservedNames reservedOpNames
  where
    reservedNames = ["module", "import",
                     "ind_left", "ind_right", "ind_end", "left", "right", "qed",
                     "nop", "fail", "many", "many1", "try", "once", "top_down",
                     "bottom_up", "innermost", "all", "one",
                     "inv", "shunt", "distr_low", "distr_up",
                     "monot_up", "monot_low", "top_preserving",
                     "bot_preserving", "cancel_up", "cancel_low", "free",
                     "Axiom", "Theorem", "Galois", "Strategy",
                     "Id", "Top", "Bot", "Maybe", "List", "Set", "Map",
                     "Ord", "Fun"]
    reservedOpNames = [">", "&", "|", "||", "=", "<=", "*", "~", "/*\\",
                       "/\\", "\\/", ".", "><", "-|-"]
