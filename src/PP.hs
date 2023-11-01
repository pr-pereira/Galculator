
module PP where

import Language.R.Syntax

parens :: ShowS -> String -> String
parens = showParen True 

pretty' :: R r -> String
pretty' r = pretty r ""

pretty :: R r -> ShowS
pretty BOT = showString "TT"
pretty TOP = showString "_||_"
pretty (NEG r) = showString "~" . pretty r
pretty (MEET r s) = parens $ pretty r . showString " /\\ " . pretty s
pretty (JOIN r s) = parens $ pretty r . showString " \\/ " . pretty s
pretty ID = showString "id"
pretty (CONV r) = pretty r . showString "*"
pretty (COMP _ r s) = parens $ pretty r . showString " . " . pretty s
pretty (SPLIT r s) = 
  showString "<" . pretty r . showString ", " . pretty s . showString ">"
pretty (ORD o) = pretty o
pretty (FUN f) = pretty f
pretty (LEFTSEC _ f s) = 
  parens $ showString "<" . pretty s . showString "> " . pretty f 
pretty (RIGHTSEC _ f s) =
  parens $ pretty f . showString "<" . pretty s . showString ">"
pretty (APPLY _ r v) = 
  parens $ pretty r . showString " " . pretty v
pretty (DEF nm t) = showString nm
pretty (Var nm) = showString nm
pretty (PROD r s) = parens $ pretty r . showString " >< " . pretty s
pretty (EITHER r s) = parens $ pretty r . showString " -|- " . pretty s
pretty (MAYBE r) = showString ""
pretty (LIST r) = showString ""
pretty (SET r) =showString ""
pretty (MAP r) =showString ""
pretty (REYNOLDS r s) =showString ""
pretty FId = showString "id"
pretty (FComp _ f g) = parens $ pretty f . showString " . " . pretty g
pretty OId = showString ""
pretty (OComp o1 o2) = showString ""
pretty (OConv o) = showString ""
pretty (OProd o) = showString ""
pretty (OJoin o) = showString ""
pretty (OMeet o) = showString ""
pretty (OMax o) = showString ""
pretty (OMin o) = showString ""
pretty (GDef nm f g fo go) = showString ""
pretty GId = showString ""
pretty (GComp _ g1 g2) = showString ""
pretty (GConv g) = showString ""
pretty (GLAdj g) = showString ""
pretty (GUAdj g) = showString ""
pretty (GLOrd _ g) = showString ""
pretty (GUOrd _ g) = showString ""

