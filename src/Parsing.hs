module Parsing where

import Prelude hiding ((<*>), (<$>), (<*), (*>), (<$))
import Lexing
import Xcuuuse
import ParserLib

prec1 :: Parser Token Formula
prec1 = chainr prec2 (Equivalence <$ symbol Teq)

prec2 :: Parser Token Formula
prec2 = chainr prec3 (Implication <$ symbol Timp)

prec3 :: Parser Token Formula
prec3 = chainl prec4 (Disjunction <$ symbol Tdis)

prec4 :: Parser Token Formula
prec4 = chainl prec5 (Conjunction <$ symbol Tcon)

prec5 :: Parser Token Formula
prec5 = (\(Tvar s) ->Var s) <$> satisfy isTvar 
        <|> symbol Topen *> prec1 <* symbol Tclose 
        <|> Negation <$ symbol Tneg <*> prec5

pFormula :: Parser Token Formula
pFormula = prec1

pNat :: Parser Token Int
pNat = (\(Tnum n)-> n) <$> satisfy isNum

pRuleType :: Parser Token RuleType
pRuleType = Introduction <$ symbol Tintroduction <|> Elimination <$ symbol Telimination

pRuleStep :: Parser Token RuleStep
pRuleStep = RNegation <$ symbol Tneg <|> RConjunction <$ symbol Tcon <|> RDisjunction <$ symbol Tdis <|> RImplication <$ symbol Timp <|> REquivalence <$ symbol Teq

pReference :: Parser Token Reference
pReference = (\x->ListReference (map (\(Tnum n) -> n) x)) <$> listOf (satisfy isNum) (symbol Tcomma)

pJustification :: Parser Token Justification
pJustification = Justification <$> pRuleStep <*> pRuleType <*> pReference

pPremise :: Parser Token Line
pPremise = Premise <$> pNat <* symbol Tpremise <*> pFormula 

pConclusion :: Parser Token Line
pConclusion = Conclusion <$ symbol Tthen <*> pFormula

pDerivation :: Parser Token Line
pDerivation = Derivation <$> pNat <*> pFormula <* symbol Tsemicolon <*> pJustification

pLine :: Parser Token Line
pLine = (pPremise <|> pConclusion <|> pDerivation) <* greedy (symbol Tnewline)

pProof :: Parser Token Proof
pProof = Proof <$> many pLine <* eof ()

isTvar (Tvar _) = True
isTvar _ = False

isNum (Tnum _) = True
isNum _ = False


parse :: Parser s a -> [s] -> a
parse p s = let (x, r) = head $ p s
            in x

fullParse xs = parse pFormula (parse tokenize xs)