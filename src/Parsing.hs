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
        -- Hyphen is overloaded.
        <|> Negation <$ symbol Tneg <*> prec5 <|> Negation <$ symbol Thyphen <*> prec5
        <|> Contradiction <$ symbol Tcont

pFormula :: Parser Token Formula
pFormula = prec1

pNat :: Parser Token Int
pNat = (\(Tnum n)-> n) <$> satisfy isNum

pRuleType :: Parser Token RuleType
pRuleType = Introduction <$ symbol Tintroduction <|> Elimination <$ symbol Telimination

pRuleStep :: Parser Token RuleStep
pRuleStep = RNegation <$ symbol Tneg <|> RConjunction <$ symbol Tcon <|> RDisjunction <$ symbol Tdis <|> RImplication <$ symbol Timp <|> REquivalence <$ symbol Teq

pRange :: Parser Token Reference
pRange = Range <$> pNat <* symbol Thyphen <*> pNat

pSingleRef :: Parser Token Reference
pSingleRef = SingleLine <$> pNat

pReferenceList :: Parser Token [Reference]
pReferenceList = listOf (pRange <|> pSingleRef) (symbol Tcomma)

pJustification :: Parser Token Justification
pJustification = Justification <$> pRuleStep <*> pRuleType <*> pReferenceList

pPremise :: Parser Token Line
pPremise = Premise <$> pNat <*> pFormula <* symbol Tpremise

pConclusion :: Parser Token Line
pConclusion = Conclusion <$ symbol Tvertical <* symbol Thyphen <*> pFormula

pDerivation :: Parser Token Line
pDerivation = Derivation <$> pNat <*> pScope <*> pFormula <*> pJustification

pLine :: Parser Token Line
pLine = (pPremise <|> pConclusion <|> pDerivation) <* greedy (symbol Tnewline)

pScope :: Parser Token Scope
pScope =    Continue . length <$> greedy (symbol Tvertical)
            <|> Start . length <$> greedy (symbol Tvertical) <* symbol Tstar

pProof :: Parser Token Proof
pProof = Proof <$> many pLine <* eof ()

isTvar (Tvar _) = True
isTvar _ = False

isNum (Tnum _) = True
isNum _ = False


parse :: Parser s a -> [s] -> a
parse p s = let ps = p s
                (x, r) = head $ ps
            in if length ps == 1 then x else error "Ambiguous grammar."

fullParse xs = parse pFormula (parse tokenize xs)