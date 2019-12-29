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
        <|> Negation <$ pNegation <*> prec5
        <|> Contradiction <$ symbol Tcont

pFormula :: Parser Token Formula
pFormula = prec1

pNat :: Parser Token Int
pNat = foldl (\acc (Tnum n)-> 10*acc + n) 0 <$> greedy' (satisfy isNum)

-- This is needed because the symbol '-' is overloaded in range.
-- Negation can be either one of the sepcified symbols, or '-'.
pNegation :: Parser Token Token
pNegation = symbol Tneg <|> symbol Thyphen

pRange :: Parser Token Reference
pRange = Range <$> pNat <* symbol Thyphen <*> pNat <* option (symbol Tcomma) Tcomma

pSingleRef :: Parser Token Reference
pSingleRef = SingleLine <$> pNat <* option (symbol Tcomma) Tcomma

pRule :: Parser Token Rule
pRule = ConjunctionI <$ symbol Tcon <* symbol Tintroduction <*> pSingleRef <*> pSingleRef
        <|> ConjunctionE <$ symbol Tcon <* symbol Telimination <*> pSingleRef
        <|> DoubleNegationI <$ pNegation <* pNegation <* symbol Tintroduction <*> pSingleRef
        <|> DoubleNegationE <$ pNegation <* pNegation <* symbol Telimination <*> pSingleRef
        <|> ImplicationI <$ symbol Timp <* symbol Tintroduction <*> pRange
        <|> ImplicationE <$ symbol Timp <* symbol Telimination <*> pSingleRef <*> pSingleRef
        <|> ModusTollens <$ symbol Tmt <*> pSingleRef <*> pSingleRef
        <|> DisjunctionI <$ symbol Tdis <* symbol Tintroduction <*> pSingleRef
        <|> DisjunctionE <$ symbol Tdis <* symbol Telimination <*> pSingleRef <*> pRange <*> pRange
        <|> Reiterate <$ symbol Treiterate <*> pSingleRef
        <|> ContradictionE <$ symbol Tcont <* symbol Telimination <*> pSingleRef
        <|> NegationI <$ pNegation <* symbol Tintroduction <*> pRange
        <|> NegationE <$ pNegation <* symbol Telimination <*> pSingleRef <*> pSingleRef
        <|> PBC <$ symbol Tpbc <*> pRange
        <|> LEM <$ symbol Tlem
        <|> Assumption <$ symbol Tassumption

pPremise :: Parser Token Line
pPremise = Premise <$> pNat <*> pFormula <* symbol Tpremise

pConclusion :: Parser Token Line
pConclusion = Conclusion <$ symbol Tvertical <* symbol Thyphen <*> pFormula

pDerivation :: Parser Token Line
pDerivation = Derivation <$> pNat <*> pScope <*> pFormula <*> pRule

pScope :: Parser Token Scope
pScope =    Continue . length <$> greedy (symbol Tvertical)
            <|> Start . length <$> greedy (symbol Tvertical) <* symbol Tstar

pProof :: Parser Token Proof
pProof = Proof <$> many (pPremise <* greedy (symbol Tnewline)) <*> (pConclusion <* greedy (symbol Tnewline)) <*> some (pDerivation <* greedy (symbol Tnewline)) <* eof ()

isTvar (Tvar _) = True
isTvar _ = False

isNum (Tnum _) = True
isNum _ = False


parse :: Parser s a -> [s] -> a
parse p s = let ps = p s
                (x, r) = head $ ps
            in if length ps == 1 || length ps == 0 then x else error "Ambiguous grammar."

fullParse xs = parse pFormula (parse tokenize xs)