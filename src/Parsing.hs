module Parsing where

import Prelude hiding ((<*>), (<$>), (<*), (*>), (<$))
import Lexing
import Xcuuuse
import ParserLib

{-
    Parsing different operator priorities of a 
    propositional formula
-}

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
        <|> Contradiction <$ symbol Tcont where
            isTvar (Tvar _) = True
            isTvar _ = False

pFormula :: Parser Token Formula
pFormula = prec1

pNat :: Parser Token Int
pNat = foldl (\acc (Tnum n)-> 10*acc + n) 0 <$> greedy' (satisfy isNum) where
    isNum (Tnum _) = True
    isNum _ = False

-- This is needed because the symbol '-' is overloaded in range.
-- Negation can be either one of the sepcified symbols, or '-'.
pNegation :: Parser Token Token
pNegation = symbol Tneg <|> symbol Thyphen

pRange :: Parser Token Reference
pRange = Range <$ option (symbol Tcomma) Tcomma <*> pNat <* symbol Thyphen <*> pNat

pSingleRef :: Parser Token Reference
pSingleRef = SingleLine <$ option (symbol Tcomma) Tcomma <*> pNat

pRef :: Parser Token Reference
pRef = pRange <|> pSingleRef

pRule :: Parser Token Rule
pRule = ConjunctionI <$ symbol Tcon <* symbol Tintroduction <*> pRef <*> pRef
        <|> ConjunctionE <$ symbol Tcon <* symbol Telimination <*> pRef
        <|> DoubleNegationI <$ pNegation <* pNegation <* symbol Tintroduction <*> pRef
        <|> DoubleNegationE <$ pNegation <* pNegation <* symbol Telimination <*> pRef
        <|> ImplicationI <$ symbol Timp <* symbol Tintroduction <*> pRef
        <|> ImplicationE <$ symbol Timp <* symbol Telimination <*> pRef <*> pRef
        <|> EquivalenceI <$ symbol Teq <* symbol Tintroduction <*> pRef <*> pRef
        <|> EquivalenceE <$ symbol Teq <* symbol Telimination <*> pRef
        <|> ModusTollens <$ symbol Tmt <*> pRef <*> pRef
        <|> DisjunctionI <$ symbol Tdis <* symbol Tintroduction <*> pRef
        <|> DisjunctionE <$ symbol Tdis <* symbol Telimination <*> pRef <*> pRef <*> pRef
        <|> Reiterate <$ symbol Treiterate <*> pRef
        <|> ContradictionE <$ symbol Tcont <* symbol Telimination <*> pRef
        <|> NegationI <$ pNegation <* symbol Tintroduction <*> pRef
        <|> NegationE <$ pNegation <* symbol Telimination <*> pRef <*> pRef
        <|> PBC <$ symbol Tpbc <*> pRef
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

parse :: Parser s a -> [s] -> Either String a
parse p s = let ps = p s
                (x, r) = head $ ps
            in if length ps == 1 then return x else
                if length ps == 0 then Left  "Parser error. Probable cause: Not the correct number of references."
                else error "Ambiguous grammar."
