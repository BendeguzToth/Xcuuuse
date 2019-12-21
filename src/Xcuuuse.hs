module Xcuuuse (Formula(..)) where

data Formula =  Var String 
                | Negation Formula 
                | Conjunction Formula Formula
                | Disjunction Formula Formula
                | Implication Formula Formula
                | Equivalence Formula Formula

instance Eq Formula where
    (Var s1) == (Var s2) = s1 == s2
    (Negation f1) == (Negation f2) = f1 == f2
    (Conjunction f1 f2) == (Conjunction e1 e2)  | f1 == e1 && f2 == e2 = True
                                                | f1 == e2 && f2 == e1 = True
                                                | otherwise = False
    (Disjunction f1 f2) == (Conjunction e1 e2)  | f1 == e1 && f2 == e2 = True
                                                | f1 == e2 && f2 == e1 = True
                                                | otherwise = False
    (Implication p1 c1) == (Implication p2 c2) = p1 == p2 && c1 == c2
    (Equivalence f1 f2) == (Equivalence e1 e2) = f1 == e1 && f2 == e2
    _ == _ = False

instance Show Formula where
    show (Var s) = s
    show (Negation f) = "-" ++ (show f)
    show (Conjunction f1 f2) = (show f1) ++ "/\\" ++ (show f2)
    show (Disjunction f1 f2) = (show f1) ++ "\\/" ++ (show f2)
    show (Implication f1 f2) = (show f1) ++ "=>" ++ (show f2)
    show (Equivalence f1 f2) = (show f1) ++ "<=>" ++ (show f2)


conjunctionIntroduction :: Formula -> Formula -> Formula -> Bool
conjunctionIntroduction p1 p2 f = Conjunction p1 p2 == f

conjunctionElimination :: Formula -> Formula -> Bool
conjunctionElimination (Conjunction p1 p2) c = c == p1 || c == p2
conjunctionElimination _ _ = False

doubleNegationIntroduction :: Formula -> Formula -> Bool
doubleNegationIntroduction p f = Negation (Negation p) == f

doubleNegationElimination :: Formula -> Formula -> Bool
doubleNegationElimination (Negation (Negation p)) f = f == p
doubleNegationElimination _ _ = False
