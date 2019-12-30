module Xcuuuse (Formula(..),
                Scope(..),
                Proof(..),
                Line(..),
                Rule(..),
                Reference(..), 
                checkNumbering,
                update,
                emptyBox,
                check) where
import Data.Either
import Control.Monad
import qualified Data.Map as M

data Proof = Proof {premises :: [Line], conclusion :: Line, derivations :: [Line]} deriving(Show)

data Line =     Premise {lineNumber :: LineIndex, formula:: Formula}
                | Conclusion {formula :: Formula}
                | Derivation {lineNumber :: LineIndex, scope :: Scope, formula :: Formula, rule :: Rule} deriving(Show)

data Formula =  Var String 
                | Negation Formula 
                | Conjunction Formula Formula
                | Disjunction Formula Formula
                | Implication Formula Formula
                | Equivalence Formula Formula 
                | Contradiction deriving(Show)

data Scope = Start Int | Continue Int deriving(Show)

data Rule = ConjunctionI Reference Reference
            | ConjunctionE Reference
            | DoubleNegationI Reference
            | DoubleNegationE Reference
            | ImplicationI Reference
            | ImplicationE Reference Reference
            | EquivalenceI Reference Reference
            | EquivalenceE Reference
            | ModusTollens Reference Reference
            | DisjunctionI Reference
            | DisjunctionE Reference Reference Reference
            | Reiterate Reference
            | ContradictionE Reference
            | NegationI Reference
            | NegationE Reference Reference
            | PBC Reference
            | LEM 
            | Assumption deriving(Show)

data Reference = SingleLine Int | Range Int Int deriving(Show)

instance Eq Formula where
    (Var s1) == (Var s2) = s1 == s2
    (Negation f1) == (Negation f2) = f1 == f2
    (Conjunction f1 f2) == (Conjunction e1 e2)  | f1 == e1 && f2 == e2 = True
                                                | f1 == e2 && f2 == e1 = True
                                                | otherwise = False
    (Disjunction f1 f2) == (Disjunction e1 e2)  | f1 == e1 && f2 == e2 = True
                                                | f1 == e2 && f2 == e1 = True
                                                | otherwise = False
    (Implication p1 c1) == (Implication p2 c2) = p1 == p2 && c1 == c2
    (Equivalence f1 f2) == (Equivalence e1 e2)  | f1 == e1 && f2 == e2 = True
                                                | f1 == e2 && f2 == e1 = True
    Contradiction == Contradiction = True
    _ == _ = False

{- 
    Functions to evaluate the specific rules. The first argument
    is always the line at which the reference is found, the remaining
    formulas are of the referenced lines. 
-}
conjunctionIntroduction :: Formula -> Formula -> Formula -> Bool
conjunctionIntroduction f p1 p2 = Conjunction p1 p2 == f

conjunctionElimination :: Formula -> Formula -> Bool
conjunctionElimination c (Conjunction p1 p2) = c == p1 || c == p2
conjunctionElimination _ _ = False

doubleNegationIntroduction :: Formula -> Formula -> Bool
doubleNegationIntroduction f p = Negation (Negation p) == f

doubleNegationElimination :: Formula -> Formula -> Bool
doubleNegationElimination f (Negation (Negation p)) = f == p
doubleNegationElimination _ _ = False

implicationIntroduction :: Formula -> Formula -> Formula -> Bool
implicationIntroduction f f1 f2 = f == Implication f1 f2

implicationElimination :: Formula -> Formula -> Formula -> Bool
implicationElimination f f1 f2 =    f1 == Implication f2 f
                                    || f2 == Implication f1 f

equivalenceIntroduction :: Formula -> Formula -> Formula -> Bool
equivalenceIntroduction (Equivalence p q) (Implication a b) (Implication c d) = p == a && p == d && q == b && q == c
                                                                                || p == b && p ==c && q == a && q == d
equivalenceIntroduction _ _ _ = False

equivalenceElimination :: Formula -> Formula -> Bool
equivalenceElimination (Implication p q) (Equivalence a b) = p == a && q == b || p == b && q == a
equivalenceElimination _ _ = False

modusTollens :: Formula -> Formula -> Formula -> Bool
modusTollens (Negation p') (Implication p q) (Negation q') = q == q' && p == p'
modusTollens (Negation p') (Negation q') (Implication p q) = q == q' && p == p'
modusTollens _ _ _ = False

disjunctionIntroduction :: Formula -> Formula -> Bool
disjunctionIntroduction (Disjunction p q) f = f == p || f == q
disjunctionIntroduction _ _ = False

-- 1. formula: The target line. 2. formula: single line ref. 3-6. formulas: boxes (start, end), (start, end)
disjunctionElimination :: Formula -> Formula -> Formula -> Formula -> Formula -> Formula -> Bool
disjunctionElimination x (Disjunction p q) p' x' q' x'' = x == x' && x == x'' && p == p' && q == q'
                                                        || x == x' && x == x'' && p == q' && q == p'
disjunctionElimination _ _ _ _ _ _ = False

reiterate :: Formula -> Formula -> Bool
reiterate f f1 = f == f1

contradictionElimination :: Formula -> Formula -> Bool
contradictionElimination f Contradiction = True
contradictionElimination _ _ = False

negationIntroduction :: Formula -> Formula -> Formula -> Bool
negationIntroduction (Negation p) p' Contradiction = p == p'
negationIntroduction _ _ _ = False

negationElimination :: Formula -> Formula -> Formula -> Bool
negationElimination Contradiction p (Negation p') = p == p'
negationElimination Contradiction (Negation p') p = p == p'
negationElimination _ _ _ = False

pbc :: Formula -> Formula -> Formula -> Bool
pbc p (Negation p') Contradiction = p == p'
pbc _ _ _ = False

lem :: Formula -> Bool
lem (Conjunction p (Negation p')) = p == p'
lem _ = False


error' :: Line -> String -> String
error' line msg = "Error on line " ++ (show $ lineNumber line) ++ ". " ++ msg

invalidRule :: Line -> String
invalidRule l = error' l "Invalid application of rule with the lines cited."

update :: Box -> Line -> Either String Box
update b line@(Derivation n (Start s) f r)      | s == (depth b) + 1 = do
                                                        chc <- checkLine line b
                                                        if chc then Right $ open b line
                                                        else Left $ invalidRule line
                                                | s == (depth b) = do
                                                        let closed = close b
                                                        chc <- checkLine line closed
                                                        if chc then Right $ open closed line
                                                        else Left $ invalidRule line
                                                | otherwise = Left (error' line "Incorrect scoping.")
update b line@(Derivation n (Continue c) f r)   | c == (depth b) = do
                                                        chc <- checkLine line b
                                                        if chc then Right $ continue b line
                                                        else Left $ invalidRule line
                                                | c == (depth b) - 1 = do
                                                        let closed =  close b
                                                        chc <- checkLine line closed
                                                        if chc then Right $ continue closed line
                                                        else Left $ invalidRule line
                                                | otherwise = Left (error' line "Incorrect scoping.")

checkLine :: Line -> Box -> Either String Bool
checkLine (Derivation _ (Start _) f Assumption) box = Right True
-- Here we handle assumptions that are not at the top of a box,
-- and other rules that might occur at the top of a box (even though they should not).
checkLine line@(Derivation _ (Continue _) f Assumption) box = Left $ error' line "Assumption needs to be the first line of a box."
checkLine line@(Derivation _ (Start _) _ _) _ = Left $ error' line "A box needs to start with an assumption."

checkLine l@(Derivation _ _ f (ConjunctionI r1@(SingleLine _) r2@(SingleLine _))) box = do
                        f1 <- ffr r1 box l
                        f2 <- ffr r2 box l
                        Right $ conjunctionIntroduction f f1 f2
checkLine l@(Derivation _ _ f (ConjunctionE r@(SingleLine _))) box = do
                        f1 <- ffr r box l
                        Right $ conjunctionElimination f f1
checkLine l@(Derivation _ _ f (DoubleNegationI r@(SingleLine _))) box = do
                        f1 <- ffr r box l
                        Right $ doubleNegationIntroduction f f1
checkLine l@(Derivation _ _ f (DoubleNegationE r@(SingleLine _))) box = do
                        f1 <- ffr r box l
                        Right $ doubleNegationElimination f f1
checkLine l@(Derivation _ _ f (ImplicationI r@(Range _ _))) box = do
                        (f1, f2) <- fsfr r box l
                        Right $ implicationIntroduction f f1 f2
checkLine l@(Derivation _ _ f (ImplicationE r1@(SingleLine _) r2@(SingleLine _))) box = do
                        f1 <- ffr r1 box l
                        f2 <- ffr r2 box l
                        Right $ implicationElimination f f1 f2
checkLine l@(Derivation _ _ f (EquivalenceI r1@(SingleLine _) r2@(SingleLine _))) box = do
                        f1 <- ffr r1 box l
                        f2 <- ffr r2 box l
                        Right $ equivalenceIntroduction f f1 f2
checkLine l@(Derivation _ _ f (EquivalenceE r@(SingleLine _))) box = do
                        f1 <- ffr r box l
                        Right $ equivalenceElimination f f1
checkLine l@(Derivation _ _ f (ModusTollens r1@(SingleLine _) r2@(SingleLine _))) box = do
                        f1 <- ffr r1 box l
                        f2 <- ffr r2 box l
                        Right $ modusTollens f f1 f2
checkLine l@(Derivation _ _ f (DisjunctionI r@(SingleLine _))) box = do
                        f1 <- ffr r box l
                        Right $ disjunctionIntroduction f f1
-- We need to make sure that the first reference is the SingleLine, and then the ranges.
checkLine l@(Derivation _ _ f (DisjunctionE sl@(SingleLine _) r1@(Range _ _) r2@(Range _ _))) box = do
                        f1 <- ffr sl box l
                        (f2, f3) <- fsfr r1 box l
                        (f4, f5) <- fsfr r2 box l
                        Right $ disjunctionElimination f f1 f2 f3 f4 f5
checkLine l@(Derivation n s f (DisjunctionE r1@(Range _ _) sl@(SingleLine _) r2@(Range _ _))) box = 
                        checkLine (Derivation n s f (DisjunctionE sl r1 r2)) box
checkLine l@(Derivation n s f (DisjunctionE r1@(Range _ _) r2@(Range _ _) sl@(SingleLine _))) box = 
                        checkLine (Derivation n s f (DisjunctionE sl r1 r2)) box
checkLine l@(Derivation _ _ f (DisjunctionE _ _ _)) _ = Left $ invalidRule l
checkLine l@(Derivation _ _ f (Reiterate r@(SingleLine _))) box = do 
                        f1 <- ffr r box l
                        Right $ reiterate f f1
checkLine l@(Derivation _ _ f (ContradictionE r@(SingleLine _))) box = do
                        f1 <- ffr r box l
                        Right $ contradictionElimination f f1
checkLine l@(Derivation _ _ f (NegationI r@(Range _ _))) box = do
                        (f1, f2) <- fsfr r box l
                        Right $ negationIntroduction f f1 f2
checkLine l@(Derivation _ _ f (NegationE r1@(SingleLine _) r2@(SingleLine _))) box = do
                        f1 <- ffr r1 box l
                        f2 <- ffr r2 box l
                        Right $ negationElimination f f1 f2
checkLine l@(Derivation _ _ f (PBC r@(Range _ _))) box = do
                        (f1, f2) <- fsfr r box l
                        Right $ pbc f f1 f2
checkLine l@(Derivation _ _ f (LEM)) box = do
                        Right $ lem f
checkLine l _ = Left $ invalidRule l

-- Formula from reference
ffr :: Reference -> Box -> Line -> Either String Formula
ffr (SingleLine n) b l = case getLine' b n of
    Right f -> return $ formula f
    Left s -> Left $ error' l s

-- Formulas from reference
fsfr :: Reference -> Box -> Line -> Either String (Formula, Formula)
fsfr (Range a c) b l = case getBox b (a, c) of
    Right bx -> return ((formula . assumption) bx, (formula . local_conclusion) bx)
    Left s -> Left $ error' l s


check :: Proof -> Either String Bool
check p = do
    let acc = initBox (premises p)
        result = case foldM update acc (derivations p) of
            (Right bx) -> if depth bx /= 0 then Left "Proof is incomplete." else
                            if  (formula . local_conclusion) (close bx) == (formula . conclusion) p
                                then Right $ True
                            else Left $ "Proof is incomplete."
            (Left s) -> Left s
    result


-- Checking if line numbering is done the correct way.
checkNumbering :: Proof -> Bool
checkNumbering (Proof p c d) = let  pl = length p
                                    premOk = all id (map (\(p', n') -> lineNumber p' == n') (zip p [1..]))
                                    derOk  = all id (map (\(d', n') -> lineNumber d' == n') (zip d [1+pl..]))
                                    in premOk && derOk


{- BOXES -}

type Depth = Int
type LineIndex = Int
type BoxIndex = (Int, Int)

data Box =  Open {lines :: (M.Map LineIndex Line), boxes :: (M.Map BoxIndex Box), opened :: (Maybe Box)}
            | Closed {assumption :: Line, local_conclusion :: Line} deriving(Show)

emptyBox :: Box
emptyBox = Open M.empty M.empty Nothing

initBox :: [Line] -> Box
initBox ls = Open (M.fromList (map (\l->(lineNumber l, l)) ls)) M.empty Nothing

depth :: Box -> Depth
depth (Open _ _ Nothing) = 0
depth (Open _ _ (Just ob)) = 1 + depth ob

-- Opens a new box, containing line.
open :: Box -> Line -> Box
open (Open l b Nothing) line = Open l b (Just $ initBox [line])
open (Open l b (Just ob)) line = Open l b (Just $ open ob line)
open _ _ = error "'add' called on Closed."

-- Closes the current box.
close :: Box -> Box
close (Open l b Nothing) = Closed ((snd . M.findMin) l) ((snd . M.findMax) l)
close (Open l b (Just ob)) = case close ob of
                                cls@(Closed _ _) -> Open l (M.insert ((lineNumber . assumption) cls, (lineNumber . local_conclusion) cls) cls b) Nothing
                                op@(Open _ _ _) -> Open l b (Just op)
close _ = error "'close' called on already closed."

-- Appends the line to the currently active box.
continue :: Box -> Line -> Box
continue (Open l b Nothing) line = Open (M.insert (lineNumber line) line l) b Nothing
continue (Open l b (Just ob)) line = Open l b (Just $ continue ob line)

{-
    Gets a line/box from the environment.
-}

getLine' :: Box -> LineIndex -> Either String Line
getLine' (Open l _ Nothing) idx =
    case M.member idx l of
        True -> Right $ l M.! idx
        False -> Left $ "Reference to inaccessible line: " ++ show idx
getLine' (Open l _ (Just bx)) idx = 
    case M.member idx l of
        True -> Right $ l M.! idx
        False -> getLine' bx idx
getLine' _ _ = error "getLine called on Closed."


getBox :: Box -> BoxIndex -> Either String Box
getBox (Open _ bs Nothing) idx =
    case M.member idx bs of
        True -> Right $ bs M.! idx
        False -> Left $ "Invalid reference to box: " ++ (show.fst) idx ++ "-" ++ (show.snd) idx
getBox (Open _ bs (Just bx)) idx = 
    case M.member idx bs of
        True -> Right $ bs M.! idx
        False -> getBox bx idx
getBox _ _ = error "getBox called on Closed."
