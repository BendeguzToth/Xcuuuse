module Xcuuuse (Formula(..),
                Scope(..),
                Proof(..),
                Line(..),
                Rule(..),
                Reference(..), 
                prettyPrint,
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

-- lineNumber :: Line -> Int
-- lineNumber (Premise i _) = i
-- lineNumber (Derivation i _ _ _) = i

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
    (Disjunction f1 f2) == (Conjunction e1 e2)  | f1 == e1 && f2 == e2 = True
                                                | f1 == e2 && f2 == e1 = True
                                                | otherwise = False
    (Implication p1 c1) == (Implication p2 c2) = p1 == p2 && c1 == c2
    (Equivalence f1 f2) == (Equivalence e1 e2) = f1 == e1 && f2 == e2
    _ == _ = False


prettyPrint (Var s) = s
prettyPrint (Negation f) = "(" ++ "-" ++ (prettyPrint f) ++ ")"
prettyPrint (Conjunction f1 f2) = "(" ++ (prettyPrint f1) ++ "/\\" ++ (prettyPrint f2) ++ ")"
prettyPrint (Disjunction f1 f2) = "(" ++ (prettyPrint f1) ++ "\\/" ++ (prettyPrint f2) ++ ")"
prettyPrint (Implication f1 f2) = "(" ++ (prettyPrint f1) ++ "=>" ++ (prettyPrint f2) ++ ")"
prettyPrint (Equivalence f1 f2) = "(" ++ (prettyPrint f1) ++ "<=>" ++ (prettyPrint f2) ++ ")"


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

-- Checking if line numbering is done the correct way.
checkNumbering :: Proof -> Bool
checkNumbering (Proof p c d) = let  pl = length p
                                    premOk = all id (map (\(p', n') -> lineNumber p' == n') (zip p [1..]))
                                    derOk  = all id (map (\(d', n') -> lineNumber d' == n') (zip d [1+pl..]))
                                    in premOk && derOk


                                    
type Depth = Int
type LineIndex = Int
type BoxIndex = (Int, Int)


error' :: Line -> String -> String
error' line msg = "Error on line " ++ (show $ lineNumber line) ++ ". " ++ msg

data Box =  Open {lines :: (M.Map LineIndex Line), boxes :: (M.Map BoxIndex Box), opened :: (Maybe Box)}
            | Closed {assumption :: Line, local_conclusion :: Line} deriving(Show)

emptyBox :: Box
emptyBox = Open M.empty M.empty Nothing

initBox :: [Line] -> Box
initBox ls = Open (M.fromList (map (\l->(lineNumber l, l)) ls)) M.empty Nothing

depth :: Box -> Depth
depth (Open _ _ Nothing) = 0
depth (Open _ _ (Just ob)) = 1 + depth ob

invalidRule :: Line -> String
invalidRule l = error' l "Invalid application of rule."

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
checkLine (Derivation _ _ f (ConjunctionI r1 r2)) box = do
                        f1 <- ffr r1 box
                        f2 <- ffr r2 box
                        Right $ conjunctionIntroduction f f1 f2
checkLine (Derivation _ _ f (ConjunctionE r)) box = do
                        f1 <- ffr r box
                        Right $ conjunctionElimination f f1
checkLine (Derivation _ _ f (DoubleNegationI r)) box = do
                        f1 <- ffr r box
                        Right $ doubleNegationIntroduction f f1
checkLine (Derivation _ _ f (DoubleNegationE r)) box = do
                        f1 <- ffr r box
                        Right $ doubleNegationElimination f f1

-- Formula from reference
ffr :: Reference -> Box -> Either String Formula
ffr (SingleLine n) b = case getLine' b n of
    Right (Derivation _ _ f _) -> return f
    Right (Premise _ f) -> return f
    Right (Conclusion f) -> return f
    Left s -> Left $ s


check :: Proof -> Either String Bool
check p = do
    let acc = initBox (premises p)
        result = case foldM update acc (derivations p) of
            (Right bx) -> if  (formula . local_conclusion) (close bx) == (formula . conclusion) p
                        then Right $ True
                        else Left $ "Proof is incomplete."
            (Left s) -> Left s
    result


{- BOXES -}

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
        False -> Left $ "Invalid reference to box " ++ show idx
getBox (Open _ bs (Just bx)) idx = 
    case M.member idx bs of
        True -> Right $ bs M.! idx
        False -> getBox bx idx
getBox _ _ = error "getBox called on Closed."