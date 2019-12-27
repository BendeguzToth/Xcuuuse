module Boxes where 
import Data.Either
import qualified Data.Map as M

type Depth = Int
type Formula = String

data Line = Derivation Int Scope Formula deriving(Show)
data Scope = Start Int | Continue Int deriving(Show)

error' :: Line -> String -> String
error' line msg = "Error on line " ++ (show $ lineNumber line) ++ ". " ++ msg

lineNumber :: Line -> Int
lineNumber (Derivation i _ _) = i

data Box =  Open {lines :: (M.Map Int Line), boxes :: (M.Map (Int, Int) Box), opened :: (Maybe Box)}
            | Closed {assumption :: Line, conclusion :: Line} deriving(Show)

emptyBox :: Box
emptyBox = Open M.empty M.empty Nothing

initBox :: Line -> Box
initBox l = Open (M.fromList [(lineNumber l, l)]) M.empty Nothing

depth :: Box -> Depth
depth (Open _ _ Nothing) = 0
depth (Open _ _ (Just ob)) = 1 + depth ob

update :: Box -> Line -> Either String Box
update b line@(Derivation n (Start s) f)    | s == (depth b) + 1 = Right $ open b line
                                            | s == (depth b) = Right $ open (close b) line
                                            | otherwise = Left (error' line "Incorrect scoping.")
update b line@(Derivation n (Continue c) f) | c == (depth b) = Right $ continue b line
                                            | c == (depth b) - 1 = Right $ continue (close b) line
                                            | otherwise = Left (error' line "Incorrect scoping.")

-- Opens a new box, containing line.
open :: Box -> Line -> Box
open (Open l b Nothing) line = Open l b (Just $ initBox line)
open (Open l b (Just ob)) line = Open l b (Just $ open ob line)
open _ _ = error "'add' called on Closed."

-- Closes the current box.
close :: Box -> Box
close (Open l b Nothing) = Closed ((snd . M.findMin) l) ((snd . M.findMax) l)
close (Open l b (Just ob)) = case close ob of
                                cls@(Closed _ _) -> Open l (M.insert ((lineNumber . assumption) cls, (lineNumber . conclusion) cls) cls b) Nothing
                                op@(Open _ _ _) -> op
close _ = error "'close' called on already closed."

-- Appends the line to the currently active box.
continue :: Box -> Line -> Box
continue (Open l b Nothing) line = Open (M.insert (lineNumber line) line l) b Nothing
continue (Open l b (Just ob)) line = Open l b (Just $ continue ob line)


line :: Line
line = Derivation 1 (Start 1) "Line"


func :: Either String Int
func = do
    Right 12
    Left "Error"
    Right 24