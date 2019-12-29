module Main where
import Prelude hiding ((<*>), (<$>), (<*), (*>), (<$))
import Lexing
import Parsing
import Xcuuuse
import Control.Monad
main :: IO ()
main = do
    x <- readFile "../data/distributivity.proof"
    let tokens = parse tokenize x
        ast@(Proof ps c ds) = parse pProof tokens
    print ast
    print $ checkNumbering ast
    case check ast of
        (Right _) -> print "Proof is correct."
        (Left e) -> print e
    return ()
