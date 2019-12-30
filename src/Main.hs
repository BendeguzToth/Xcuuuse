module Main where
import Prelude hiding ((<*>), (<$>), (<*), (*>), (<$))
import Lexing
import Parsing
import Xcuuuse
import Control.Monad
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    proof <- readFile (head args)
    putStrLn $ process proof
    return ()

process :: String -> String
process proof = case parse tokenize proof of
    (Left s) -> s
    (Right tokens) -> case parse pProof tokens of
        (Left s) -> s
        (Right ast) -> if checkNumbering ast then
            case check ast of
                (Right _) -> "Proof is correct."
                (Left e) -> e
                        else "Incorrect line numbering."
