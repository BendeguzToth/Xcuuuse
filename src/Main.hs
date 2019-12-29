module Main where
import Prelude hiding ((<*>), (<$>), (<*), (*>), (<$))
import Lexing
import Parsing
import Xcuuuse

main :: IO ()
main = do
    x <- readFile "../data/test.proof"
    let tokens = parse tokenize x
        ast = parse pProof tokens
    print ast
    -- print $ checkNumbering ast
    return ()
