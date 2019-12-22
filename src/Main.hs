module Main where
import Prelude hiding ((<*>), (<$>), (<*), (*>), (<$))
import Lexing
import Parsing

main :: IO ()
main = do
    x <- readFile "../data/test.proof"
    let tokens = parse tokenize x
        ast = pProof tokens
    print ast
    return ()
