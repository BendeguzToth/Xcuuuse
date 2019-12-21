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
        <|> Parenthesised <$ symbol Topen <*> prec1 <* symbol Tclose 
        <|> Negation <$ symbol Tneg <*> prec5


isTvar (Tvar _) = True
isTvar _ = False


parse :: Parser s a -> [s] -> a
parse p s = let (x, r) =head $ p s
            in x
