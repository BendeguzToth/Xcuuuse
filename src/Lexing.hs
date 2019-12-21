module Lexing where

import Prelude hiding ((<*>), (<$>), (<*), (*>), (<$))
import ParserLib

data Token = Tvar String | Tneg | Tcon | Tdis | Timp |Teq | Open | Close deriving(Show)

lVarsym :: Parser Char Token
lVarsym = (\l r-> Tvar (l:r)) <$> uppercase <*> many (digit <|> symbol '\'')

lNegsym :: Parser Char Token
lNegsym = Tneg <$ symbol '-' <|> Tneg <$ symbol '~'

lConsym :: Parser Char Token
lConsym = Tcon <$ symbol '&' <|> Tcon <$ token "/\\"

lDissym :: Parser Char Token
lDissym = Tdis <$ symbol 'v' <|> Tdis <$ token "\\/"

lImpsym :: Parser Char Token
lImpsym = Timp <$ token "->" <|> Timp <$ token "=>"

lEqsym :: Parser Char Token
lEqsym = Teq <$ symbol '=' <|> Teq <$ token "<=>"

lOpensym :: Parser Char Token
lOpensym = Open <$ symbol '('

lClosesym :: Parser Char Token
lClosesym = Close <$ symbol ')'

tokenize :: Parser Char [Token]
tokenize = white *> (many (choice [lVarsym, lNegsym, lConsym, lDissym, lImpsym, lEqsym, lOpensym, lClosesym]) <* white) <* eof ()