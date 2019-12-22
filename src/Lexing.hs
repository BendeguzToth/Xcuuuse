module Lexing where

import Prelude hiding ((<*>), (<$>), (<*), (*>), (<$))
import ParserLib

data Token = Tvar   String | Tneg | Tcon | Tdis | Timp |Teq | Topen | Tclose
                    | Tnewline | Tthen | Tnum Int
                    deriving(Show, Eq)

lVarsym :: Parser Char Token
lVarsym = (\l r-> Tvar (l:r)) <$> uppercase <*> many (digit <|> symbol '\'')

lNegsym :: Parser Char Token
lNegsym = Tneg <$ symbol '-' <|> Tneg <$ symbol '~'

lConsym :: Parser Char Token
lConsym = Tcon <$ symbol '&' <|> Tcon <$ token "/\\"

lDissym :: Parser Char Token
lDissym = Tdis <$ symbol 'v' <|> Tdis <$ token "\\/" <|> Tdis <$ token "||"

lImpsym :: Parser Char Token
lImpsym = Timp <$ token "->" <|> Timp <$ token "=>"

lEqsym :: Parser Char Token
lEqsym = Teq <$ symbol '=' <|> Teq <$ token "<=>"

lOpensym :: Parser Char Token
lOpensym = Topen <$ symbol '('

lClosesym :: Parser Char Token
lClosesym = Tclose <$ symbol ')'

lNlsym :: Parser Char Token
lNlsym = Tnewline <$ symbol '\n'

lThensym :: Parser Char Token
lThensym = Tthen <$ token "|-"

lNumsym :: Parser Char Token
lNumsym = Tnum . read . (:[]) <$> digit

tokenize :: Parser Char [Token]
tokenize = allWhite *> (many (choice [lVarsym, lNegsym, lConsym, lDissym, lImpsym, lEqsym, lOpensym, lClosesym, lNlsym, lThensym, lNumsym] <* white)) <* eof ()
