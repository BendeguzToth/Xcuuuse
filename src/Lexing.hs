module Lexing where

import Prelude hiding ((<*>), (<$>), (<*), (*>), (<$))
import ParserLib

data Token = Tvar   String | Tneg | Tcon | Tdis | Timp |Teq | Tcont | Topen | Tclose
                    | Tnewline | Tnum Int | Tpremise | Tcomma | Thyphen
                    | Tintroduction | Telimination | Tvertical | Tstar | Tassumption
                    | Tmt | Tpbc | Tlem | Treiterate
                    deriving(Show, Eq)

lVarsym :: Parser Char Token
lVarsym = (\l r-> Tvar (l:r)) <$> uppercase <*> many (digit <|> symbol '\'')

-- Note that '-' is also a negation symbol (see hyphen). It is overloaded.
lNegsym :: Parser Char Token
lNegsym = Tneg <$ symbol '~'

lConsym :: Parser Char Token
lConsym = Tcon <$ symbol '&' <|> Tcon <$ token "/\\"

lDissym :: Parser Char Token
lDissym = Tdis <$ symbol 'v' <|> Tdis <$ token "\\/"

lImpsym :: Parser Char Token
lImpsym = Timp <$ token "->" <|> Timp <$ token "=>"

lEqsym :: Parser Char Token
lEqsym = Teq <$ symbol '=' <|> Teq <$ token "<=>"

lContsym :: Parser Char Token
lContsym = Tcont <$ token "#"

lOpensym :: Parser Char Token
lOpensym = Topen <$ symbol '('

lClosesym :: Parser Char Token
lClosesym = Tclose <$ symbol ')'

lNlsym :: Parser Char Token
lNlsym = Tnewline <$ symbol '\n'


lNumsym :: Parser Char Token
lNumsym = Tnum . read . (:[]) <$> digit

lPremise :: Parser Char Token
lPremise = Tpremise <$ symbol 'p'

lComma :: Parser Char Token
lComma = Tcomma <$ symbol ','

lHyphen :: Parser Char Token
lHyphen = Thyphen <$ symbol '-'

lIntroduction :: Parser Char Token
lIntroduction = Tintroduction <$ symbol 'i'

lElimination :: Parser Char Token
lElimination = Telimination <$ symbol 'e'

lVertical :: Parser Char Token
lVertical = Tvertical <$ symbol '|'

lStar :: Parser Char Token
lStar = Tstar <$ symbol '*'

lAssumption :: Parser Char Token
lAssumption = Tassumption <$ token "ass"

lMT :: Parser Char Token
lMT = Tmt <$ token "mt"

lPBC :: Parser Char Token
lPBC = Tpbc <$ token "pbc"

lLEM :: Parser Char Token
lLEM = Tlem <$ token "lem"

lReiterate :: Parser Char Token
lReiterate = Treiterate <$ symbol 'r'

tokenize :: Parser Char [Token]
tokenize = allWhite *> (many (choice [lVarsym, lNegsym, lConsym, lDissym, lImpsym, lEqsym, lContsym, lOpensym, lClosesym,
                                     lNlsym, lNumsym, lPremise, lComma, lHyphen, lIntroduction, lElimination, lVertical,
                                     lStar, lAssumption, lMT, lPBC, lLEM, lReiterate] <* white)) <* eof ()
