module ParserLib where
import Prelude hiding ((<*>), (<$>), (<*), (*>), (<$))
import Data.Char

type Parser symbol result = [symbol] -> [(result, [symbol])]


infixl 6 <*>
(<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
(p<*>q) xs = [(f res, zs) | (f, ys) <- p xs, (res, zs) <- q ys]

infixl 4 <|>
(<|>) :: Parser s a -> Parser s a -> Parser s a
(p<|>q) xs = p xs ++ q xs

infixl 4 <<|>
(<<|>) :: Parser s a -> Parser s a -> Parser s a
(p<<|>q) xs | null r = q xs
            | otherwise = r
            where r = p xs

infixl 7 <$>
(<$>) :: (a -> b) -> Parser s a -> Parser s b
(f<$>p) xs = [(f r, ys) | (r, ys) <- p xs]

infixl 7 <$
(<$) :: b -> Parser s a -> Parser s b
f <$ p = const f <$> p

infixl 6 <*
(<*):: Parser s a -> Parser s b -> Parser s a
p <* q = const <$> p <*> q

infixl 6 *>
(*>):: Parser s a -> Parser s b -> Parser s b
p *> q = flip const <$> p <*> q

many :: Parser s a -> Parser s [a]
many p = (:) <$> p <*> many p <|> succeed []

some :: Parser s a -> Parser s [a]
some p = (:) <$> p <*> many p

greedy :: Parser s a -> Parser s [a]
greedy p = (:) <$> p <*> greedy p <<|> succeed []

greedy' :: Parser s a -> Parser s [a]
greedy' p = (:) <$> p <*> greedy p

option :: Parser s a -> a -> Parser s a
option p def = p <|> succeed def

succeed :: a -> Parser s a
succeed a = const a <$> epsilon

satisfy :: (s -> Bool) -> Parser s s
satisfy p (x:xs) | p x = [(x, xs)]
satisfy _ _ = []

symbol :: (Eq s) => s -> Parser s s
symbol s = satisfy (==s)

letter :: Parser Char Char
letter = satisfy isLetter

digit :: Parser Char Char
digit = satisfy isDigit

epsilon :: Parser s ()
epsilon xs = [((), xs)]

token :: (Eq s) => [s] -> Parser s [s]
token t xs  | res == t = [(res, remaining)]
            | otherwise = []
            where
                n = length t
                res = take n xs
                remaining = drop n xs

pack :: Parser s a -> Parser s b -> Parser s c -> Parser s b
pack l p r = id <$ l <*> p <* r

listOf :: Parser s a -> Parser s b -> Parser s [a]
listOf p s = (:) <$> p <*> many (s *> p)

listOf' :: Parser s a -> Parser s b -> Parser s [a]
listOf' p s = (:) <$> p <*> greedy (s *> p)

failp :: Parser s a
failp _ = []

eof :: a -> Parser s a
eof d [] = [(d, [])]
eof _ _ = []

uppercase :: Parser Char Char
uppercase = satisfy (`elem`['A'..'Z'])

white :: Parser Char String
white = greedy $ satisfy (`elem` [' ', '\n', '\t'])

choice :: [Parser a b] -> Parser a b
choice [] = failp
choice ps = foldl1 (<|>) ps

chainl :: Parser s a -> Parser s (a->a->a) -> Parser s a
chainl p s = foldl (flip ($)) <$> p <*> many (flip <$> s <*> p)

chainr :: Parser s a -> Parser s (a->a->a) -> Parser s a
chainr p s = flip (foldr ($)) <$> many (flip ($) <$> p <*> s) <*> p