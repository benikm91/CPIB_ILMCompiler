-- IML interpreter and VCG
-- Edgar F.A. Lederer, FHNW, 2010

module ParserCombis where

infixr 5 +++

-- suffix P indicates a parser
-- suffix C indicates a parser combinator

data Parser t a = P ([t] -> [(a, [t])])

parse :: Parser t a -> [t] -> [(a, [t])]
parse (P p) inp = p inp

instance Functor (Parser t) where
  fmap f = (>>= return . f)

instance Applicative (Parser t)

instance Monad (Parser t) where
  return v = P (\inp -> [(v, inp)])
  p >>= f = P (\inp -> case parse p inp of
                         [] -> []
                         [(v, out)] -> parse (f v) out)

failureP :: Parser t a
failureP = P (\inp -> [])

itemP :: Parser t t
itemP = P (\inp -> case inp of
                     [] -> []
                     (x:xs) -> [(x, xs)])

(+++) :: Parser t a -> Parser t a -> Parser t a
p +++ q = P (\inp -> case parse p inp of
                       [] -> parse q inp
                       [(v, out)] -> [(v, out)])

rep0C :: Parser t a -> Parser t [a]
rep0C p = rep1C p +++ return []

rep1C :: Parser t a -> Parser t [a]
rep1C p = do v <- p; vs <- rep0C p; return (v : vs)

sepList1C :: Parser t a -> Parser t b -> ([a] -> c) -> Parser t c
sepList1C elemP sepP f =
  do
    e  <- elemP
    es <- rep0C (do sepP; e <- elemP; return e)
    return (f (e : es))

sepList0C :: Parser t a -> Parser t b -> ([a] -> c) -> Parser t c
sepList0C elemP sepP f = sepList1C elemP sepP f +++ return (f [])

epsilonP :: Parser t [a]
epsilonP = return []

optC :: Parser t a -> Parser t (Maybe a)
optC elemP =
      (do e <- elemP; return (Just e))
  +++ (do epsilonP;   return Nothing)
