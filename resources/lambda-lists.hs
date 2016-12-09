{-#LANGUAGE RankNTypes#-}

data Number a = Number (forall a. (a -> a) -> a -> a)

instance Num a => Show (Number a) where
  show (Number f) = show $ f (1 +) 0

zero :: Number a
zero = Number (\f x -> x)

inc :: Number a -> Number a
inc (Number n) = Number (\f x -> f $ n f x)

one :: Number a
one = inc zero

two :: Number a
two = inc one

three :: Number a
three = inc two

true = \x y -> x

false = \x y -> y

pair = \x y z -> z x y

first = \p -> p (\x y -> x)

second = \p -> p(\x y -> y) 

nil = pair true true

isNil = first

cons = \h t -> pair false (pair t h)

hd = \z -> first $ second z

tl = \z -> second $ second z

data List a = EmptyList | ListElement a (List a) deriving Show

-- data [] a = [] | a : [a] deriving Show


