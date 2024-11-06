{-# LANGUAGE GADTs #-}

module ExList where

import Data.Char qualified as C
import Data.List qualified as L
import Prelude
  ( Bool (..),
    Char,
    Double,
    Enum (..),
    Eq (..),
    Float,
    Int,
    Integer,
    Integral (..),
    Num (..),
    Ord (..),
    String,
    curry,
    error,
    flip,
    not,
    otherwise,
    uncurry,
    undefined,
    ($),
    (&&),
    (.),
    (||)

  )

import Prelude qualified as P

{- import qualified ... as ... ?

To use a function from a qualified import
you need to prefix its name with its alias and a dot:
P.head   C.toUpper   etc.

I import these for you to test the original functions on ghci:

ghci> :t C.toUpper
C.toUpper :: Char -> Char

You MUST NOT use ANY of these in your code

-}

{- Our lists vs Haskell lists

Our definition:

data List a where
  Nil  :: List a
  Cons :: a -> List a -> List a

Here we use Haskell's built-in lists and associated syntactic sugar.
It is as if it was defined like this:

    data [a] = [] | (x : xs)

or like this:

    data [a] where
      []  :: [a]
      (:) :: a -> [a] -> [a]

write [a]       for our List a
write []        for our List
write []        for our Nil
write (x : xs)  for our Cons x xs
write [u,v]     for our u `Cons` (v `Cons` Nil)
-}


isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False

head :: [a] -> a
head [] = error "empty list, no head"
head (x:_) = x

tail :: [a] -> [a]
tail [] = error "empty list, no tail"
tail (_:xs) = xs

null :: [a] -> Bool
null [] = True
null _ = False

length :: (Integral i) => [a] -> i
length (_:xs) = 1 + length xs
length [] = 0

sum :: (Num a) => [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs

product :: (Num a) => [a] -> a
product [] = 1
product (x:xs) = x * product xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = x `snoc` (reverse xs)

(++) :: [a] -> [a] -> [a]
(++) l [] =  l
(++) l (x:xs) = (++) (x `snoc` l) xs

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc n [ ] = [n]
snoc n (x:xs) = x:(snoc n xs)

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ [] = xs
xs +++ [y] = xs <: y
xs +++ (y : ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm??)
infixl 5 +++


minimum :: Ord a => [a] -> a
minimum [ ] = error "empty list"
minimum [x] = x
minimum (x:xs) =
  (let m = minimum xs in
     if x < m
     then x
     else m)

maximum :: Ord a => [a] -> a
maximum [] = error "empty list"
maximum [x] = x
maximum (x:xs) =
  (let m = maximum xs in
     if x > m
     then x
     else m)

take :: Integral n => n -> [a] -> [a]
take _ [] = []
take 0 n = []
take n (x:xs) = x:(take (n - 1) xs)

drop :: Integral n => n -> [a] -> [a]
drop _ [] =  []
drop 0 (x:xs) = x:xs
drop n (x:xs) = drop (n - 1) xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs) | p x = x:(takeWhile p xs)
                   | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p (x:xs) | p x = dropWhile p xs
                   | otherwise = (x:xs)

tails :: [a] -> [[a]]
tails [] = [[]]
tails (x:xs) = (x:xs):(tails xs)

init :: [a] -> [a]
init [] = error "empty list"
init [_] = []
init (x:xs) = x:(init xs)


-- subsequences

any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any p (x:xs) =
  if p x
  then True
  else any p xs


all :: (a -> Bool) -> [a] -> Bool
all _ [] = False
all p (x:xs) =
  if not (p x)
  then False
  else all p xs

and :: [Bool] -> Bool
and = fold (&&) True

or :: [Bool] -> Bool
or = fold (||) False

concat :: [[a]] -> [a]
concat = fold (++) []

-- elem using the funciton 'any' above
elem :: Eq a => a -> [a] -> Bool
elem e l = any (\x -> x == e) l

-- elem': same as elem but elementary definition
-- (without using other functions except (==))
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs) =
  if e == x
  then True
  else elem' e xs

-- (!!)

-- (!!)

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs) | p x = x:(filter p xs)
                | otherwise = filter p xs

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = (f x):map f xs

cycle :: [a] -> [a]
cycle [] = error "empty list"
cycle xs = xs ++ (cycle xs)

repeat :: a -> [a]
repeat x = x:repeat x

fold :: (a -> a -> a) -> a -> [a] -> a
fold _ i [] = i
fold f i (x:xs) = f x (fold f i xs)

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x:(replicate (n - 1) x)

-- isPrefixOf
-- isInfixOf
-- isSuffixOf

zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x, y):(zip xs ys)

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x:xs) (y:ys) = (f x y):(zipWith f xs ys)

-- intercalate
-- nub


-- thanks to Isaac Marlon!
consL :: a -> ([a], [a]) -> ([a], [a])
consL n (l, r) = (n:l, r)

consR :: a -> ([a], [a]) -> ([a], [a])
consR n (l, r) = (l, n:r)

splitAt :: Integral n => n -> [a] -> ([a], [a])
splitAt _ [] = ([], [])
splitAt 0 xs = ([], xs)
splitAt n (x:xs)
  | n < 0 = error "index out of bounds"
  | otherwise = consL x (splitAt (n - 1) xs)


-- what is the problem with the following?:
splitAt' n xs  =  (take n xs, drop n xs)

break :: (a -> Bool) -> [a] -> ([a], [a])
break   _ [] = ([], [])
break p (x:xs) =
  if p x
  then ([], (x:xs))
  else consL x (break p xs)


left :: (a, a) -> a
left (l, _) = l

right :: (a, a) -> a
right (_, r) = r

slice :: (Eq a) => a -> [a] -> ([a], [a])
slice _ [] = ([], [])
slice e (x : xs) =
  if e == x
    then ([], xs)
    else consL x (slice e xs)

splitWhenFind :: (Eq a) => a -> [a] -> [[a]]
splitWhenFind _ [] = []
splitWhenFind c l = left slices : splitWhenFind c (right slices)
  where
    slices = slice c l

lines :: String -> [String]
lines = splitWhenFind '\n'

words :: String -> [String]
words = splitWhenFind ' '


unline :: [String] -> String
unline = (fold (++) "") . (map (\s -> s ++ "\n"))

unwords :: [String] -> String
unwords [] = ""
unwords [w] = w
unwords (w:ws) = (w ++ " ") ++ unwords ws

-- transpose auxiliar functions

matrixHeight :: Integral b => [[a]] -> b
matrixHeight m = (maximum . (map length)) m

get :: Int -> [a] -> a
get 0 (x:_) = x
get n (_:xs) = get (n-1) xs
get _ [] = error "index out of bounds"

getNths :: Int -> [[a]] -> [a]
getNths n lss = [get n ls | ls <- lss, n < length ls]

--

transpose :: [[a]] -> [[a]]
transpose [] = [[]]
transpose lss = [getNths n lss | n <- [0..(matrixHeight lss) - 1], not (isEmpty lss)]



-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome l = (==) l (reverse l)

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}
