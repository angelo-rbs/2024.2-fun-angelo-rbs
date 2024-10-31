{-# LANGUAGE GADTs #-}

module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral(..)
    , Bool(..) , not , (&&) , (||)
    , (++)
    , ($)
    , (.)
    , undefined
    , error
    , otherwise
    )


data Nat where
  O :: Nat
  S :: Nat -> Nat

----------------------------------------------------------------
-- typeclass implementations
----------------------------------------------------------------

o, so, sso, ssso, sssso, ssssso, sssssso, ssssssso, sssssssso, ssssssssso, sssssssssso :: Nat
o = O
so = (S O)
sso = (S (S O))
ssso = (S (S (S O)))
sssso = (S (S (S (S O))))
ssssso = (S (S (S (S (S O)))))
sssssso = (S (S (S (S (S (S O))))))
ssssssso = (S (S (S (S (S (S (S O)))))))
sssssssso = (S (S (S (S (S (S (S (S O))))))))
ssssssssso = (S (S (S (S (S (S (S (S (S O)))))))))
sssssssssso = (S (S (S (S (S (S (S (S (S (S O))))))))))



instance Show Nat where

    -- zero  should be shown as O
    -- three should be shown as SSSO
    show O = "O"
    show (S n) = "S" ++ show n

instance Eq Nat where

    (==) O O = True
    (==) (S m) (S n) = (==) m n
    (==) _ _ = False

instance Ord Nat where

    (<=) (S m) (S n) = (<=) m n
    (<=) (S _) O = False
    (<=) _ _ = True


min :: Nat -> Nat -> Nat
min O n = n
min n O = n
min (S m) (S n) = S (ExNat.min m n)

max :: Nat -> Nat -> Nat
max O n = n
max n O = n
max (S m) (S n) = S (ExNat.max m n)


----------------------------------------------------------------
-- internalized predicates
----------------------------------------------------------------

isZero :: Nat -> Bool
isZero O = True
isZero _ = False

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred (S n) = n
pred O = O

even :: Nat -> Bool
even O = True
even (S O) = False
even (S (S n)) = even n

odd :: Nat -> Bool
odd O = False
odd (S O) = True
odd (S (S n)) = odd n

----------------------------------------------------------------
-- operations
----------------------------------------------------------------

-- addition
(<+>) :: Nat -> Nat -> Nat
(<+>) n (S m) = S((<+>) n m)
(<+>) n O = n

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
(<->) :: Nat -> Nat -> Nat
(<->) (S m) (S n) = (<->) m n
(<->) n O = n
(<->) O _ = O

-- multiplication
(<*>) :: Nat -> Nat -> Nat
(<*>) O _ = O
(<*>) _ O = O
(<*>) m (S n) = (<+>) m ((<*>) m n)

-- exponentiation
(<^>) :: Nat -> Nat -> Nat
(<^>) O O = error "indeterminado"
(<^>) _ O = (S O)
(<^>) O _ = O
(<^>) m (S n) = (<*>) m ((<^>) m n)


-- quotient
(</>) :: Nat -> Nat -> Nat
(</>) _ O = error "division by zero"
(</>) n d =
  if n < d
  then O
  else (S ((</>) ((<->) n d) d))

-- remainder
(<%>) :: Nat -> Nat -> Nat
(<%>) _ O = error "divisinho by zero"
(<%>) n d =
  if n < d
  then n
  else ((<%>) ((<->) n d) d)

-- divides
(<|>) :: Nat -> Nat -> Bool
(<|>) n d = ((<%>) n d) == O

divides = (<|>)

-- x `absDiff` y = |x - y|
-- (Careful here: this - is the real minus operator!)
absDiff :: Nat -> Nat -> Nat
absDiff n O = n
absDiff O n = n
absDiff (S m) (S n) = S(absDiff m n)

(|-|) :: Nat -> Nat -> Nat
(|-|) = absDiff

factorial :: Nat -> Nat
factorial O = (S O)
factorial (S n) = (<*>) (S n) (factorial n)

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O = O
sg (S _) = S O

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo _ O = error "log of 0"
lo O _ = error "log of base 0"
lo _ (S O) = O
lo b a
  | a < b = O
  | a == b = S O
  | a > b = S (lo b ((</>) a b))

----------------------------------------------------------------
-- Num & Integral fun
----------------------------------------------------------------

-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!

toNat :: Integral a => a -> Nat
toNat n
  | n > 0 = S (toNat (n - 1))
  | otherwise = O

fromNat :: Integral a => Nat -> a
fromNat O = 0
fromNat (S n) = 1 + (fromNat n)


-- Voilá: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger x
      | x < 0     = O
      | x == 0    = O
      | otherwise = S (fromInteger (x - 1))
