module Nat where

import Distribution.Compat.CharParsing (Parsing (unexpected))
import Prelude hiding (Num (..), gcd, max, min, mod, pred, (<), (>))

data Nat = O | S Nat
  deriving (Eq, Show)

o, so, sso, ssso, sssso, ssssso :: Nat
o = O
so = S O
sso = S (S O)
ssso = S (S (S O))
sssso = S (S (S (S O)))
ssssso = S (S (S (S (S O))))

-- item 1, 2

(+) :: Nat -> Nat -> Nat
n + O = n
n + S m = S (n + m)

(*) :: Nat -> Nat -> Nat
n * O = O
n * S m = n + (n * m)

-- item 3, 4

double :: Nat -> Nat
double = (*) (S (S O))

-- item 7

pred :: Nat -> Nat
pred O = O
pred (S n) = n

-- item 8

fact :: Nat -> Nat
fact O = S O
fact (S n) = S n * fact n

fib :: Nat -> Nat
fib O = S O
fib (S O) = S O
fib (S (S n)) = fib (S n) + fib n

min :: (Nat, Nat) -> Nat
min (n, O) = O
min (O, n) = O
min (S n, S m) = S (min (n, m))

max :: (Nat, Nat) -> Nat
max (n, O) = n
max (O, n) = n
max (S n, S m) = S (max (n, m))

-- auxiliares pras funçõe seguintes

-- natMinus :: Nat -> Nat -> Nat

(>) :: Nat -> Nat -> Bool
(>) (S _) O = True
(>) O (S _) = False
(>) (S m) (S n) =
  if m == n
    then False
    else (>) m n

(<) :: Nat -> Nat -> Bool
(<) (S _) O = False
(<) O (S _) = True
(<) (S m) (S n) =
  if m == n
    then False
    else (<) m n

-- fim funções auxiliares

div :: Nat -> Nat -> (Nat, Nat)
div _ O = error "division by zero"
div O n = (O, n)
div (S m) (S n)
  | m == n = (S O, O)
  | m > n =
  | n > m = (O, m)

quot :: (Nat, Nat) -> Nat
quot = undefined

rem :: (Nat, Nat) -> Nat
rem = undefined

gcd :: (Nat, Nat) -> Nat
gcd (m, O) = m
gcd (m, n) = undefined -- gcd (n, mod m n)

--

gcm :: (Nat, Nat) -> Nat
gcm = undefined
