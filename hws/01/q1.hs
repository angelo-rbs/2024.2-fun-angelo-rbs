module Nat where

import Distribution.Compat.CharParsing (Parsing (unexpected))
import Prelude hiding (Num (..), max, min, pred)

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

-- questão 8

fact :: Nat -> Nat
fact O = S O
fact (S n) = S n * fact n

fib :: Nat -> Nat
fib = undefined

min :: (Nat, Nat) -> Nat
min (n, O) = O
min (O, n) = O
min (S n, S m) = S (min (n, m))

max :: (Nat, Nat) -> Nat
max (n, O) = n
max (O, n) = n
max (S n, S m) = S (max (n, m))

div :: Nat -> Nat -> (Nat, Nat)
div = undefined

quot :: (Nat, Nat) -> Nat
quot = undefined

rem :: (Nat, Nat) -> Nat
rem = undefined

gcd :: (Nat, Nat) -> Nat
gcd = undefined

gcm :: (Nat, Nat) -> Nat
gcm = undefined
