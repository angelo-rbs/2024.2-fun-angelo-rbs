data Nat = O | S Nat
  deriving (Eq)

o, so, sso, ssso, sssso, ssssso :: Nat
o = O
so = S O
sso = S (S O)
ssso = S (S (S O))
sssso = S (S (S (S O)))
ssssso = S (S (S (S (S O))))

instance (Show Nat) where
  show O = "o"
  show (S n) = "s" ++ show n

(<+>) :: Nat -> Nat -> Nat
(<+>) n O = n
(<+>) n (S m) = S (n <+> m)

prod :: Nat -> Nat -> Nat
prod n O = O
prod n (S m) = n <+> (n `prod` m)

(<->) :: Nat -> Nat -> Nat
(<->) n O = n
(<->) O (S m) = O
(<->) (S n) (S m) = n <-> m

sg :: Nat -> Nat
sg O = O
sg _ = S O

instance (Num Nat) where
  (+) = (<+>)
  (*) = prod
  (-) = (<->)
  abs n = n
  signum = sg
  fromInteger x
    | x < 0 = O
    | x == 0 = O
    | otherwise = S (fromInteger (x - 1))
