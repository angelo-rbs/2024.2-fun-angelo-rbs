consL :: a -> ([a], [a]) -> ([a], [a])
consL e (l, r) = (e : l, r)

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
