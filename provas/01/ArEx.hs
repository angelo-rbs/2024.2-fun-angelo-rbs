data Op where
  BinOp :: (Int -> Int -> Int) -> Op
  UnOp :: (Int -> Int) -> Op

data ArEx where
  BinEx :: ArEx -> ArEx -> Op -> ArEx
  UnEx :: ArEx -> Op -> ArEx
  IntEx :: Int -> ArEx

eval :: ArEx -> Int
eval (IntEx n) = n
eval (UnEx x op) = op (eval x)
