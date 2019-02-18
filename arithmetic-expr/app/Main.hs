module Main where


data Expr a = Lit a | Add (Expr a) (Expr a) | Mult (Expr a) (Expr a)

instance Show a => Show (Expr a) where
  showsPrec _ (Lit a) = shows a
  showsPrec p (Add e1 e2)
    = showParen (p > precAdd)
    $ showsPrec precAdd e1
      . showString "+"
      . showsPrec precAdd e2
    where precAdd = 5
  showsPrec p (Mult e1 e2)
    = showParen (p > precMult)
    $ showsPrec precMult e1
      . showString "*"
      . showsPrec precMult e2
    where precMult = 6


main :: IO ()
main = do
  putStrLn "Hello, World !"

