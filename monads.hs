import Control.Monad

type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
  | abs (left + n - right) < 4 = Just (left + n, right)
  | otherwise = Nothing


landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
  | abs (right + n - left) < 4 = Just (left, right + n)
  | otherwise = Nothing

banana :: Pole -> Maybe Pole
banana _ = Nothing

x -: f = f x


type KnightPos = (Int,Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
  (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
              ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
              ]
  guard (c' `elem` [1..8] && r' `elem` [1..8])
  return (c',r')

in3 :: KnightPos -> [KnightPos]
in3 start = do
  first <- moveKnight start
  second <- moveKnight first
  moveKnight second

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start