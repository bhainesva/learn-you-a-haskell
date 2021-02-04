import Control.Monad.Writer
import Data.Monoid

isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")

applyLog :: (Monoid m) => (a, m) -> (a -> (b,m)) -> (b,m)
applyLog (x,log) f = let (y, newLog) = f x in (y,log `mappend` newLog)


type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
  a <- logNumber 3
  b <- logNumber 5
  logNumber 10
  -- tell ["Gonna multiply these two"]
  -- return (a*b)

multWithLog' :: Writer [String] Int
multWithLog' = logNumber 3 >>= (\_ ->
  logNumber 5 >>= (\_ ->
  logNumber 10))

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
  | b == 0 = do
      tell ["Finished with " ++ show a]
      return a
  | otherwise = do
      tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
      gcd' b (a `mod` b)

double x = [x,x]
test = do
  a <- [1,2,3]
  double a

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }
toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Semigroup (DiffList a) where
  (DiffList f) <> (DiffList g) = DiffList (\xs -> f (g (xs)))

instance Monoid (DiffList a) where
  mempty = DiffList (id)

finalCountdown :: Int -> Writer [String] ()
finalCountdown 0 = do
  tell ["0"]
finalCountdown x = do
  finalCountdown (x-1)
  tell [show x]

sum3 a b c = a + b + c
fdo = do
  a <- (*2)
  b <- (*4)
  c <- (*8)
  return [a,b,c]

strLen :: String -> Int
strLen = length

divideLen :: Int -> String -> Int
divideLen x = \s -> (length s) `div` x
