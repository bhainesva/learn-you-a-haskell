import Data.List
import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set

doubleMe x = x + x

doubleUs x y = x*2 + y*2

doubleSmallNumber x = if x > 100
  then x
  else x * 2

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]

length'' :: (Num b) => [a] -> b
length'' [] = 0
length'' (_:xs) = 1 + length'' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

factorial n = product [1..n]

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
  |  bmi <= 18.5 = "you're underweight"
  |  bmi <= 25.0 = "you're normal"
  |  bmi <= 30 = "you're fat"
  |  True = "you're obese"

initials :: String -> String -> String
initials (f:_) (l:_) = [f] ++ ". " ++ [l] ++ "."

max' :: Ord a => [a] -> a
max' [x] = x
max' (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = max' xs

rep :: (Num i, Ord i) => b -> i -> [b]
rep x n
  | n <= 0 = []
  | otherwise = x: rep x (n-1)

tk :: (Num i, Ord i) => i -> [a] -> [a]
tk i _
  | i <= 0 = []
tk _ [] = []
tk i (x:xs) = x : tk (i-1) xs

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

rep' :: a -> [a]
rep' x = x : rep' x

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' x (h:xs)
  | x == h = True
  | otherwise = elem' x xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
  in smallerSorted ++ [x] ++ biggerSorted

isUpperAlphanum :: Char -> Bool
isUpperAlphanum x = elem x ['A'..'Z']

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f y x = f x y

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
  | f x       = x : filter' f xs
  | otherwise = filter' f xs

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' y ys = foldl (\acc el -> acc || el == y) False ys

maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (max)

negify :: (Num a) => [a] -> [a]
negify = map (\x -> negate (abs x))

negify' :: (Num a) => [a] -> [a]
negify' = map $ negate . abs

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

intersperse' :: a -> [a] -> [a]
intersperse' x = foldr (\el acc -> el : x : acc) []

encode :: Int -> String -> String
encode shift msg =
  let ords = map ord msg
      shifted = map (+ shift) ords
  in map chr shifted

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing