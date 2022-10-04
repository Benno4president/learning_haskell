-- recursion
import Data.List(group)

-- opg 1, requires Eq to see if Equal to 1. 
-- Takes absolut value to handle negative error
rep :: x -> Int -> [x]
rep x 1 = [x]
rep x n = x:rep x (abs (n-1))

-- This solution does it correct on integers, but reverse on stings...
improve :: [x] -> [x]
improve [] = []
improve (x:xs)
    | odd (length xs) = improve xs
    | otherwise = x:improve xs

rev :: [a] -> [a]
rev [] = []
rev [x] = [x]
rev (x:xs) = (rev xs) ++ [x]

myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs

