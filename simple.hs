
-- This is the simple program from the slides from the introduction

laengde :: (Num p) => [a] -> p

laengde [] = 0
laengde (x:l) = 1 + (laengde l)

myList = [2,3,17,9,69,484000]

data BTree = BLeaf Int | BBranch Int BTree BTree deriving Show

-- sumtree :: BTree -> Int

sumtree (BLeaf x) = x
sumtree (BBranch x t1 t2) = let v1 = sumtree t1
                                v2 = sumtree t2
                            in x + v1 + v2


myBigOak = BBranch 14 (BLeaf 13) (BLeaf 17)

-- Quicksort

qsort :: (Ord a) => [a] -> [a]

qsort [] = []
qsort (x:xs) = small ++ [x] ++ big
                 where small = qsort [a | a <- xs, a <= x]
                       big   = qsort [a | a <- xs, a > x]


{- session 1 opgs  -}
-- partial function version.
allButSecond, aBS :: [a] -> [a]
allButSecond [] = []
allButSecond (x:xs) = x:(tail xs)

-- Pure function version
aBS (x:xs) = if (length xs > 0)
    then x:(tail xs)
    else [x]
-- other version of a Pure function. The *true haskell* way.
aBS2 :: [a] -> [a]
aBS2 [] = []
aBS2 [x] = [x]
aBS2 (x:xs) = x:(tail xs)

midtover :: [a] -> ([a],[a])
midtover xs = (take (div (length xs) 2) xs, drop (div (length xs) 2) xs)
{-
midtover2 :: [a] -> ([a],[a])
midtover2 xs = [front,back]
    where
        let i = div (length xs) 2
        front = take i xs
        back = drop i xs
-}
midtover3 :: [a] -> ([a],[a])
midtover3 xs = splitAt (div (length xs) 2) xs 






















