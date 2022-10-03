#!/usr/bin/env stack
-- stack --resolver lts-19.22 script

{-
https://www.haskelltutorials.com/guides/haskell-lists-ultimate-guide.html
http://learnyouahaskell.com/making-our-own-types-and-typeclasses
-}

--import System.Directory ()

main :: IO()
main = do
    {- val assignments are NOT equal; they work in different ways -}
    let other_val = [6,6,6,7,8,9,5,4,3,2,1]
    val <- getlist
    print val
    print other_val
    print $ qsort (val::[Int]) 
    print $ reverseQsort (val::[Int]) 
    {- (x::type) is a type hint, b/c [] would error out. -}
    print $ length $ 2:val -- 2:val inserts 2 at index 0.
    print $ filter (>5) val
    print $ elem 5 val -- bool check; 5 in val
    print $ notElem 5 val -- 5 not in val
    print $ indexOf 5 val -- val.index(k)
    print $ take 5 val -- takes 5 first values in val
    print $ takeWhile (\x -> x /= 5) val -- takes values while condition is true.
    print $ takeWhile (/=5) val -- same same, but different

    print $ sum [1,2,3]

    let chararr = [getChar, getChar, getChar]
    x <- seqn chararr
    print x

    print $ PlayerStats "Gragas" (Kda 2 3 4)
    let gert = Person "Gert" 45 (Kda 2 0 4)
    print gert
    print $ name gert
    print $ presentPerson gert

    print Monday
    print $ Monday < Friday
    print $ Saturday == Saturday
    print $ succ Wednesday 
    print $ pred Wednesday 
    print (maxBound :: Day)
    print ([minBound .. maxBound] :: [Day])
    print (minBound :: Int)


getlist :: IO [Int]
getlist = return [6,4,2,8,5,9,3,5,1] --first resolved at val <- getlist

qsort, reverseQsort :: [Int] -> [Int] --optional, but probably recommended.
qsort [] = []
qsort (x:xs) = qsort [a | a <- xs, a<=x] ++ [x] ++ [b | b <- xs, b>x]
reverseQsort x = reverse $ qsort x 

{- returns index of k in l -}
indexOf :: Int -> [a] -> a
indexOf k l = l !! k

seqn :: [IO a] -> IO [a]
seqn [] = return []
seqn (act:acts) = do
    x <- act
    xs <- seqn acts
    return (x:xs)

data Kda = Kda Int Int Int deriving (Show)
data PlayerStats = PlayerStats String Kda deriving (Show)

kdaAvg :: Kda -> Double
kdaAvg (Kda k d a) = fromIntegral $ div (sum [k, d, a]) (length [k, d, a]) 

data Person = Person {
    name :: String,
    age :: Int,
    kda :: Kda
} deriving (Show)

presentPerson :: Person -> String
presentPerson Person {name=a, age=b,kda=(Kda c d f)} = 
    "The persons name is " ++ a ++ 
    " with the age of " ++ show b ++
    " and a spicy k/d/a of " ++ show c ++"/"
    ++ show d ++"/"++ show f


data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
           deriving (Eq, Ord, Show, Read, Bounded, Enum) 



