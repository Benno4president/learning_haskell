
pyt :: Int -> [(Int,Int,Int)]
pyt i = [(a,b,c) | a <- [1..i], b <- [1..i], c <- [1..i], ((a*a)+(b*b))==(c*c)]

sevens :: Int -> [Int]
sevens i = [x | x <- [1..i-1], mod x 7 == 0]

headsup :: Eq a => [a] -> Bool
headsup [] = False
headsup [x] = False
headsup (x:y:_) = x==y

(%) a b = mod a b 
(!&) a b = not (a==b)


plonk :: Num a => a -> a -> a -> a
plonk x y z = x+y+z

-- lambda version
-- \x y z -> x+y+z
-- fx:
sevens2 i = [x | x <- [1..i-1], (\x -> mod x 7) x == 0]


sup :: (Ord a1, Eq a2) => a2 -> a2 -> (a1, a1) -> a1
sup x y (a,b)
    | x == y = a
    | a <= b = a
    | a > b = b
