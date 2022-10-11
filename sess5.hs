

-- within [1,2,3,4,5,6,6,7,8] (3,5)
within :: (Ord a) => [a] -> (a,a) -> [a]
within xs (a,b) = (filter (>=a) . filter (<=b)) xs

-- sumrows [[1,2,3,4],[5,6,7,8,9]]
sumrows :: Num a => [[a]] -> [a]
sumrows = map sum

fact k = product [1..k]

approx :: (Fractional a, Enum a) => a -> a
approx n = sum $ map (\x -> 1 / (fact x)) [1..n]

-- div is Integral division, / is division...
-- the fix is
-- div = (/)
-- and then it works as one expects.
approx' :: Integral a => a -> a
approx' n = sum $ map (\x -> div 1 (fact x)) [1..n]

-- HvA fuck er dog dette>!?!?
mapmap :: [a -> b] -> [[a] -> [b]]
mapmap = map map