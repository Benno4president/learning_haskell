-- recursion

-- opg 1, requires Eq to see if Equal to 1. 
-- Takes absolut value to handle negative error
rep :: (Eq n, Num n) => x -> n -> [x]
rep x 1 = [x]
rep x n = x:rep x (abs (n-1))

-- This solution does it correct on integers, but reverse on stings...
improve :: [x] -> [x]
improve [] = []
improve (x:xs)
    | odd (length xs) = improve xs
    | otherwise = x:improve xs