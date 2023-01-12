import Distribution.Simple.Utils (xargs)
import Control.Applicative

-- **
-- **
-- **

-- problem 1.1
--allAnswers f [] = Just []
--allAnswers f (x:xs) = let fun = f x
--                            in if fun == Nothing then Nothing
--                            else x : allAnswers f xs
--                                where x=x
-- wrong return type, no just
-- where is inf loop


-- problem 1.2
-- allAnswersworks :: p -> [a1] -> Maybe [a2]
allAnswersworks :: Eq a1 => (a2 -> Maybe a1) -> [a2] -> Maybe [a1]
allAnswersworks f [] = Just []
allAnswersworks f xs = if Nothing `elem` ys then Nothing
                    else Just (map fromJust ys)
                    where
                        ys = map f xs
                        fromJust (Just x) = x

-- testing
myfun :: (Ord a, Num a) => a -> Maybe a
myfun x = if x > 3 then Just (x+3) else Nothing
-- allAnswersworks myfun [9,9]

-- problem 1.3
allAnswersworks' :: (t -> Maybe a) -> [t] -> Maybe [a]
allAnswersworks' f [] = Just []
allAnswersworks' f (x:xs) = do
                        z <- allAnswersworks' f xs
                        v <- f x
                        return (v:z)
-- do block is monad style, therefore opg done


-- problem 2.1


a_21 :: Eq a1 => a2 -> (a1, a1) -> [a2]
a_21 x (z, y) = [x | z == y]

b_21 :: Eq a => (a -> a -> Bool) -> Bool -> a -> a -> Bool
b_21 f x y z = f (head [y,z]) z && x && y == z
-- hacky hacky

c_21 :: Show a => a -> IO b -> IO b
c_21 x f = do
    y <- f
    print x
    return y

d_21 :: (t -> a) -> t -> t -> [a]
d_21 f x y = [f (head [x,y])]
-- list xy to force same type
-- f head for at force t -> a ik [t] -> a

-- problem 2.2

-- The type of a polymorphic function often gives a strong indication about the
-- function’s behaviour. For example, from the type [a] -> [b] -> [(a,b)] we
-- can conclude that zip pairs up elements from two lists, although the type on its
-- own doesn’t capture the precise manner in which this is done.

-- A type that contains one or more class constraints is called overloaded, as is
-- an expression with such a type. Hence, Num a => a -> a -> a is an overloaded
-- type and (+) is an overloaded function.

a_22 = "both"
b_22 = "overload"
c_22 = "overload"
d_22 = "poly"


-- problem 3.1
data Tree a = Node (Tree a) (Tree a) | Leaf a
    deriving (Show, Eq)

-- husk at læse hele opgaven :))))
testtree = Node (Leaf "dog") (Node (Leaf "cat") (Leaf "hamster"))

-- problem 3.2
minimax :: Ord a => Tree a -> (a, a)
minimax (Leaf x) = (x,x)
minimax (Node x y) = (u,v)
                    where
                        (x1,y1) = minimax x
                        (x2,y2) = minimax y
                        u = min x1 x2
                        v = max y1 y2


-- problem 4.1
-- Convert the following do-block into an equivalent expression that does
-- not use the do-notation but uses monadic binds.
-- echo = do
-- putStr ”Please type a word: ”
-- s <− getLine
-- putStrLn (”You typed ” ++ s)

echo :: IO ()
echo = putStr "type word:" >> (getLine >>= (\s -> putStrLn ("you typed "++s)))


-- problem 4.2

-- Write a do block that defines a value that
-- • from the console gets an input string from the user that is a string
-- corresponding to a list of pairs of truth values
-- • outputs a string to the console that is a list of the second components of these pairs

seconds :: IO ()
seconds = do
    x <- getLine
    let blist = read x :: [(Bool, Bool)]
        w = map (\(x,y) -> y) blist in
            do
                putStr (show w)



