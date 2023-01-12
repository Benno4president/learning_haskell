-- all
-- map
-- just maybe nothing

-- parametric polymorphism
-- overloading

-- types
-- Eq IO
{-# LANGUAGE InstanceSigs #-}
import Data.Char ( isDigit, digitToInt )

main :: IO () -- IO () means side effects, no return. () is empty tuble.
main = do
    w <- getLine
    loop (read w :: Int)
    where
        loop 1 = putStr (show 1)
        loop x = do
            putStr (show x)
            if even x
                then loop (x `div` 2)
                else loop (3*x+1)

textflip :: IO ()
textflip = do
    w <- getLine -- get that cmd input
    print_vertical w
        where -- helper func
            print_vertical [] = return () -- return () just retunrs b/c nothing more todo
            print_vertical (x:xs) = do
                putStr (x:"\n") -- notice string/list concat
                print_vertical xs

hugorm :: IO ()
hugorm = do
    putStr "How many? "
    w <- getLine
    nx <- readmore (read w::Int) []
    putStrLn ("total: " ++ show (sum nx))

-- NOT A HELPER FUNC: has other type!
readmore :: (Eq t, Num t) => t -> [Int] -> IO [Int]
readmore 0 xs = return xs -- return is important..?!
readmore n xs = do
    s <- getLine
    readmore (n-1) ((read s :: Int) : xs)


putRow :: Show a => a -> Int -> IO ()
putRow row num = do
    putStr (show row)
    putStr ": "
    putStrLn (concat (replicate num "* "))

-- wtf is a functor
data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Show)

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap _ Nil = Nil -- nothing to fmap in this case
  fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)


data UTree a = UNode a [UTree a] deriving (Show)

instance Functor UTree where
    fmap :: (a -> b) -> UTree a -> UTree b
    fmap f (UNode a []) = UNode (f a) []
    fmap f (UNode a xss) = UNode (f a) [fmap f xs | xs <- xss]

hello :: IO () -- testing 123
hello = print (UNode 1 [UNode 2 [UNode 3 []], UNode 22 []])

-- onion
data Onion a = Core a | Layer (Onion a) deriving (Show)

instance Functor Onion where
    fmap f (Core a) = Core (f a)
    fmap f (Layer a) = Layer (fmap f a)

oni = print (fmap (*2) (Layer (Layer (Core 2))))

-- monads monadic binds
-- mapM implementation
mapMM :: Monad m => (t -> m a) -> [t] -> m [a]
mapMM f [] = return []
mapMM f (x:xs) = do 
    y <- f x
    ys <- mapMM f xs
    return (y:ys)

conv :: Char -> Maybe Int
conv c  | isDigit c = Just (digitToInt c)
        | otherwise = Nothing


ff :: (Monad m, Num a) => m b -> m (a, b)
ff xs = do -- 123 = [(4,1),(4,2),(4,3)]
    x <- xs
    return (4,x)

ff2 :: (Monad m, Num a) => m b -> m (a, b)
ff2 xs = xs >>= (\x -> return (4,x))

-- datatype newtype *3rd


-- To be able to explain precisely what a higher-order function is and what the type of a higher-order function is
-- To be able to explain precisely higher-order functions on lists including map, filter , foldr and foldl



-- instance Functor Expr where
--     fmap f (Val n) = Val n
--     fmap f (Var x) = Var (f x)
--     fmap f (Add e1 e2) = Add (fmap f e1) (fmap f e2)
-- 
-- instance Applicative Expr where
--     pure = Var
--     (Val n) <*> _ = Val n
--     (Var f) <*> x = fmap f x
--     (Add e1 e2) <*> x = Add (e1 <*> x) (e2 <*> x)