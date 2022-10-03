#!/usr/bin/env stack
-- stack --resolver lts-19.22 script

main :: IO ()
main = do
    let numbers = [5,3,8,1,8,0,2,5,6]
    let numberTree = foldr treeInsert EmptyTree numbers
    print numberTree
    print $ treeElem 1 numberTree
    print $ treeElem 9 numberTree
    print $ treeElemDepth 2 numberTree
    print $ treeElemDepth 6 numberTree
    print $ treeElemDepth 10 numberTree

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Eq)

newLeaf :: a -> Tree a
newLeaf x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = newLeaf x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right

-- returns the depth of item or where it would have been.
treeElemDepth :: (Ord a) => a -> Tree a -> Int
treeElemDepth x EmptyTree = 1
treeElemDepth x (Node a left right)
    | x == a = 1
    | x < a = 1 + treeElemDepth x left
    | x > a = 1 + treeElemDepth x right
