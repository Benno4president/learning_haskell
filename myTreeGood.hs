module Main where

import Control.Applicative

data MyTree a = MyNode a [MyTree a]
                deriving (Show)

instance Functor MyTree where
   fmap f (MyNode x treeList) = MyNode (f x) (map (fmap f) treeList)

instance Applicative MyTree where
   pure x = MyNode x []
   (MyNode f treeFunctionList) <*> (MyNode x treeElementList) = 
      MyNode (f x) ( map (fmap f) treeElementList ++ map (<*> MyNode x treeElementList) treeFunctionList )

instance Monad MyTree where
   return x = MyNode x []
   MyNode x treeList >>= f = MyNode x' (treeList' ++ map (>>= f) treeList)
      where MyNode x' treeList' = f x -- how they are piped (this line creates nodes btw)

main :: IO ()
main  =
   do
      putStrLn "Program begins."

      putStrLn "Tests that prove that MyTree behaves as a type constructor."

      let tree1 = MyNode 5 [MyNode 3 [], MyNode 2 []]
      print tree1

      let tree2 = MyNode "ABC" [MyNode "DEFG" [], MyNode "HIJKL" []]
      print tree2

      putStrLn "Tests that prove that MyTree behaves as a Functor."

      print (fmap (*2) tree1)
      print (fmap length tree2)

      putStrLn "Tests that prove that MyTree behaves as an Applicative."

      print (MyNode (*2) [] <*> tree1)
      print (MyNode (*2) [MyNode (+100) [], MyNode (+1000) []] <*> tree1)
      print (MyNode init [] <*> tree2)
      print (MyNode init [MyNode reverse [MyNode tail []]] <*> tree2)

      putStrLn "Tests that prove that MyTree behaves as a Monad."

      print (tree1 >>= (\x -> MyNode (x+200) []))
      print (tree2 >>= (\x -> MyNode (tail x) []))

      putStrLn "Program ends."