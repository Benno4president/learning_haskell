module Main where

import Control.Applicative ()

data MyTree a = MyEmptyNode
              | MyFilledNode a (MyTree a) (MyTree a)
              deriving (Show)

instance Functor MyTree where
   fmap f MyEmptyNode          = MyEmptyNode
   fmap f (MyFilledNode x y z) = MyFilledNode (f x) (fmap f y) (fmap f z)

instance Applicative MyTree where
   pure x = MyFilledNode x MyEmptyNode MyEmptyNode
   (MyFilledNode f fy fz) <*> (MyFilledNode x y z) = MyFilledNode (f x) MyEmptyNode MyEmptyNode
   _ <*> _ = MyEmptyNode

instance Monad MyTree where
   return x = MyFilledNode x MyEmptyNode MyEmptyNode
   (MyFilledNode x y z) >>= f = f x
   MyEmptyNode >>= _ = MyEmptyNode

main :: IO ()
main  =
   do
      putStrLn "Program begins."

      putStrLn "Tests that prove that MyTree behaves as a type constructor."

      let tree1 = MyFilledNode 5 (MyFilledNode 3 MyEmptyNode MyEmptyNode) (MyFilledNode 2 MyEmptyNode MyEmptyNode)
      print tree1

      let tree2 = MyFilledNode "ABC" (MyFilledNode "AB" MyEmptyNode MyEmptyNode) (MyFilledNode "ABCDEF" MyEmptyNode MyEmptyNode)
      print tree2

      putStrLn "Tests that prove that MyTree behaves as a Functor."

      print (fmap (*2) tree1)
      print (fmap length tree2)

      putStrLn "Tests that prove that MyTree behaves as an Applicative."

      print (MyFilledNode (*2) MyEmptyNode MyEmptyNode <*> tree1)
      print (MyFilledNode init MyEmptyNode MyEmptyNode <*> tree2)

      putStrLn "Tests that prove that MyTree behaves as a Monad."

      print (tree1 >>= (\x -> MyFilledNode (x+200) MyEmptyNode MyEmptyNode))
      print (tree2 >>= (\x -> MyFilledNode (tail x) MyEmptyNode MyEmptyNode))

      putStrLn "Program ends."