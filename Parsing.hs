-- Functional parsing library from chapter 13 of Programming in Haskell,
-- Graham Hutton, Cambridge University Press, 2016.
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use <$>" #-}

module Parsing (module Parsing, module Control.Applicative) where

import Control.Applicative
import Data.Char
    ( isAlpha, isAlphaNum, isDigit, isLower, isSpace, isUpper )

-- Basic definitions

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) = p
--parse (P p) = p

item :: Parser Char
item = P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])

-- Sequencing parsers

instance Functor Parser where
   -- fmap :: (a -> b) -> Parser a -> Parser b
   fmap g p = P (\inp -> case parse p inp of
                            []        -> []
                            [(v,out)] -> [(g v, out)])

instance Applicative Parser where
   -- pure :: a -> Parser a
   pure v = P (\inp -> [(v,inp)])

   -- <*> :: Parser (a -> b) -> Parser a -> Parser b
   pg <*> px = P (\inp -> case parse pg inp of
                             []        -> []
                             [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
   -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
   p >>= f = P (\inp -> case parse p inp of
                           []        -> []
                           [(v,out)] -> parse (f v) out)

-- Making choices

instance Alternative Parser where
   -- empty :: Parser a
   empty = P (const [])

   -- (<|>) :: Parser a -> Parser a -> Parser a
   p <|> q = P (\inp -> case parse p inp of
                           []        -> parse q inp
                           [(v,out)] -> [(v,out)])

-- Derived primitives

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

ident :: Parser String
ident = do x  <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
       <|> nat

-- Handling spacing

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

-- INT PARSE ghci :: parse expr "1+2*3" = [(7,"")]
expr :: Parser Int
expr = do
   t <- term
   do
      symbol "+"
      e <- expr
      return (t + e)
      <|> return t

term :: Parser Int
term = do
   f <- factor
   do symbol "*"
      t <- term
      return (f * t)
      <|> return f

factor :: Parser Int
factor = do
   symbol "("
   e <- expr
   symbol ")"
   return e
   <|> natural

-- final form <3
eval :: String -> Int
eval xs = case parse expr xs of
   [(n,[])] -> n
   [(_,out)] -> error ("Unused input " ++ out)
   [] -> error "Invalid input"


data Onion = Core Int| Layer Onion deriving Show

-- expr = Core int | Layer expr

--oexpr :: Parser Onion
oexpr :: Parser Onion
oexpr = do
   symbol "L"
   x <- oexpr
   return (Layer x)
   <|> core
   
core :: Parser Onion
core = do
   y <- integer
   return (Core y)
   

oeval :: String -> Onion
oeval xs = case parse oexpr xs of
   [(n,[])] -> n
   [(_,out)] -> error ("Unused input " ++ out)
   [] -> error "Invalid input"