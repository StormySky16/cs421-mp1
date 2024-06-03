--- Getting Started
--- ===============

--- Relevant Files
--- --------------
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

module Lib where


--- Problems
--- ========

--- Algebraic Data Types
--- --------------------

data List a = Cons a (List a)
            | Nil
  deriving (Show, Eq)

data Exp = IntExp Integer
         | PlusExp [Exp]
         | MultExp [Exp]
  deriving (Show, Eq)

--- ### list2cons

-- don't forget to put the type declaration or you will lose points!
list2cons :: [a] -> List a
list2cons [] = Nil
list2cons (x:xs) = Cons x (list2cons xs)

--- ### cons2list

-- don't forget to put the type declaration or you will lose points!
cons2list :: List a -> [a]
cons2list Nil = []
cons2list (Cons x xs) = x: cons2list xs

--- ### eval

-- don't forget to put the type declaration or you will lose points!
eval :: Exp -> Integer
eval (IntExp x) = x
eval (PlusExp exp) = sum (addHelper exp [])
    where
        addHelper :: [Exp] -> [Integer] -> [Integer]
        addHelper [] list = list
        addHelper (x:xs) list = eval x : addHelper xs list
eval (MultExp exp) = product (multHelper exp [])
    -- product function suggested by VS Code, originally used foldr (*) 1
    -- details on how foldr worked assisted by ChatGPT and cross referenced in documentation
    where
        multHelper :: [Exp] -> [Integer] -> [Integer]
        multHelper [] list = list
        multHelper (x:xs) list = eval x : multHelper xs list



--- ### list2cons'

-- don't forget to put the type declaration or you will lose points!
list2cons' :: [a] -> List a
list2cons' = foldr Cons Nil -- foldr using Cons as binary function and Nil as the initial accumulator value

--- ### BinTree

-- BinTree
data BinTree a =  Node a (BinTree a) (BinTree a)
                | Leaf
    deriving (Show)


    -- created with assistance from ChatGPT, modeled after custom List 

--- ### sumTree

-- don't forget to put the type declaration or you will lose points!

sumTree :: Num a => BinTree a -> a
sumTree Leaf = 0
sumTree (Node val left right) = val + sumTree left + sumTree right

--- ### SimpVal

-- SimpVal
data SimpVal = IntVal Integer
            | BoolVal Bool
            | StrVal String
            | ExnVal String
    deriving (Show)
    --Modeled after Exp ADT
--- ### liftIntOp

-- don't forget to put the type declaration or you will lose points!

liftIntOp :: (Integer -> Integer -> Integer) -> SimpVal -> SimpVal -> SimpVal
liftIntOp op (IntVal x) (IntVal y) = IntVal(op x y) -- Assisted by ChatGPT to better understand how the operator worked
liftIntOp _ _ _ = ExnVal("not an IntVal!")
