--- Getting Started
--- ===============

--- Relevant Files
--- --------------
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use :" #-}

module Lib where

-- This line imports the Prelude module without certain functions
import Prelude hiding ( take, drop, reverse
                      , zip, zipWith
                      , map, foldl, foldr
                      , iterate, repeat
                      , replicate, cycle
                      , (++)
                      )
-- When you are allowed to use builtin functions Prepend them with "P."
-- for example `P.take`
import qualified Prelude as P

--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake

-- don't forget to put the type declaration or you will lose points!
mytake :: Int -> [a] -> [a]
mytake n [] = []
mytake n (x:xs)
    | n <= 0 = []
    | otherwise = x : mytake (n-1) xs
    --syntax for if-else equivalent in Haskell found on Stack Overflow

--- ### mydrop

-- don't forget to put the type declaration or you will lose points!
mydrop :: Int -> [a] -> [a]
mydrop n [] = []
mydrop n (x:xs)
    | n <= 0 = x:xs
    | n == 1 = xs
    | otherwise = mydrop (n-1) xs




--- ### rev

-- don't forget to put the type declaration or you will lose points!
rev :: [a] -> [a]
rev [] = []
rev inputList = revHelper inputList []
    where
        revHelper :: [a] -> [a] -> [a]
        revHelper [] temp = temp --empty inputList, nothing left to put into temp
        revHelper (x:xs) temp = revHelper xs (x:temp) {-take head of current input, append to front of temp,
                                                        recursively repeat until entire list is reversed.-}
    --completed with help from ChatGPT for better understanding of Haskell prepending syntax and inspiration of methodology from StackOverflow


--- ### app

-- don't forget to put the type declaration or you will lose points!
-- x: list to append, y: destination list for the append
-- appends to front of list
app :: [a] -> [a] -> [a]
app [] y = y
app (x:xs) y = x : app xs y
--- ### inclist

-- don't forget to put the type declaration or you will lose points!
inclist :: Num a => [a] -> [a]
inclist [] = []
inclist (x:xs) = x + 1 : inclist xs
--- ### sumlist

-- don't forget to put the type declaration or you will lose points!
sumlist :: Num a => [a] -> a
sumlist [] = 0
sumlist list = sumlistHelper list 0
    where
        sumlistHelper :: Num a => [a] -> a -> a
        sumlistHelper [] sum = sum
        sumlistHelper (x:xs) sum = sumlistHelper xs (x+sum)

--- ### myzip

-- don't forget to put the type declaration or you will lose points!
myzip :: [a] -> [b] -> [(a,b)]
--myzip [] b = []
--myzip a [] = []
myzip a b = myzipHelper a b []
    where
        myzipHelper :: [a] -> [b] -> [(a,b)] -> [(a,b)]
        myzipHelper [] b zipped = zipped
        myzipHelper a [] zipped = zipped
        myzipHelper (x:xs) (y:ys) zipped =  (x,y): myzipHelper xs ys zipped
    -- assisted by ChatGPT in regards to how to use pairs
--- ### addpairs

-- don't forget to put the type declaration or you will lose points!
addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs a b = addpairsHelper (myzip a b)
    where
        addpairsHelper :: (Num a) => [(a,a)] -> [a]
        addpairsHelper [] = []
        addpairsHelper (x:xs) = (fst x + snd x) : addpairsHelper xs
    -- assisted by ChatGPT to find how to access values of a pair


--- ### ones

-- don't forget to put the type declaration or you will lose points!
ones :: [Integer]
ones = 1 : ones

--- ### nats

-- don't forget to put the type declaration or you will lose points!
nats :: [Integer]
nats = [0..]

--- ### fib

-- don't forget to put the type declaration or you will lose points!
fib :: [Integer]
fib = [0,1] P.++ addpairs fib (tail fib)

--- Set Theory
--- ----------

--- ### add

-- don't forget to put the type declaration or you will lose points!
add :: Ord a => a -> [a] -> [a]
add n [] = [n]
add n (x:xs)
    | n < x = n:(x:xs)
    | n > x = x: add n xs
    | otherwise = x:xs

--- ### union

-- don't forget to put the type declaration or you will lose points!
union :: Ord a => [a] -> [a] -> [a]
union [] b = b
union a [] = a
union (x:xs) (y:ys)
    | x == y = union xs (y:ys)
    | x < y = x : union xs (y:ys)
    | otherwise = y: union (x:xs) ys




--- ### intersect

-- don't forget to put the type declaration or you will lose points!
intersect :: Ord a => [a] -> [a] -> [a]
intersect [] b = []
intersect a [] = []
intersect (x:xs) (y:ys)
    | x < y = intersect xs (y:ys)
    | x > y = intersect (x:xs) ys
    | otherwise = x : intersect xs ys

--- ### powerset

-- don't forget to put the type declaration or you will lose points!
powerset :: Ord a => [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) =  union (powerset xs) (P.map (x:) (powerset xs))
    -- detailed explanation of powersets and ways to implement algorithms to calculate one assisted by ChatGPT
    -- applications of map function found from documentation on zvon.org

--- Higher Order Functions
--- ----------------------

--- ### inclist'

-- don't forget to put the type declaration or you will lose points!
inclist' :: Num a => [a] -> [a]
inclist' = P.map (+1)
    -- originally inclist' list = P.map(+1) list, VS Code suggestion simplified to current state

--- ### sumlist'

-- don't forget to put the type declaration or you will lose points!
sumlist' :: (Num a) => [a] -> a
sumlist' = P.sum
    -- details of sum function found from documentation on zvon.org