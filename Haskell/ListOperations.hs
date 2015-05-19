module ListOperations where

-- This function and its helper function find what index an element a is located at. Returns -1 if not found
whatIndex :: (Eq a) => a -> [a] -> Integer
index :: (Num a, Eq a1) => a1 -> [a1] -> a -> a

whatIndex num list = index num list 0
index _ [] _ = -1
index num (x:xs) i =
    if num == x
    then i
    else index num xs (i+1)
    
-- This function adds an integer a to a list. It is done by list comprehension and recursion
adda_list_comp :: Integer -> [Integer] -> [Integer]
adda_list_rec :: Integer -> [Integer] -> [Integer]

adda_list_comp a xs = [ x + a | x <- xs]
adda_list_rec a [] = []
adda_list_rec a (x:xs) = (x+a): adda_list_rec a xs

--This function cubes all odds and squares all even numbers of a list
cubeOdds :: [Integer] -> [Integer]
cubeOdds list = [ if (odd x) == True then x^3 else x^2 | x <- list] 

--This function and its helper function finds the minimum number in a list
listMin :: (Ord a) => [a] -> a
min' :: Ord a => a -> [a] -> a

listMin (x:xs) = min' x xs

min' x [] = x
min' x (y:ys) =
     if (y > x)
	 then min' x ys
     else min' y ys

    
    