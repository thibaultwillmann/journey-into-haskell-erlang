--1. Write a function isPalindrome, which takes a String and returns True if that String is a palindrome and False otherwise.
isPalindrome :: [Char] -> Bool
isPalindrome x = x == reverse x


--2. Write a function isPrefix, which takes 2 Strings and returns True if the first String is a prefix of the second String, and False otherwise.
isPrefix :: [Char] -> [Char] -> Bool
isPrefix x y = x == take (length x) y


--3.a. Write a function squareList, which takes an Int n and produces a list of the first n squares in descending order. 
--     (You may assume the input n is always non-negative.)
squareList :: Int -> [Int]
squareList 0 = [0]
squareList x = (x ^ 2) : squareList (x - 1)


--3.b. Write a function listSquare, which takes an Int n and produces a list of the first n squares in ascending order, using ++. 
--     (You may assume the input n is always non-negative.)
listSquare :: Int -> [Int]
listSquare 0 = [0]
listSquare x = listSquare (x - 1) ++ [x ^ 2]


--4. Write a function fact, which calculates the factorial of a given number by accumulating the result. 
--   The function should take an Integer as input and return an Integer as output. (You may assume the input is always non-negative)
fact :: Int -> Int
fact x = fact' x 1

fact' :: Int -> Int -> Int
fact' 0 x = x
fact' x y = fact' (x - 1) (x * y)


--5. In this exercise we will implement a credit card validation function using a checksum. Implement the following steps of the validation algorithm:

--5.a. Create a function, which converts positive Integers to a list of its digits. (For 0 and negative numbers it should return the empty list)
toDigits :: Integer -> [Integer]
toDigits x = if (x > 0) then toDigits (x `div` 10) ++ [x `mod` 10] else []

--5.b. Create a function that doubles every other number in a list beginning from the right - so it doubles the second-to-last number, the fourth-to-last number, etc.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = doubleEveryOther' x 1

doubleEveryOther' :: [Integer] -> Int -> [Integer]
doubleEveryOther' [] _ = []
doubleEveryOther' x y = if (y `mod` 2 == 0) then doubleEveryOther' (init x) (y + 1) ++ [2 * (last x)]
  else doubleEveryOther' (init x) (y + 1) ++ [last x]

--5.c. Create a function that takes a list of numbers (not necessarily one-digit numbers) and returns the sum of all the digits of all the numbers in it.
If the list is empty it returns 0.
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = if (x > 9) then sumDigits (xs ++ toDigits x) else x + sumDigits xs

--5.d. Create a function that indicates whether a given Integer is a valid credit card number by checking if the remainder of the given sum divided by 10 is equal to 0.
--     This will use all the functions defined in the previous sections.
validate :: Integer -> Bool
validate x = sumDigits (doubleEveryOther (toDigits x)) `mod` 10 == 0 
