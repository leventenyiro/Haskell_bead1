module Bead where

type Dictionary = [(Char, Integer)]

dictionary :: [Char] -> Dictionary
dictionary x = zip x [1..]

dictionary_az :: Dictionary
dictionary_az = dictionary ['a'..'z']

dictionary_az_AZ :: Dictionary
dictionary_az_AZ = dictionary (['a'..'z'] ++ ['A'..'Z'])

-- 1. feladat
charToNum :: Dictionary -> Char -> Integer
charToNum dict c
    | result == [] = 0
    | otherwise = snd(head result)
    where
        result = [x | x <- dict, fst x == c]

-- 2. feladat
numToChar :: Dictionary -> Integer -> Char
numToChar dict n
    | result == [] = '*'
    | otherwise = fst(head result)
    where
        result = [x | x <- dict, snd x == n]

-- 3. feladat
translate :: Dictionary -> String -> [Integer]
translate dict str
    | result == [] = []
    | otherwise = result
    where result = [charToNum dict y | y <- [x | x <- str]]

isPrime :: Integer -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime n = odd n && null [d | d <- [3,5..squareRoot n], n `mod` d == 0] where
  squareRoot :: Integer -> Integer
  squareRoot n = floor (sqrt (fromIntegral n))

primeList :: [Integer]
primeList = 2:[ x | x <- [3,5..], isPrime x]

-- 4. feladat abba >> 2^1 * 3^2 * 5^2 * 7^1

encode :: Dictionary -> String -> Integer
encode dict str
    | result == [] = 1
    | otherwise = product [powTuple x | x <- result]
    where
        result = zip primeList (translate [x | x <- dict] str)
        powTuple (a,b) = a ^ b

-- 5. feladat - prímfaktorizáció

primeFactorization :: Integer -> [Integer]
primeFactorization a 
    | a <= 1 = []
    | factor == [] = [a]
    | otherwise = factor ++ primeFactorization (a `div` (head factor))
    where 
        factor = take 1 [p | p <- [2..a-1], a `mod` p == 0]

-- 6. feladat - dekódolás