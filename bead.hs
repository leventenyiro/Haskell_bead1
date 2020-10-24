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

-- Prímek
isPrime2 :: Integer -> Bool
isPrime2 n = length [x | x <- [1..n], n `mod` x == 0] == 2

isPrime :: Integer -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime n = odd n && null [d | d <- [3,5..squareRoot n], n `mod` d == 0] where
  squareRoot :: Integer -> Integer
  squareRoot n = floor (sqrt (fromIntegral n))

primeList :: [Integer]
primeList = 2:[ x | x <- [3,5..], isPrime x]

-- 4. feladat
