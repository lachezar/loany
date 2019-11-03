module Scoring where

import System.Random

data LoanApplicationResult i = Offer Rational | Denial deriving (Show, Eq)

isPrime :: Int -> Bool
isPrime n
  | n == 1 = False
  | n `elem` [2, 3, 5, 7, 11, 13] = True
  | n `mod` 2 == 0 = False
  | otherwise = testPrimality n 3

testPrimality :: Int -> Int -> Bool
testPrimality n m
  | n `mod` m == 0 = False
  | m * m < n = testPrimality n $ m + 2
  | otherwise = True

score :: Int -> Int -> IO (LoanApplicationResult Rational)
score currentAmount maximumAmount
  | currentAmount <= maximumAmount = return Denial
  | isPrime currentAmount = return $ Offer 9.99
  | otherwise = do
    randomDouble <- randomIO :: IO Double
    let rationalInterestRate = toRational $ randomDouble * 8.0 + 4.0
    return $ Offer rationalInterestRate