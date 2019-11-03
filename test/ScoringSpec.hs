{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ScoringSpec
  ( spec
  ) where

import Data.List (maximum)
import Scoring
import TestImport

spec :: Spec
spec =
  withApp $ do
    let primes =
          [ 2
          , 3
          , 5
          , 7
          , 11
          , 13
          , 17
          , 19
          , 23
          , 29
          , 31
          , 37
          , 41
          , 43
          , 47
          , 53
          , 59
          , 61
          , 67
          , 71
          , 73
          , 79
          , 83
          , 89
          , 97
          , 101
          , 103
          , 107
          , 109
          , 113
          , 127
          , 131
          , 137
          , 139
          , 149
          , 151
          , 157
          , 163
          , 167
          , 173
          , 179
          , 181
          , 191
          , 193
          , 197
          , 199
          , 211
          , 223
          , 227
          , 229
          , 233
          , 239
          , 241
          , 251
          , 257
          , 263
          , 269
          , 271
          , 277
          , 281
          , 283
          , 293
          , 307
          , 311
          , 313
          , 317
          , 331
          , 337
          , 347
          , 349
          , 353
          , 359
          , 367
          , 373
          , 379
          , 383
          , 389
          , 397
          , 401
          , 409
          , 419
          , 421
          , 431
          , 433
          , 439
          , 443
          , 449
          , 457
          , 461
          , 463
          , 467
          , 479
          , 487
          , 491
          , 499
          , 503
          , 509
          , 521
          , 523
          , 541
          ]
    describe "Prime checking" $ do
      it "tests correctly the first 100 prime numbers" $
        assertEq "The first 100 prime numbers are checked as primes" True $ foldl' (&&) True $ map isPrime primes
      it "tests correctly the non-primes until the 100th prime number" $
        let m = Data.List.maximum primes
         in assertEq "checks correct non-primes" False $
            foldl' (||) False [isPrime x | x <- [1 .. m], x `notElem` primes]
    describe "Loan application scoring" $ do
      it "denies applications" $ do
        result <- liftIO $ score 10 11
        assertEq "denies" Denial result
      it "approves prime amounts with rate of 9.99" $ do
        result <- liftIO $ score 11 5
        assertEq "approves" (Offer 9.99) result
      it "approves non-prime amounts with random rate" $ do
        result <- liftIO $ score 12 5
        case result of
          Denial -> assertEq "It should not be denied" True False
          Offer interestRate ->
            assertEq "with random interest rate" True $ interestRate >= minRate && interestRate <= maxRate
            where minRate = 4 :: Rational
                  maxRate = 12 :: Rational