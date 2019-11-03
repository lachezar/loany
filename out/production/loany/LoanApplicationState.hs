{-# LANGUAGE TemplateHaskell #-}
module LoanApplicationState where

import Database.Persist.TH
import Prelude

data LoanApplicationState = NotScoredYet | Approved | Denied
    deriving (Show, Read, Eq)
derivePersistField "LoanApplicationState"