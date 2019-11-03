{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module Handler.LoanApplication where

import Import
import Data.Typeable

import Database.Persist.Postgresql
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.), (?.))


import LoanApplicationState
import Scoring
import Numeric (showFFloat)
import GHC.Float (double2Float)

-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo :: FileInfo
    , fileDescription :: Text
    }

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getLoanApplicationR :: Handler Html
getLoanApplicationR = do
  (formWidget, formEnctype) <- generateFormPost $ renderDivs $ loanApplicationAForm Nothing
  defaultLayout $(widgetFile "loan-application-page")

postLoanApplicationR :: Handler Html
postLoanApplicationR = do
  maxSoughtAmountVar <- fmap maxSoughtAmount getYesod
  ((result, formWidget), formEnctype) <- runFormPost $ renderDivs $ loanApplicationAForm Nothing
  case result of
    FormSuccess loanApplication -> do
      maxAmount <- readTVarIO maxSoughtAmountVar
      loanApplicationResult <- liftIO $ score currentAmount maxAmount
      liftIO $ print $ show loanApplicationResult
      liftIO $ atomically $ writeTVar maxSoughtAmountVar $ max currentAmount maxAmount
      loanApplicationKey <- persisteLoanApplication loanApplicationResult loanApplication
      -- redirect based on the loan application result
      case loanApplicationResult of
        Offer _ -> redirect $ LoanApplicationOfferR loanApplicationKey
        Denial -> redirect LoanApplicationDenialR
      where currentAmount = loanApplicationAmount loanApplication
    _ -> liftIO $ print ("some error in the form" :: String)
  defaultLayout $(widgetFile "loan-application-page")

getLoanApplicationOfferR :: LoanApplicationId -> Handler Html
getLoanApplicationOfferR loanApplicationId = do
  loanApplication <- runDB $ get404 loanApplicationId
  loanOffer <- runDB $ get404 loanOfferId
  let interestRate = formatRational $ loanOfferNominalInterestRate loanOffer
  defaultLayout $(widgetFile "loan-application-offer-page")
  where
    loanOfferId = LoanOfferKey {unLoanOfferKey = loanApplicationId}

getLoanApplicationDenialR :: Handler Html
getLoanApplicationDenialR = defaultLayout $(widgetFile "loan-denial-page")


getAllLoanApplicationsR :: Handler Html
getAllLoanApplicationsR = do
  loanApplications <-
    runDB $
    E.select $
    E.from $ \(la `E.LeftOuterJoin` lo) -> do
      E.on (E.just (la ^. LoanApplicationId) E.==. lo E.?. LoanOfferLoanApplicationId)
      E.orderBy [E.desc (la ^. LoanApplicationInsertedAt)]
      return
        ( la ^. LoanApplicationName
        , la ^. LoanApplicationEmail
        , la ^. LoanApplicationPhone
        , la ^. LoanApplicationAmount
        , la ^. LoanApplicationState
        , la ^. LoanApplicationInsertedAt
        , lo ?. LoanOfferNominalInterestRate
--        , maybe "" (^. LoanOfferNominalInterestRate) lo
--      , E.coalesceDefault [lo ^. LoanOfferNominalInterestRate] (E.val "")
         )
  liftIO $ print $ show $ typeOf loanApplications
  defaultLayout $(widgetFile "all-loan-applications-page")


loanApplicationAForm :: Maybe LoanApplication -> AForm (HandlerFor App) LoanApplication
loanApplicationAForm mloanApplication = LoanApplication
    <$> areq textField "Name" (loanApplicationName <$> mloanApplication)
    <*> areq emailField "Email"  (loanApplicationEmail  <$> mloanApplication)
    <*> areq telField "Phone"  (loanApplicationPhone  <$> mloanApplication)
    <*> areq amountField "Amount"  (loanApplicationAmount <$> mloanApplication)
    <*> pure NotScoredYet
    <*> lift (liftIO getCurrentTime)
  where
    amountField = checkBool (> 0) ("The amount must be positive integer" :: Text) intField

persisteLoanApplication :: LoanApplicationResult Rational -> LoanApplication -> Handler LoanApplicationId
persisteLoanApplication loanResult loanApplication = do
  currentTime <- liftIO getCurrentTime
  case loanResult of
    Offer interestRate -> do
      loanApplicationPK@(LoanApplicationKey (SqlBackendKey _laId)) <- runDB $ insert $ loanApplication {loanApplicationState = Approved, loanApplicationInsertedAt = currentTime}
      liftIO $ print $ show loanApplicationPK
      let offer =
            LoanOffer
              { loanOfferLoanApplicationId = loanApplicationPK
              , loanOfferNominalInterestRate = interestRate
              , loanOfferInsertedAt = currentTime
              }
      insertedLoanOffer <- runDB $ insert offer
      liftIO $ print $ show insertedLoanOffer
      return loanApplicationPK
    Denial -> do
      liftIO $ print ("the loan application was denied" :: String)
      loanApplicationPK@(LoanApplicationKey (SqlBackendKey _laId)) <- runDB $ insert $ loanApplication {loanApplicationState = Denied, loanApplicationInsertedAt = currentTime}
      liftIO $ print $ show loanApplicationPK
      return loanApplicationPK


telField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Text
telField = Field
    { fieldParse = parseHelper Right
    , fieldView = \theId name attrs val isReq ->
        [whamlet|
$newline never
<input id="#{theId}" name="#{name}" *{attrs} type="tel" pattern="^\+?\d{5,15}$" :isReq:required value="#{either id id val}">
|]
    , fieldEnctype = UrlEncoded
    }

formatRational :: Rational -> Text
formatRational r = pack $ showFFloat (Just 2) (double2Float $ fromRational r) ""