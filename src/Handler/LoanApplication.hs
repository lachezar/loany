{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module Handler.LoanApplication where

import Import

import qualified Database.Esqueleto as E
import Database.Esqueleto ((?.), (^.))
import Database.Persist.Postgresql

import GHC.Float (double2Float)
import Numeric (showFFloat)

import LoanApplicationState
import Scoring

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
      liftIO $ atomically $ writeTVar maxSoughtAmountVar $ max currentAmount maxAmount
      loanApplicationKey <- persistLoanApplication loanApplicationResult loanApplication
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
        , lo ?. LoanOfferNominalInterestRate)
  defaultLayout $(widgetFile "all-loan-applications-page")

loanApplicationAForm :: Maybe LoanApplication -> AForm (HandlerFor App) LoanApplication
loanApplicationAForm mloanApplication =
  LoanApplication <$> areq textField "Name" (loanApplicationName <$> mloanApplication) <*>
  areq emailField "Email" (loanApplicationEmail <$> mloanApplication) <*>
  areq telField "Phone" (loanApplicationPhone <$> mloanApplication) <*>
  areq amountField "Amount" (loanApplicationAmount <$> mloanApplication) <*>
  pure NotScoredYet <*>
  lift (liftIO getCurrentTime)
  where
    amountField = checkBool (> 0) ("The amount must be positive integer" :: Text) intField

persistLoanApplication :: LoanApplicationResult Rational -> LoanApplication -> Handler LoanApplicationId
persistLoanApplication loanResult loanApplication = do
  currentTime <- liftIO getCurrentTime
  case loanResult of
    Offer interestRate -> do
      loanApplicationPK@(LoanApplicationKey (SqlBackendKey _laId)) <-
        runDB $ insert $ loanApplication {loanApplicationState = Approved, loanApplicationInsertedAt = currentTime}
      let offer =
            LoanOffer
              { loanOfferLoanApplicationId = loanApplicationPK
              , loanOfferNominalInterestRate = interestRate
              , loanOfferInsertedAt = currentTime
              }
      _insertedLoanOffer <- runDB $ insert offer
      return loanApplicationPK
    Denial -> do
      loanApplicationPK@(LoanApplicationKey (SqlBackendKey _laId)) <-
        runDB $ insert $ loanApplication {loanApplicationState = Denied, loanApplicationInsertedAt = currentTime}
      return loanApplicationPK

telField ::
     Monad m
  => RenderMessage (HandlerSite m) FormMessage =>
       Field m Text
telField =
  Field
    { fieldParse = parseHelper Right
    , fieldView =
        \theId name attrs val isReq ->
          [whamlet|
$newline never
<input id="#{theId}" name="#{name}" *{attrs} type="tel" pattern="^\+?\d{5,15}$" :isReq:required value="#{either id id val}">
|]
    , fieldEnctype = UrlEncoded
    }

formatRational :: Rational -> Text
formatRational r = pack $ showFFloat (Just 2) (double2Float $ fromRational r) ""