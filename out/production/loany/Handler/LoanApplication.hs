{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module Handler.LoanApplication where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import Data.Time
import Data.Fixed


import LoanApplicationState

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
  ((result, formWidget), formEnctype) <- runFormPost $ renderDivs $ loanApplicationAForm Nothing
  let submission =
        case result of
          FormSuccess res -> Just res
          _ -> Nothing
  liftIO $ print ("----------------------------------------- hello world" :: String)
  liftIO $ print $ maybe "nothing" loanApplicationName submission
  case result of
    FormSuccess res -> do
      insertedLoanApplication <- runDB $ insert res
      liftIO $ print $ show insertedLoanApplication
    _ -> liftIO $ print ("some error in the form" :: String)
  liftIO $ print ("----------------------------------------- hello world 222" :: String)
  defaultLayout $(widgetFile "loan-application-page")

getLoanApplicationOfferR :: LoanApplicationId -> Handler Html
getLoanApplicationOfferR loanApplicationId = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe FileForm
        handlerName = "getHomeR" :: Text
    allComments <- runDB $ getAllComments

    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

getLoanApplicationRejectionR :: Handler Html
getLoanApplicationRejectionR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe FileForm
        handlerName = "getHomeR" :: Text
    allComments <- runDB $ getAllComments

    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")


getAllLoanApplicationsR :: Handler Html
getAllLoanApplicationsR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe FileForm
        handlerName = "getHomeR" :: Text
    allComments <- runDB $ getAllComments

    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

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

sampleForm :: Form FileForm
sampleForm = renderBootstrap3 BootstrapBasicForm $ FileForm
    <$> fileAFormReq "Choose a file"
    <*> areq textField textSettings Nothing
    -- Add attributes like the placeholder and CSS classes.
    where textSettings = FieldSettings
            { fsLabel = "What's on the file?"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control")
                , ("placeholder", "File description")
                ]
            }

commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")

getAllComments :: DB [Entity Comment]
getAllComments = selectList [] [Asc CommentId]


telField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Text
telField = Field
    { fieldParse = parseHelper $ Right
    , fieldView = \theId name attrs val isReq ->
        [whamlet|
$newline never
<input id="#{theId}" name="#{name}" *{attrs} type="tel" pattern="^\+?\d{5,15}$" :isReq:required value="#{either id id val}">
|]
    , fieldEnctype = UrlEncoded
    }

mkUTCTime :: (Integer, Int, Int)
          -> (Int, Int, Pico)
          -> UTCTime
mkUTCTime (year, mon, day) (hour, min, sec) =
  UTCTime (fromGregorian year mon day)
          (timeOfDayToTime (TimeOfDay hour min sec))