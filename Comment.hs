{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts, GADTs #-}

module Comment (
   Comment
  ,makeComment
  ,storeComment
  ,fetchCommentsByPageId
  ,commentName
  ,commentBody
  ,commentPosted
  ,commentPageId
  ,migrateAll
) where

import Data.Time (UTCTime)
import Data.Map (fromList, (!))
import Text.JSON
import Database.Persist
import Database.Persist.TH
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import DBSetting

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
Comment
  name String
  body String
  posted UTCTime
  pageId Int
  deriving (Show)
|]

makeComment :: String -> String -> UTCTime -> Int-> Comment
makeComment = Comment

anonymousComment :: String -> UTCTime -> Int-> Comment
anonymousComment = makeComment "anonymous"

storeComment :: Comment -> IO (Key Comment)
storeComment comment = runSQLAction $ do
  insert comment

fetchCommentsByPageId :: Int -> IO [Comment]
fetchCommentsByPageId i = runSQLAction $ do
  result <- selectList [CommentPageId ==. i] []
  return $ map entityVal result

instance JSON Comment where
  readJSON json = 
    case json of
      JSObject obj -> Ok $ go obj
      _ -> Error "Failed"
    where
      go obj = 
        let m = fromList $ fromJSObject obj
            n = case readJSON (m ! "name") of
                  Ok val -> val
                  _ -> error "No name"
            b = case readJSON (m ! "body") of
                  Ok val -> val
                  _ -> error "No body"
            p = case readJSON (m ! "posted") of
                  Ok val -> posixSecondsToUTCTime $ fromIntegral (val :: Int)
                  _ -> error "No posted"
            i = case readJSON (m ! "page_id") of
                  Ok val -> val
                  _ -> error "No page_id"
        in Comment {
             commentName = n,
             commentBody = b,
             commentPosted = p,
             commentPageId = i
           }

  showJSON comment = JSObject $ toJSObject [
                      ("name", showJSON $ commentName comment),
                      ("body", showJSON $ commentBody comment),
                      ("posted", showJSON $ posixSeconds $ commentPosted comment),
                      ("page_id", showJSON $ commentPageId comment)
                     ]
    where
      posixSeconds :: UTCTime -> Int
      posixSeconds = round . utcTimeToPOSIXSeconds
