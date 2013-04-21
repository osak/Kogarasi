{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts, GADTs #-}

module Comment (
   Comment
  ,makeComment
) where

import Data.Time (UTCTime)
import Data.Map (fromList, (!))
import Text.JSON
import Database.Persist
import Database.Persist.TH
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
                  Ok val -> read val :: UTCTime
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
                      ("posted", showJSON $ show $ commentPosted comment),
                      ("page_id", showJSON $ commentPageId comment)
                     ]
