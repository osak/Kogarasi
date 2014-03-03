{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts, GADTs #-}

module Comment (
   Comment
  ,makeComment
  ,storeComment
  ,fetchCommentsBySlug
  ,commentName
  ,commentBody
  ,commentPosted
  ,commentPageId
  ,Comment.migrateAll
) where

import Data.Time (UTCTime)
import Database.Persist
import Database.Persist.TH
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import DBSetting
import Page

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
Comment json
  name String
  body String
  posted UTCTime
  posted_posix Int
  pageId PageId Eq
  deriving (Show)
|]

makeComment :: String -> String -> UTCTime -> String -> IO Comment
makeComment name body posted slug = do
  maybePage <- runSQLAction $ getBy $ UniquePage slug
  pageId <- case maybePage of
              Nothing -> newPage slug
              Just page -> return $ entityKey page
  let postedPOSIX = posixSeconds posted
  return $ Comment name body posted postedPOSIX pageId
  where
    posixSeconds :: UTCTime -> Int
    posixSeconds = round . utcTimeToPOSIXSeconds


storeComment :: Comment -> IO (Key Comment)
storeComment comment = runSQLAction $ do
  insert comment

fetchCommentsBySlug :: String -> IO [Comment]
fetchCommentsBySlug slug = runSQLAction $ do
  maybePage <- getBy $ UniquePage slug
  case maybePage of
    Nothing -> return []
    Just page -> selectList [CommentPageId ==. entityKey page] [] >>= return . map entityVal

{-
fetchCommentsByPageId :: Int -> IO [Comment]
fetchCommentsByPageId i = runSQLAction $ do
  result <- selectList [CommentPageId ==. i] []
  return $ map entityVal result
-}

{-
instance JSON Comment where
  readJSON json = 
    case json of
      JSObject obj -> Ok $ go obj
      _ -> Error "Failed"
    where
      go :: JSObject JSValue -> Comment
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
-}
