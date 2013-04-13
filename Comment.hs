{-# LANGUAGE OverloadedStrings #-}

module Comment (
   Comment (..)
  ,makeComment
  ,IDType
) where

import Data.Text
import Data.Time (UTCTime)
import Data.Map
import Text.JSON

type IDType = Integer

data Comment = Comment {
  name :: Text,
  body :: Text,
  posted :: UTCTime,
  pageId :: IDType
} deriving (Show, Eq, Ord)

makeComment :: Text -> Text -> UTCTime -> IDType -> Comment
makeComment = Comment

anonymousComment :: Text -> UTCTime -> IDType -> Comment
anonymousComment = makeComment "anonymous"

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
             name = n,
             body = b,
             posted = p,
             pageId = i
           }

  showJSON comment = JSObject $ toJSObject [
                      ("name", showJSON $ name comment),
                      ("body", showJSON $ body comment),
                      ("posted", showJSON $ show $ posted comment),
                      ("page_id", showJSON $ pageId comment)
                     ]
