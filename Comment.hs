{-# LANGUAGE OverloadedStrings #-}

module Comment (
   Comment (..)
  ,makeComment
) where

import Data.Text
import Data.Time (UTCTime)

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
