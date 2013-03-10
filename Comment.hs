{-# LANGUAGE OverloadedStrings #-}

module Comment (
   Comment
  ,name
  ,body
  ,posted
) where

import Data.Text
import Data.Time (UTCTime)

data Comment = Comment {
  name :: Text,
  body :: Text,
  posted :: UTCTime
} deriving (Show, Eq, Ord)

anonymousComment :: Text -> UTCTime -> Comment
anonymousComment = Comment "anonymous"
