{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Control.Monad.IO.Class (liftIO)
import Data.Map hiding (map)
import Control.Applicative
import Data.Time (UTCTime, getCurrentTime)
import Data.Text.Lazy
import Network.HTTP.Types (mkStatus)
import Model
import Comment
import Rating

type InputDictionary = Map Text Text

-- Create comment from given dictionary and time.
createCommentFrom :: InputDictionary -> UTCTime -> IO Comment
createCommentFrom dict posted = let
  name = unpack $ dict ! "name"
  body = unpack $ dict ! "body"
  slug = unpack $ dict ! "slug"
  in makeComment name body posted slug

main :: IO ()
main = scotty 3123 $ do
  get "/show/:slug" $ do
    slug <- param "slug"
    comments <- liftIO $ fetchCommentsBySlug slug
    addHeader "Pragma" "no-cache"
    addHeader "Cache-control" "no-cache"
    json comments
    addHeader "Content-Type" "application/json; charset=utf-8"
  get "/rating/:slug" $ do
    slug <- param "slug"
    rating <- liftIO $ fetchRatingBySlug slug
    addHeader "Pragma" "no-cache"
    addHeader "Cache-control" "no-cache"
    json rating
    addHeader "Content-Type" "application/json; charset=utf-8"
  post "/post" $ do
    dict <- fromList <$> params
    case strip $ dict ! "body" of
      "" -> status $ mkStatus 400 "Body should not be empty"
      _ -> do
        curtime <- liftIO $ getCurrentTime
        comment <- liftIO $ createCommentFrom dict curtime
        _ <- liftIO $ storeComment comment
        json comment
  post "/rating/:slug/positive" $ do
    slug <- param "slug"
    liftIO $ positiveVote slug
    text "Success"
  post "/rating/:slug/negative" $ do
    slug <- param "slug"
    liftIO $ negativeVote slug
    text "Success"
