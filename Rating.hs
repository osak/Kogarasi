{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts, GADTs #-}

module Rating where

import Control.Applicative
import Database.Persist
import Database.Persist.TH
import DBSetting
import Model
import Page
import Control.Monad.IO.Class

newRating :: PageId -> IO RatingId
newRating pageId = runSQLAction $ insert $ Rating 0 0 pageId

vote :: [Update Rating] -> String -> IO ()
vote upd slug = runSQLAction $ do
  pageId <- liftIO $ fetchPageIdBySlug slug
  maybeRating <- liftIO $ runSQLAction $ getBy $ UniqueRating pageId
  ratingId <- liftIO $ case maybeRating of
                Nothing -> newRating pageId
                Just rating -> return $ entityKey rating
  update ratingId upd

positiveVote :: String -> IO ()
positiveVote = vote [RatingPositive +=. 1]

negativeVote :: String -> IO ()
negativeVote = vote [RatingNegative +=. 1]

fetchRatingBySlug :: String -> IO Rating
fetchRatingBySlug slug = do
  pageId <- fetchPageIdBySlug slug
  maybeRating <- runSQLAction $ getBy $ UniqueRating pageId
  case maybeRating of
    Nothing -> do
      ratingId <- newRating pageId
      runSQLAction $ getJust $ ratingId
    Just rating -> return $ entityVal rating
