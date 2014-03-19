{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts, GADTs #-}

module Model where

import Database.Persist
import Database.Persist.TH
import Data.Time (UTCTime)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
Page
  slug String
  UniquePage slug
Comment json
  name String
  body String
  posted UTCTime
  posted_posix Int
  pageId PageId Eq
  deriving (Show)
Rating json
  positive  Int
  negative  Int
  pageId PageId Eq
  UniqueRating pageId
  deriving (Show)
|]
