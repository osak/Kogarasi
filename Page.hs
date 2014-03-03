{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts, GADTs #-}

module Page where

import Database.Persist
import Database.Persist.TH
import DBSetting

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
Page
  slug String
  UniquePage slug
|]

newPage :: String -> IO PageId
newPage slug = runSQLAction $ do
  insert $ Page slug

fetchPageIdBySlug :: String -> IO PageId
fetchPageIdBySlug slug = do
  maybePage <- runSQLAction $ getBy $ UniquePage slug
  case maybePage of
    Nothing -> newPage slug
    Just page -> return $ entityKey page
