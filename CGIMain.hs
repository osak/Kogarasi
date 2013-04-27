{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (concat)
import Comment
import Network.CGI
import Data.Maybe
import Data.List (foldl')
import Data.Map hiding (foldl', map)
import Data.Time (UTCTime, getCurrentTime)
import Data.Aeson (toJSON, encode)
import Data.ByteString.Lazy.Char8 (unpack)

type InputDictionary = Map String String

-- Check if requires fields are all exists.
-- If some fields absent, return error message as Left.
validate :: [String] -> InputDictionary -> Either String InputDictionary
validate fields dict = foldl' check (Right dict) fields
  where
    check acc field = acc >> if member field dict then acc
                                                  else Left ("Key " ++ show field ++ " not found")

-- Create comment from given dictionary and time.
createCommentFrom :: InputDictionary -> UTCTime -> IO Comment
createCommentFrom dict posted = let
  name = dict ! "name"
  body = dict ! "body"
  slug = dict ! "slug"
  in makeComment name body posted slug

-- Store action.
store :: CGI CGIResult
store = do
  inputs <- getInputs >>= return . fromList
  curtime <- liftIO getCurrentTime
  case validate ["name", "body", "slug"] inputs of
    Left message -> errorPage message
    Right _ -> go inputs curtime
  where
    go :: InputDictionary -> UTCTime -> CGI CGIResult
    go dict posted = do
      comment <- liftIO $ createCommentFrom dict posted
      liftIO $ storeComment comment
      successPage [comment]

-- Fetch action.
fetch :: CGI CGIResult
fetch = do
  inputs <- getInputs >>= return . fromList
  case validate ["slug"] inputs of
    Left message -> errorPage message
    Right _ -> go inputs
  where
    go :: InputDictionary -> CGI CGIResult
    go dict = do
      comments <- liftIO $ fetchCommentsBySlug (dict ! "slug")
      successPage comments

setGeneralHeaders :: CGI ()
setGeneralHeaders = do
  setHeader "Content-type" "text/plain"
  setHeader "Pragma" "no-cache"
  setHeader "Cache-control" "no-cache"

errorPage :: String -> CGI CGIResult
errorPage message = do
  setGeneralHeaders
  outputError 400 message []

successPage :: [Comment] -> CGI CGIResult
successPage comments = do
  setGeneralHeaders
  output $ unpack $ encode $ toJSON comments

cgiMain :: CGI CGIResult
cgiMain = do
  action <- getInput "action"
  case fromMaybe "" action of
    "store" -> store
    "fetch" -> fetch
    _ -> errorPage "Bad Request"

main :: IO ()
main = runCGI $ handleErrors cgiMain
