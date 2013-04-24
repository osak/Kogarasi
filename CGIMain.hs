{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (concat)
import Comment
import Network.CGI
import Data.Maybe
import Data.List (foldl')
import Data.Map hiding (foldl', map)
import Data.Time (UTCTime, getCurrentTime)
import Text.JSON

type InputDictionary = Map String String

-- Check if requires fields are all exists.
-- If some fields absent, return error message as Left.
validate :: [String] -> InputDictionary -> Either String InputDictionary
validate fields dict = foldl' check (Right dict) fields
  where
    check acc field = acc >> if member field dict then acc
                                                  else Left ("Key " ++ show field ++ " not found")

-- Create comment from given dictionary and time.
createCommentFrom :: InputDictionary -> UTCTime -> Comment
createCommentFrom dict posted = let
  name = dict ! "name"
  body = dict ! "body"
  pageId = read $ dict ! "pageId"
  in makeComment name body posted pageId

-- Store action.
store :: CGI CGIResult
store = do
  inputs <- getInputs >>= return . fromList
  curtime <- liftIO getCurrentTime
  case validate ["name", "body", "pageId"] inputs of
    Left message -> errorPage message
    Right _ -> go inputs curtime
  where
    go :: InputDictionary -> UTCTime -> CGI CGIResult
    go dict posted = do
      let comment = createCommentFrom dict posted
      liftIO $ storeComment comment
      successPage [comment]

-- Fetch action.
fetch :: CGI CGIResult
fetch = do
  inputs <- getInputs >>= return . fromList
  case validate ["pageId"] inputs of
    Left message -> errorPage message
    Right _ -> go inputs
  where
    go :: InputDictionary -> CGI CGIResult
    go dict = do
      comments <- liftIO $ fetchCommentsByPageId (read (dict ! "pageId"))
      successPage comments

setGeneralHeaders :: CGI ()
setGeneralHeaders = do
  setHeader "Content-type" "text/html"

errorPage :: String -> CGI CGIResult
errorPage message = do
  setGeneralHeaders
  outputError 400 message []

successPage :: [Comment] -> CGI CGIResult
successPage comments = do
  setGeneralHeaders
  output $ encode $ showJSONs comments

cgiMain :: CGI CGIResult
cgiMain = do
  action <- getInput "action"
  case fromMaybe "" action of
    "store" -> store
    "fetch" -> fetch
    _ -> errorPage "Bad Request"

main :: IO ()
main = runCGI $ handleErrors cgiMain
