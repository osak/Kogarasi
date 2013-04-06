{-# LANGUAGE OverloadedStrings #-}
import Comment
import CommentStorage
import Network.CGI
import Data.Maybe
import Data.List (foldl')
import Data.Map hiding (foldl')
import Data.Time (UTCTime, getCurrentTime)
import Data.Text (pack)

type InputDictionary = Map String String

requiredFields :: [String]
requiredFields = ["name", "body", "pageId"]

validate :: InputDictionary -> Either String InputDictionary
validate dict = foldl' check (Right dict) requiredFields
  where
    check acc field = acc >> if member field dict then acc
                                                  else Left ("Key " ++ field ++ " not found")

createCommentFrom :: InputDictionary -> UTCTime -> Comment
createCommentFrom dict posted = let
  name = pack $ dict ! "name"
  body = pack $ dict ! "body"
  pageId = read $ dict ! "pageId"
  in Comment {
     name = name,
     body = body,
     posted = posted,
     pageId = pageId
  }

store :: CGI CGIResult
store = do
  inputs <- getInputs
  curtime <- liftIO getCurrentTime
  go $ createCommentFrom (fromList inputs) curtime
  where
    go :: Comment -> CGI CGIResult
    go comment = (liftIO $ storeComment comment) >> 
                 successPage comment

fetch :: CGI CGIResult
fetch = undefined

errorPage :: String -> CGI CGIResult
errorPage = undefined

successPage :: Comment -> CGI CGIResult
successPage = undefined

cgiMain :: CGI CGIResult
cgiMain = do
  action <- getInput "action"
  case fromMaybe "" action of
    "store" -> store
    "fetch" -> fetch
    _ -> errorPage ""

main :: IO ()
main = runCGI $ cgiMain
