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
                                                  else Left ("Key " ++ show field ++ " not found")

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
  inputs <- getInputs >>= return . fromList
  curtime <- liftIO getCurrentTime
  case validate inputs of
    Left message -> errorPage message
    Right _ -> go inputs curtime
  where
    go :: InputDictionary -> UTCTime -> CGI CGIResult
    go dict posted = do
      let comment = createCommentFrom dict posted
      liftIO $ storeComment comment
      successPage comment

setGeneralHeaders :: CGI ()
setGeneralHeaders = do
  setHeader "Content-type" "text/html"

fetch :: CGI CGIResult
fetch = undefined

errorPage :: String -> CGI CGIResult
errorPage message = do
  setGeneralHeaders
  outputError 400 message []

successPage :: Comment -> CGI CGIResult
successPage comment = do
  setGeneralHeaders
  output "<html><body>Success</body></html>"

cgiMain :: CGI CGIResult
cgiMain = do
  action <- getInput "action"
  case fromMaybe "" action of
    "store" -> store
    "fetch" -> fetch
    _ -> errorPage "Bad Request"

main :: IO ()
main = runCGI $ handleErrors cgiMain
