{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (concat)
import Comment
import CommentStorage
import Network.CGI
import Data.Maybe
import Data.List (foldl', intercalate)
import Data.Map hiding (foldl', map)
import Data.Time (UTCTime, getCurrentTime)
import Data.Text (Text, pack, unpack, concat)

type InputDictionary = Map String String

validate :: [String] -> InputDictionary -> Either String InputDictionary
validate fields dict = foldl' check (Right dict) fields
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
  case validate ["name", "body", "pageId"] inputs of
    Left message -> errorPage message
    Right _ -> go inputs curtime
  where
    go :: InputDictionary -> UTCTime -> CGI CGIResult
    go dict posted = do
      let comment = createCommentFrom dict posted
      liftIO $ storeComment comment
      successPage [comment]

setGeneralHeaders :: CGI ()
setGeneralHeaders = do
  setHeader "Content-type" "text/html"

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

commentTable :: [Comment] -> String
commentTable comments = 
  unpack $ concat ["<table><tr><th>Name</th><th>Body</th><th>PageID</th></tr>", concat $ map commentRow comments, "</table>"]
  where
    commentRow :: Comment -> Text
    commentRow comment = 
      let nameStr = name comment
          bodyStr = body comment
          pageIdStr = pack . show $ pageId comment
      in concat ["<tr><td>", nameStr, "</td><td>", bodyStr, "</td><td>", pageIdStr, "</td></tr>"]

errorPage :: String -> CGI CGIResult
errorPage message = do
  setGeneralHeaders
  outputError 400 message []

successPage :: [Comment] -> CGI CGIResult
successPage comments = do
  setGeneralHeaders
  let t = commentTable comments
  output ("<html><body>" ++ t ++ "</body></html>")

cgiMain :: CGI CGIResult
cgiMain = do
  action <- getInput "action"
  case fromMaybe "" action of
    "store" -> store
    "fetch" -> fetch
    _ -> errorPage "Bad Request"

main :: IO ()
main = runCGI $ handleErrors cgiMain
