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
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Text (pack, strip)

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

getUTF8Inputs :: CGI [(String, String)]
getUTF8Inputs = getInputsFPS >>= return . map (\(a,x) -> (a,toString x))

-- Store action.
store :: CGI CGIResult
store = do
  inputs <- getUTF8Inputs >>= return . fromList
  curtime <- liftIO getCurrentTime
  case validate ["name", "body", "slug"] inputs of
    Left message -> errorPage message
    Right _ -> case strip $ pack (inputs ! "body") of
                 "" -> errorPage "Body should not be empty"
                 _ -> go inputs curtime
  where
    go :: InputDictionary -> UTCTime -> CGI CGIResult
    go dict posted = do
      comment <- liftIO $ createCommentFrom dict posted
      liftIO $ storeComment comment
      successPage [comment]

-- Fetch action.
fetch :: CGI CGIResult
fetch = do
  inputs <- getUTF8Inputs >>= return . fromList
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
  setHeader "Pragma" "no-cache"
  setHeader "Cache-control" "no-cache"

errorPage :: String -> CGI CGIResult
errorPage message = do
  setHeader "Content-type" "text/html"
  setGeneralHeaders
  outputError 400 message []

successPage :: [Comment] -> CGI CGIResult
successPage comments = do
  setGeneralHeaders
  setHeader "Content-type" "text/plain"
  output $ unpack $ encode $ toJSON comments

cgiMain :: CGI CGIResult
cgiMain = do
  method <- requestMethod
  case method of
    "GET" -> errorPage "GET is not supported"
    "POST" -> do
      action <- getInput "action"
      case fromMaybe "" action of
        "store" -> store
        "fetch" -> fetch
        _ -> errorPage "Bad Request"

main :: IO ()
main = runCGI $ handleErrors cgiMain
