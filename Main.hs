import Comment
import CommentStorage
import System.Environment
import Data.Time
import Data.Text (pack)

store :: [String] -> IO ()
store (nameStr:bodyStr:postedStr:pageIdStr:_) = 
    let posted = read postedStr :: UTCTime
        pageId = read pageIdStr :: IDType
    in storeComment $ Comment (pack nameStr) (pack bodyStr) posted pageId
store _ = fail "Insufficient arguments"

fetch :: [String] -> IO ()
fetch (pageIdStr:_) = do
    let pageId = read pageIdStr :: IDType
    comments <- fetchCommentsByPageId pageId
    mapM_ (putStrLn . show) comments
fetch _ = fail "Too few arguments"

process :: [String] -> IO ()
process ("store":as) = store as
process ("fetch":as) = fetch as
process _            = fail "Invalid command."

main :: IO ()
main = do
    args <- getArgs
    process args
