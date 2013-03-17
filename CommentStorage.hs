module CommentStorage (
    storeComment
   ,fetchCommentsByPageId
) where

import Comment
import Database.HDBC
import Database.HDBC.MySQL
import Data.Map (Map, (!))
import Data.Time (localTimeToUTC, utc)

connectInfo :: MySQLConnectInfo
connectInfo = defaultMySQLConnectInfo { 
    mysqlHost = "localhost", 
    mysqlUser = "osak", 
    mysqlPassword = "test",
    mysqlDatabase = "Kogarasi",
    mysqlUnixSocket = "/run/mysqld/mysqld.sock"
}

connection :: IO Connection
connection = connectMySQL connectInfo

storeComment :: Comment -> IO ()
storeComment comment = do
    conn <- connection
    stmt <- prepare conn "INSERT INTO comments (name, body, posted, page_id) VALUES (?, ?, ?, ?)"
    execute stmt [toSql $ name comment, toSql $ body comment, toSql $ posted comment, toSql $ pageId comment]
    commit conn
    disconnect conn

fetchCommentsByPageId :: IDType -> IO [Comment]
fetchCommentsByPageId pid = do
    conn <- connection
    stmt <- prepare conn "SELECT name, body, posted, page_id FROM comments WHERE page_id = ?"
    execute stmt [toSql pid]
    rows <- fetchAllRowsMap stmt
    return $ map fromMap rows

fromMap :: Map String SqlValue -> Comment
fromMap row = Comment {
    name = fromSql $ row ! "name",
    body = fromSql $ row ! "body",
    posted = localTimeToUTC utc . fromSql $ row ! "posted",
    pageId = fromSql $ row ! "page_id"
}
