{-# LANGUAGE FlexibleContexts #-}
module CommentStorage (
    storeComment
) where

import Comment
import Database.HDBC
import Database.HDBC.MySQL
import Data.Map (Map, (!))

connectInfo :: MySQLConnectInfo
connectInfo = defaultMySQLConnectInfo { 
    mysqlHost = "localhost", 
    mysqlUser = "osa", 
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

fromMap :: Map String SqlValue -> Comment
fromMap row = Comment {
    name = fromSql $ row ! "name",
    body = fromSql $ row ! "body",
    posted = fromSql $ row ! "posted",
    pageId = fromSql $ row ! "page_id"
}
