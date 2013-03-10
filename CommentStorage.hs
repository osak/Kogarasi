module CommentStorage (
    storeComment
) where

import Comment
import Database.HDBC
import Database.HDBC.MySQL

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
    stmt <- prepare conn "INSERT INTO comments (name, body, posted) VALUES (?, ?, ?)"
    execute stmt [toSql $ name comment, toSql $ body comment, toSql $ posted comment]
    commit conn
    disconnect conn
