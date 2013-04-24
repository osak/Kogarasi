module DBSetting (
   runSQLAction
 ) where

import Database.Persist.MySQL
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Control.Monad.Logger (runNoLoggingT, NoLoggingT)

connectInfo :: ConnectInfo
connectInfo = defaultConnectInfo {
                connectHost = "localhost"
               ,connectUser = "osak"
               ,connectPassword = "test"
               ,connectDatabase = "Kogarasi"
}

runSQLAction :: SqlPersist (ResourceT (NoLoggingT IO)) a -> IO a
runSQLAction = runNoLoggingT . runResourceT . withMySQLConn connectInfo . runSqlConn
