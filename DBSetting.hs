module DBSetting (
   runSQLAction
 ) where

import Database.Persist.MySQL
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Control.Monad.Logger (runNoLoggingT, NoLoggingT)

-- Should change this accroding to your environment!
connectInfo :: ConnectInfo
connectInfo = defaultConnectInfo {
                connectHost = "localhost"
               ,connectUser = "kogarasi"
               ,connectPassword = "kogarasi"
               ,connectDatabase = "Kogarasi"
}

runSQLAction :: SqlPersist (ResourceT (NoLoggingT IO)) a -> IO a
runSQLAction = runNoLoggingT . runResourceT . withMySQLConn connectInfo . runSqlConn
