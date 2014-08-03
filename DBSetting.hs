module DBSetting (
   runSQLAction
 ) where

import Database.Persist.Postgresql
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Control.Monad.Logger (runNoLoggingT, NoLoggingT)
import qualified Data.ByteString.Char8 as BS

-- Should change this accroding to your environment!
{-
connectInfo :: ConnectInfo
connectInfo = defaultConnectInfo {
                connectHost = "localhost"
               ,connectUser = "kogarasi"
               ,connectPassword = "kogarasi"
               ,connectDatabase = "Kogarasi"
}
-}
connectInfo :: ConnectionString
connectInfo = BS.pack "user=koragasi dbname=kogarasi password=yourpassword"

runSQLAction :: SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runSQLAction = runNoLoggingT . runResourceT . withPostgresqlConn connectInfo . runSqlConn
