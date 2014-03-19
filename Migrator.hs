import DBSetting
import Database.Persist.MySQL
import Control.Monad.IO.Class
import Model

main :: IO ()
main = runSQLAction $ do
  runMigration migrateAll
