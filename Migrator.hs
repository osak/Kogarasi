import Comment
import DBSetting
import Database.Persist.MySQL

main :: IO ()
main = runSQLAction $ do
  runMigration migrateAll
