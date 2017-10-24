import           Control.Concurrent           (forkIO, threadDelay)
import           Database.PostgreSQL.Embedded (DBConfig (..),
                                               StartupConfig (..), Version (..),
                                               startPostgres, stopPostgres)
import           Database.PostgreSQL.Simple   (connectPostgreSQL)
import           Network.HTTP.Client          (Manager, defaultManagerSettings,
                                               newManager)
import           Servant.API
import           Servant.Client


import           API                          (api)
import           Server                       (startServer)
import           Types

main :: IO ()
main = do
    let sConfig = StartupConfig True (Version "9.6.5-1")
    let dConfig = DBConfig 46782 "postgres"
    rc <- startPostgres sConfig dConfig
    conn <- connectPostgreSQL "host=127.0.0.1 user=postgres dbname=postgres port=46782"
    _ <- forkIO $ startServer 8081 conn
    threadDelay 200000

    let getThread :<|> createThread :<|> _ :<|> _ = client api
    let queries = do
            _ <- createThread $ PartialThread $ Just "test title"
            getThread $ ThreadId 1

    manager <- newManager defaultManagerSettings
    err <- execute manager (getThread $ ThreadId 1)
    print err
    res <- execute manager queries
    print res

    stopPostgres rc
    where
        execute :: Manager -> ClientM a -> IO (Either ServantError a)
        execute manager req = runClientM req (ClientEnv manager (BaseUrl Http "localhost" 8081 ""))
