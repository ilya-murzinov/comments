import           Control.Concurrent           (forkIO, threadDelay)
import           Database.PostgreSQL.Embedded (DBConfig (..),
                                               StartupConfig (..), Version (..),
                                               startPostgres, stopPostgres)
import           Network.HTTP.Client          (Manager, defaultManagerSettings,
                                               newManager)
import           Servant.API
import           Servant.Client


import           API
import           Persistence
import           Server
import           Types

main :: IO ()
main = do
    let sConfig = StartupConfig True (Version "9.6.5-1")
    let dConfig = DBConfig 46782 "postgres"
    rc <- startPostgres sConfig dConfig

    let dbConnection = "host=127.0.0.1 user=postgres dbname=postgres port=46782"
    pool <- createDefaultPool dbConnection

    _ <- forkIO $ startServer 8081 pool
    threadDelay 200000

    let getThreadE :<|> createThreadE :<|> _ :<|> _ = client api
    let queries = do
            _ <- createThreadE $ PartialThread $ Just "test title"
            getThreadE $ ThreadId 1

    manager <- newManager defaultManagerSettings
    err <- execute manager (getThreadE $ ThreadId 1)
    print err
    res <- execute manager queries
    print res

    stopPostgres rc
    where
        execute :: Manager -> ClientM a -> IO (Either ServantError a)
        execute manager req = runClientM req (ClientEnv manager (BaseUrl Http "localhost" 8081 ""))
