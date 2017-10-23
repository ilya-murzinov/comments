import           Control.Concurrent           (forkIO, threadDelay)
import           Database.PostgreSQL.Embedded (DBConfig (..),
                                               StartupConfig (..), Version (..),
                                               startPostgres, stopPostgres)
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
    _ <- forkIO $ startServer 8081 "host=127.0.0.1 user=postgres dbname=postgres port=46782"
    threadDelay 200000

    let getThread :<|> createThread :<|> _ :<|> _ = client api
    let queries = do
            _ <- createThread $ PartialThread "test" $ Just "test title"
            getThread $ ThreadIdentifier "test"

    manager <- newManager defaultManagerSettings
    err <- execute manager (getThread $ ThreadIdentifier "test")
    print err
    res <- execute manager queries
    print res

    stopPostgres rc
    where
        execute :: Manager -> ClientM a -> IO (Either ServantError a)
        execute manager req = runClientM req (ClientEnv manager (BaseUrl Http "localhost" 8081 ""))
