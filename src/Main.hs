{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Main where

import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString          as B
import           Data.Text                as T
import           Data.Text.Encoding       as TE
import           Database.Redis           as Redis
import           GHC.Generics
import           Network.Wai
import qualified Network.Wai.Handler.Warp as W
import           Servant

data Url = Url
  { url :: T.Text
  } deriving (Eq, Show, Generic)

data Stat = Stat
  { code    :: Integer
  , message :: T.Text
  } deriving (Eq, Show, Generic)

data NewUrl = NewUrl
  { newUrl :: T.Text
  } deriving (Eq, Show, Generic)

instance ToJSON Url
instance ToJSON Main.Stat
instance ToJSON NewUrl
instance FromJSON NewUrl

listName :: B.ByteString
listName = "putdata"

type UserAPI = "urls" :> Get '[JSON] [Url]
    :<|> "newurl" :> ReqBody '[JSON] NewUrl :> Post '[JSON] Stat
    :<|> "url" :> Capture "uString" Text :> Delete '[JSON] Stat

userAPI :: Proxy UserAPI
userAPI = Proxy

server :: Connection -> Server UserAPI
server conn = liftIO (getUrls conn)
    :<|> (liftIO . addUrl conn)
    :<|> (liftIO . deleteUrl conn)

getUrls :: Connection -> IO [Url]
getUrls conn = do
    r <- Redis.runRedis conn (Redis.lrange listName 0 (-1))
    case r of
        (Left _) -> return []
        (Right values) -> return $ (fmap (Url . TE.decodeUtf8) values)

addUrl :: Connection -> NewUrl -> IO Stat
addUrl conn insertUrl = do
    r <- Redis.runRedis conn (Redis.lpush listName [TE.encodeUtf8 . newUrl $ insertUrl])
    case r of
        (Left _) -> return Main.Stat { code = 1, message = "Error while adding url"}
        (Right _) -> return Main.Stat { code = 0, message = "Url added successfully"}

deleteUrl :: Connection -> Text -> IO Stat
deleteUrl conn delUrl = do
    r <- Redis.runRedis conn (Redis.lrem listName 1 (TE.encodeUtf8 delUrl))
    case r of
        (Left _) -> return Main.Stat { code = 1, message = "Error while deleting url"}
        (Right _) -> return Main.Stat { code = 0, message = "Url deleted successfully"}

app :: Connection -> Application
app conn  = serve userAPI $ server conn

main :: IO ()
main = do
    conn <- Redis.connect Redis.defaultConnectInfo
    W.run 8081 $ app conn
