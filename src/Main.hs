{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Main where

import           Control.Monad.IO.Class
import           Control.Exception        (bracket)
import           Data.Aeson
import qualified Data.ByteString          as B
import qualified Crypto.Hash              as Crypto
import           Data.Text                as T
import           Data.Text.Encoding       as TE
import           Database.Redis           as Redis
import           GHC.Generics
import qualified Network.Wai              as Wai
import qualified Network.Wai.Handler.Warp as W
import           Servant

data Url = Url
  { hash :: T.Text
  , url :: T.Text
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
    :<|> "url" :> Capture "uHash" Text :> Delete '[JSON] Stat

userAPI :: Proxy UserAPI
userAPI = Proxy

server :: Connection -> Server UserAPI
server conn = liftIO (getUrls conn)
    :<|> (liftIO . addUrl conn)
    :<|> (liftIO . deleteUrl conn)

getUrls :: Connection -> IO [Url]
getUrls conn = do
    r <- Redis.runRedis conn (Redis.hgetall listName)
    case r of
        (Left _) -> return []
        (Right values) -> return $ fmap toUrl values

addUrl :: Connection -> NewUrl -> IO Stat
addUrl conn insertUrl = do
    let stringHash = hexHash . newUrl $ insertUrl
    r <- Redis.runRedis conn $ Redis.hset listName stringHash (TE.encodeUtf8 . newUrl $ insertUrl)
    case r of
        (Left _) -> return Main.Stat { code = 1, message = "Error while adding url"}
        (Right _) -> return Main.Stat { code = 0, message = "Url added successfully"}

deleteUrl :: Connection -> Text -> IO Stat
deleteUrl conn delHash = do
    r <- Redis.runRedis conn (Redis.hdel listName [TE.encodeUtf8 delHash])
    case r of
        (Left _) -> return Main.Stat { code = 1, message = "Error while deleting url"}
        (Right 0) -> return Main.Stat { code = 1, message = "Error while deleting url"}
        (Right _) -> return Main.Stat { code = 0, message = "Url deleted successfully"}

app :: Connection -> Wai.Application
app conn  = serve userAPI $ server conn

main :: IO ()
main = bracket
         (Redis.connect Redis.defaultConnectInfo)
         (`Redis.runRedis` Redis.quit)
         (W.run 8081 . app)

hexHash :: Text -> B.ByteString
hexHash = Crypto.digestToHexByteString . hashDigest . TE.encodeUtf8
    where
    hashDigest :: B.ByteString -> Crypto.Digest Crypto.SHA1
    hashDigest = Crypto.hash

toUrl :: (B.ByteString, B.ByteString) -> Url
toUrl (hHash, hString) = Url (TE.decodeUtf8 hHash) (TE.decodeUtf8 hString)
