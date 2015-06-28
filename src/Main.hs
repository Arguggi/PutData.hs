{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Main where

import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Maybe
import           Data.Text                as T
import           Database.HDBC
import           Database.HDBC.PostgreSQL
import           GHC.Generics
import           Network.Wai
import qualified Network.Wai.Handler.Warp as W
import           Servant

data Url = Url
  { urlId :: Integer
  , url   :: T.Text
  } deriving (Eq, Show, Generic)

data Status = Status
  { code    :: Integer
  , message :: T.Text
  } deriving (Eq, Show, Generic)

data NewUrl = NewUrl
  { newUrl :: T.Text
  } deriving (Eq, Show, Generic)

instance ToJSON Url
instance ToJSON Status
instance ToJSON NewUrl
instance FromJSON NewUrl

type UserAPI = "urls" :> Get '[JSON] [Url]
    :<|> "newurl" :> ReqBody '[JSON] NewUrl :> Post '[JSON] Status
    :<|> "url" :> Capture "uId" Integer :> Delete '[JSON] Status

userAPI :: Proxy UserAPI
userAPI = Proxy

server :: Server UserAPI
server = liftIO getUrls
    :<|> liftIO . addUrl
    :<|> liftIO . deleteUrl

getUrls :: IO [Url]
getUrls = do
    conn <- connectPostgreSQL "dbname = putdata"
    r <- quickQuery' conn "SELECT * from strings ORDER BY id DESC" []
    disconnect conn
    return $ fmap convRow r

addUrl :: NewUrl -> IO Status
addUrl insertUrl = do
    conn <- connectPostgreSQL "dbname = putdata"
    stmt <- prepare conn "INSERT INTO strings (string) VALUES (?)"
    rows <- execute stmt [toSql $ newUrl insertUrl]
    commit conn
    disconnect conn
    case rows of
        1 -> return Status { code = 0, message = "Url added successfully"}
        _ -> return Status { code = 1, message = "Error while adding url"}

deleteUrl :: Integer -> IO Status
deleteUrl delUrl = do
    conn <- connectPostgreSQL "dbname = putdata"
    stmt <- prepare conn "DELETE FROM strings WHERE id = ?"
    rows <- execute stmt [toSql delUrl]
    commit conn
    disconnect conn
    case rows of
        1 -> return Status { code = 0, message = "Url deleted successfully"}
        _ -> return Status { code = 1, message = "Error while deleting url"}

app :: Application
app = serve userAPI server

main :: IO ()
main = W.run 8081 app

convRow :: [SqlValue] -> Url
convRow [sqlId, sqlUrl] = Url { urlId = toUrlId, url = toUrl }
    where toUrlId = fromSql sqlId :: Integer
          toUrl  = fromMaybe "" (fromSql sqlUrl)
convRow _ = Url { urlId = 0, url = "" }
