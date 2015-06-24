{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Maybe
import           Database.HDBC
import           Database.HDBC.PostgreSQL
import           GHC.Generics
import           Network.Wai
import qualified Network.Wai.Handler.Warp as W
import           Servant

data Url = Url
  { aid :: Integer
  , url :: String
  } deriving (Eq, Show, Generic)

instance ToJSON Url

type UserAPI = "urls" :> Get '[JSON] [Url]

userAPI :: Proxy UserAPI
userAPI = Proxy

server :: Server UserAPI
server = do
    conn <- liftIO $ connectPostgreSQL "dbname = putdata"
    r <- liftIO $ quickQuery' conn "SELECT * from strings" []
    liftIO $ disconnect conn
    return $ fmap convRow r

app :: Application
app = serve userAPI server

main :: IO ()
main = W.run 8081 app

convRow :: [SqlValue] -> Url
convRow [sqlId, sqlUrl] = Url { aid = toUrlId, url = toUrl }
    where toUrlId = fromSql sqlId :: Integer
          toUrl  = fromMaybe "" (fromSql sqlUrl)
convRow _ = Url { aid = 0, url = "" }
