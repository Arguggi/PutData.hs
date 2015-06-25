{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Main where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Maybe
import           Data.Text                  as T
import           Database.HDBC
import           Database.HDBC.PostgreSQL
import           GHC.Generics
import           Network.Wai
import qualified Network.Wai.Handler.Warp   as W
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
    :<|> "url" :> Capture "uId" Int :> Delete '[JSON] Status

userAPI :: Proxy UserAPI
userAPI = Proxy

server :: Server UserAPI
server = urls
    :<|> newurl
    :<|> deleteUrl

    where urls :: EitherT ServantErr IO [Url]
          urls = do
              conn <- liftIO $ connectPostgreSQL "dbname = putdata"
              r <- liftIO $ quickQuery' conn "SELECT * from strings" []
              liftIO $ disconnect conn
              return $ fmap convRow r

          newurl :: NewUrl -> EitherT ServantErr IO Status
          newurl insertUrl = do
              conn <- liftIO $ connectPostgreSQL "dbname = putdata"
              stmt <- liftIO $ prepare conn "INSERT INTO strings (string) VALUES (?)"
              rows <- liftIO $ execute stmt [toSql $ newUrl insertUrl]
              liftIO $ commit conn
              liftIO $ disconnect conn
              case rows of
                  1 -> return Status { code = 0, message = "Url added successfully"}
                  _ -> return Status { code = 1, message = "Error while adding url"}

          deleteUrl :: Int -> EitherT ServantErr IO Status
          deleteUrl delUrl = do
              conn <- liftIO $ connectPostgreSQL "dbname = putdata"
              stmt <- liftIO $ prepare conn "DELETE FROM strings WHERE id = ?"
              rows <- liftIO $ execute stmt [toSql delUrl]
              liftIO $ commit conn
              liftIO $ disconnect conn
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
