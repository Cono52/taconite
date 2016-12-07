{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Lib
    ( startApp
    ) where

import           Data.Aeson
import           Data.Proxy
import           Data.Aeson.TH
import           Data.List                (sortBy)
import           Data.Ord                 (comparing)
import           Data.String
import           Data.Char
import           Data.Time.Calendar
import qualified Data.ByteString.Char8 as C
import           System.Random
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.HTTP.Client (newManager, defaultManagerSettings)
import           Servant
import           Servant.Client
import           Servant.API
import           GHC.Generics
import           Control.Monad.Trans      (liftIO)




--Auth API

data Token = Token 
  { token :: Int
  } deriving (Show, Generic, FromJSON, ToJSON) 


startApp :: IO ()
startApp = do
    putStrLn "Running on port 8001..."
    run 8001 app

app :: Application
app = serve api server

type API = "login" :> QueryParam "username" String :> Get '[JSON] Token
      :<|> "getPublicKey" :> Get '[JSON] PublicKey

api :: Proxy API
api = Proxy

server :: Server API
server = login
    :<|> return getPublicKey

login :: Maybe String -> Handler Token
login uname = liftIO $ do
  e <- getUsers
  print e
  return $ Token(10)

--encryption code
data PublicKey = PublicKey { pubkey :: Int } deriving (Show, Generic, FromJSON, ToJSON)
data PrivateKey = PrivateKey { prikey :: Int } deriving (Show, Generic, FromJSON, ToJSON)

modulto = 256
testPub = PublicKey(123)
testPri = PrivateKey(133)

generateKeys :: IO Int
generateKeys = randomRIO(1, modulto - 1) --traped in IO 

encrypt :: String -> PublicKey -> String
encrypt str p =  map (\a -> chr $ ((ord a) + (pubkey p)) `mod` modulto) str

decrypt :: String -> PrivateKey -> String
decrypt str p =  map (\a -> chr $ ((ord a) + (prikey p)) `mod` modulto) str

getPublicKey :: PublicKey
getPublicKey = testPub

-- All the stuff used to interact with the DB from here on
data User = User
  { username :: String
  , password :: String
  } deriving (Show, Generic, FromJSON, ToJSON)


data UserFile = UserFile 
  { file :: String
  } deriving (Show, Generic, FromJSON, ToJSON)

data ResponseData = ResponseData
  { response :: String
  } deriving Generic

instance ToJSON ResponseData
instance FromJSON ResponseData


users :: Maybe String -> ClientM [User]

saveFile :: UserFile -> ClientM ResponseData

type DataAPI = "users" :> QueryParam "search" String :> Get '[JSON] [User]
          :<|> "saveFile" :> ReqBody '[JSON] UserFile :> Post '[JSON] ResponseData

dataAPI :: Proxy DataAPI
dataAPI = Proxy


(users :<|> saveFile) = client dataAPI


queries :: ClientM [User]
queries = do
  u <- users (Just "")
  return u

getUsers :: IO ()
getUsers = do
  manager <- newManager defaultManagerSettings
  res <- runClientM queries (ClientEnv manager (BaseUrl Http "127.0.0.1" 8000 ""))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (users) -> do
      print users