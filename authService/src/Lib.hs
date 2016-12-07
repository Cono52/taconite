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



data PublicKey = PublicKey { pubkey :: Int }
data PrivateKey = PrivateKey { prikey :: Int }


--type UserAPI = "users" :> Get '[JSON] [User]
--           :<|> "albert" :> Get '[JSON] User
--           :<|> "isaac" :> Get '[JSON] User


--startApp :: IO ()
--startApp = do
--   putStrLn "Running on port 8001..."
--    run 8001 app

--app :: Application
--app = serve api server

--api :: Proxy UserAPI
--api = Proxy

--server :: Server UserAPI
--server = return users
 --   :<|> return albert
 --   :<|> return isaac

modulto = 256
testPub = PublicKey(123)
testPri = PrivateKey(133)

generateKeys :: IO Int
generateKeys = randomRIO(1, modulto - 1) --traped in IO 

encrypt :: String -> PublicKey -> String
encrypt str p =  map (\a -> chr $ ((ord a) + (pubkey p)) `mod` modulto) str

decrypt :: String -> PrivateKey -> String
decrypt str p =  map (\a -> chr $ ((ord a) + (prikey p)) `mod` modulto) str


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

startApp :: IO ()
startApp = do
  manager <- newManager defaultManagerSettings
  res <- runClientM queries (ClientEnv manager (BaseUrl Http "127.0.0.1" 8000 ""))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (users) -> do
      print users