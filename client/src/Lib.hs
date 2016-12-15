{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Lib
    ( startApp
    ) where


import           Control.Monad            (forever, when)
import           Control.Monad.Trans      (liftIO)
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Char8    as C
import           Data.Char
import           Data.List                (sortBy)
import           Data.Ord                 (comparing)
import           Data.Proxy
import           Data.String
import           Data.Time.Calendar
import           Data.Typeable
import           GHC.Generics
import           Network.HTTP.Client      (defaultManagerSettings, newManager)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.API
import           Servant.Client
import           System.Exit
import           System.Random



startApp :: IO ()
startApp = forever $ do
    l <- getLine
    when (l == "quit") $ do
            putStrLn ""
            putStrLn "goodbye..."
            exitSuccess
    when (l == "saveFile") $ do
            putStrLn "saved!\n"
    when (l == "readFile") $ do
            putStrLn ""
            putStrLn "here you go..."
    when (l == "login") $ do
            putStrLn ""
            putStrLn "Please Enter Username"


--AuthApi

data PublicKey = PublicKey { pubkey :: Int } deriving (Show, Generic, FromJSON, ToJSON)

data Token = Token
  { token :: Int
  } deriving (Show, Generic, FromJSON, ToJSON)

login :: Maybe String -> ClientM Token

getPublicKey :: ClientM PublicKey

type AuthAPI = "login" :> QueryParam "username" String :> Get '[JSON] Token
      :<|> "getPublicKey" :> Get '[JSON] PublicKey

authAPI :: Proxy AuthAPI
authAPI = Proxy

(login :<|> getPublicKey) = client authAPI

--getKey :: Maybe String -> PublicKey
getKey :: IO()
getKey = do
  manager <- newManager defaultManagerSettings
  res <- runClientM getPublicKey (ClientEnv manager (BaseUrl Http "127.0.0.1" 8001 ""))
  case res of
    Left err     -> putStrLn $ "Error: " ++ show err
    Right pubkey -> print pubkey
