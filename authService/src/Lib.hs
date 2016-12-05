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
import           Servant
import           GHC.Generics
import           Control.Monad.Trans      (liftIO)



data PublicKey = PublicKey { pubkey :: Int }
data PrivateKey = PrivateKey { prikey :: Int }


data User = User
  { userId   :: Int
  , userName :: String
  , email    :: String
  , userReg  :: Day
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type UserAPI = "users" :> Get '[JSON] [User]
           :<|> "albert" :> Get '[JSON] User
           :<|> "isaac" :> Get '[JSON] User


startApp :: IO ()
startApp = do
    putStrLn "Running on port 8001..."
    run 8001 app

app :: Application
app = serve api server

api :: Proxy UserAPI
api = Proxy

server :: Server UserAPI
server = return users
    :<|> return albert
    :<|> return isaac


users :: [User]
users = [isaac, albert]

isaac :: User
isaac = User 372 "Isaac Newton" "isaac@newton.co.uk" (fromGregorian 1683 3 1)

albert :: User
albert = User 136 "Albert Einstein" "ae@mc2.org" (fromGregorian 1905 12 1)

modulto = 256
testPub = PublicKey(123)
testPri = PrivateKey(133)

generateKeys :: IO Int
generateKeys = randomRIO(1, modulto - 1) --traped in IO 

encrypt :: String -> PublicKey -> String
encrypt str p =  map (\a -> chr $ ((ord a) + (pubkey p)) `mod` modulto) str

decrypt :: String -> PrivateKey -> String
decrypt str p =  map (\a -> chr $ ((ord a) + (prikey p)) `mod` modulto) str




