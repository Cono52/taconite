{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}

module Lib
    ( startApp
    ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.List                (sortBy)
import           Data.Ord                 (comparing)
import           Data.Time.Calendar
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

import           Control.Monad.Trans      (liftIO)
import           Database.MongoDB         (Action, Document, Document, Value,
                                           access, allCollections, close, connect, delete,
                                           exclude, find, findOne, host, insertMany,
                                           master, project, rest, select, sort,
                                           (=:))

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
           :<|> "sortedById" :> Get '[JSON] [User]

startApp :: IO ()
startApp = do
    putStrLn "Running on port 8000..."
    run 8000 app

app :: Application
app = serve api server

api :: Proxy UserAPI
api = Proxy

server :: Server UserAPI
server = return users
    :<|> return albert
    :<|> return isaac
    :<|> return sortedById


sortedById :: [User]
sortedById = sortById users

sortById :: [User] -> [User]
sortById = sortBy (comparing userId)

users :: [User]
users = [isaac, albert]

isaac :: User
isaac = User 372 "Isaac Newton" "isaac@newton.co.uk" (fromGregorian 1683 3 1)

albert :: User
albert = User 136 "Albert Einstein" "ae@mc2.org" (fromGregorian 1905 12 1)

runMongo functionToRun = do
    pipe <- connect (host "127.0.0.1")
    e <- access pipe master "test" functionToRun
    print e 
    close pipe

printdata =  runMongo allCollections

firstFile = runMongo $ findOne $ select [] "posts"