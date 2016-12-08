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
import           Data.Attoparsec.ByteString
import           Data.Bson.Generic
import           Data.List                  (sortBy)
import qualified Data.List                  as DL
import           Data.Maybe                 (mapMaybe)
import           Data.Ord                   (comparing)
import           Data.Time.Calendar

import           Control.Monad.Trans        (liftIO)
import           Database.MongoDB
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

data User = User
  { username :: String
  , password :: String
  } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON)


data UserFile = UserFile
  { file :: String
  } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON)

deriving instance FromBSON String
deriving instance ToBSON   String

data ResponseData = ResponseData
  { response :: String
  } deriving Generic

instance ToJSON ResponseData
instance FromJSON ResponseData


type UserAPI = "getUserByName" :> QueryParam "search" String :> Get '[JSON] [User]
          :<|> "saveFile" :> ReqBody '[JSON] UserFile :> Post '[JSON] ResponseData

startApp :: IO ()
startApp = do
    putStrLn "Running on port 8000..."
    run 8000 app

app :: Application
app = serve api server

api :: Proxy UserAPI
api = Proxy

server :: Server UserAPI
server = getUserByName
    :<|> saveFile


runMongo functionToRun = do
    pipe <- connect (host "127.0.0.1")
    e <- access pipe master "filebase" functionToRun
    print e
    close pipe

returnMongo :: Action IO a0 -> IO a0
returnMongo functionToRun = do
    pipe <- connect (host "127.0.0.1")
    e <- access pipe master "filebase" functionToRun
    close pipe
    return e

printdata =  runMongo allCollections

firstFile = runMongo $ findOne $ select [] "files"

findAllFiles = runMongo $ find (select [] "files") >>= rest

insertFile :: Document -> IO()
insertFile fileToPost = runMongo $ insert "files" fileToPost

deleteFile :: Document -> IO()
deleteFile doc = runMongo $ delete $ select doc "files"

saveFile :: UserFile -> Handler ResponseData
saveFile userfile = liftIO $ do
    e <- insertFile ( toBSON userfile )
    return $ ResponseData (file userfile) --make response data the same string from the file


getUserByName :: Maybe String -> Handler [User]
getUserByName uname = liftIO $ do
    print "hit"
    docs <- returnMongo $ find (select ["username" =: uname] "users") >>= rest
    return $ mapMaybe (\ b ->  fromBSON b :: Maybe User) docs
