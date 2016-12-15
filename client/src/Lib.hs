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
