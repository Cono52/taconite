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
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.Exit             
import           GHC.Generics
import           Control.Monad.Trans      (liftIO)
import           Control.Monad            (forever, when)


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
