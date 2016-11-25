{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Main where

import           Control.Monad.Trans (liftIO)
import           Database.MongoDB    (Action, Document, Document, Value, access,
                                      close, connect, delete, exclude, find,
                                      host, insertMany, master, project, rest,
                                      select, sort, (=:))

import           Lib

main :: IO ()
main = startApp

runMongo dbName functionToRun = do
    pipe <- connect (host "127.0.0.1")
    e <- access pipe master dbName functionToRun
    close pipe
