{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Prelude hiding (read)
import Database.Cassandra.CQL as Cassandra
import Data.Text (pack)
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Control.Concurrent (ThreadId, myThreadId, forkIO, threadDelay, killThread)
import System.Posix.Signals
import System.Posix.Types (ProcessID)
import System.Posix.Process
import System.Random (randomRIO)

import Shoppr.NameService.LoadBalancingBroker
import Shoppr.NameService.Types
import Shoppr.Session
import Shoppr.Types
import Shoppr.Consts (cTABLE_NAME)
import Shoppr.Server (runServer)
import Shoppr.DBDriver

broker = "localhost"
fePort :: Int
fePort = 5558

bePort :: Int
bePort = 5559

keyspace :: Keyspace
keyspace = Keyspace $ pack "shoppr"

servers :: [Cassandra.Server]
servers = [("127.0.0.1","9042"),
           ("127.0.0.2","9042"),
           ("127.0.0.3","9042")]

main :: IO ()
main =
  let fe = Frontend $ "tcp://" ++ broker ++ ":" ++ (show fePort) in
  let be = Backend  $ "tcp://" ++ broker ++ ":" ++ (show bePort) in
	let ns = mkNameService fe be "localhost" 5560 in
    do
      putStrLn "Driver : Creating Table"
      pool <- newPool ([head servers]) keyspace Nothing
      runCas pool $ createTable cTABLE_NAME
      return ()

