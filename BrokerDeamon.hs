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


main :: IO ()
main =
  let fe = Frontend $ "tcp://" ++ broker ++ ":" ++ (show fePort) in
  let be = Backend  $ "tcp://" ++ broker ++ ":" ++ (show bePort) in
	let ns = mkNameService fe be "localhost" 5560 in
    do
      putStrLn "Driver : Starting Broker"
      startBroker (Frontend $ "tcp://*:" ++ show fePort)
                     (Backend $ "tcp://*:" ++ show bePort)


reportSignal :: Pool -> [ProcessID] -> ThreadId -> IO ()
reportSignal pool procList mainTid = do
  putStrLn "Handling keyboard signal"
  mapM_ killProc procList 
  runCas pool $ dropTable cTABLE_NAME
  killThread mainTid
  where 
    killProc pid = signalProcess sigTERM pid
