module Main where

import Database.Cassandra.CQL as Cassandra
import Data.Text (pack)
import Control.Monad.Trans (liftIO)
import Control.Concurrent (ThreadId, myThreadId, forkIO, threadDelay, killThread)
import System.Posix.Signals
import System.Posix.Types (ProcessID)
import System.Posix.Process

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
      putStrLn "Driver : Starting Broker"
      b <- forkProcess $ startBroker (Frontend $ "tcp://*:" ++ show fePort)
                     (Backend $ "tcp://*:" ++ show bePort)
      threadDelay 1000000
      putStrLn "Driver : Creating Table"
      pool <- newPool ([head servers]) keyspace Nothing
      runCas pool $ createTable cTABLE_NAME
      putStrLn "Driver : Starting servers"
      s1 <- forkProcess $ runServer [("127.0.0.1","9042")] keyspace ns
      s2 <- forkProcess $ runServer [("127.0.0.2","9042")] keyspace ns
      s3 <- forkProcess $ runServer [("127.0.0.3","9042")] keyspace ns
      threadDelay 1000000
      putStrLn "Driver : Starting client"
      res <- runSession ns $ return ()
      -- Install handler for Ctrl-C
      tid <- myThreadId
      installHandler keyboardSignal (Catch $ reportSignal pool [b,s1,s2,s3] tid) Nothing
      threadDelay (10000000)
      -- Woken up..
      mapM_ (signalProcess sigTERM) [b,s1,s2,s3]
      putStrLn "Driver : Dropping Table"
      runCas pool $ dropTable cTABLE_NAME
      return ()


reportSignal :: Pool -> [ProcessID] -> ThreadId -> IO ()
reportSignal pool procList mainTid = do
  putStrLn "Handling keyboard signal"
  mapM_ killProc procList 
  runCas pool $ dropTable cTABLE_NAME
  killThread mainTid
  where 
    killProc pid = signalProcess sigTERM pid
