{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Main where

import qualified Data.List as L
import Prelude 
import Database.Cassandra.CQL as Cassandra
import Data.Text (pack)
import Data.String
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Control.Concurrent (ThreadId, myThreadId, forkIO, threadDelay, killThread)
import System.Posix.Signals
import System.Posix.Types (ProcessID)
import System.Posix.Process
import System.Random (randomRIO)
import Control.Monad (replicateM_, foldM, when, forever)
import Data.Time 
import System.Environment
import Control.Concurrent.MVar

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

numOpsPerRound :: Num a => a
numOpsPerRound = 1

main :: IO ()
main =
  let fe = Frontend $ "tcp://" ++ broker ++ ":" ++ (show fePort) in
  let be = Backend  $ "tcp://" ++ broker ++ ":" ++ (show bePort) in
	let ns = mkNameService fe be "localhost" 5560 
	in do
      -- ARGS
      -- Ask the number of rounds and the number of concurrent clinets
      args <- getArgs  
      let rounds = (read $ L.head args) :: Int
      print $ "rounds: " ++ (show rounds)
      let threads = (read $ L.last args) :: Int 
      print $ "threads: " ++ (show threads) 
      
      someTime <- getCurrentTime
      mv::(MVar NominalDiffTime)<- newEmptyMVar
      t1 <- getCurrentTime
      putStrLn "Driver : Starting client"
      replicateM_ threads $ forkIO $ do
          key <- liftIO $ newKey
	  avgLatency <- runSession ns $ foldM (clientCore  key someTime) 0 [1 .. rounds]
          print avgLatency
	  --putMVar mv avgLatency
     -- totalLat <- foldM (\l _ -> takeMVar mv >>= \newL -> return $ l + newL) 0 [1..threads]
      t2 <- getCurrentTime  
      --putStrLn $ "Throughput (ops/s) = " ++ (show $ (fromIntegral $ numOpsPerRound * rounds * threads) / (diffUTCTime t2 t1))
      --putStrLn $ "Latency (s) = " ++ (show $ (totalLat / fromIntegral threads))




clientCore :: Key-> UTCTime -> NominalDiffTime -> Int -> CSN NominalDiffTime
clientCore  key someTime avgLat round = do
  -- Generate key
  t1 <- liftIO $ getCurrentTime
  (initVal :: Int) <- liftIO $ randomRIO (1,1000) 
  write key initVal
  -- 1: Increment
  val <- readKey key
  write key (val + 1)
  
  -- 2: Decrement
  val <- readKey key
  write key (val - 1)
  
  -- 3: Increment
  val <- readKey key
  write key (val + 1)


  t2 <- liftIO $ getCurrentTime
  let timeDiff = diffUTCTime t2 t1
  let newAvgLat = ((timeDiff / numOpsPerRound) + (avgLat * (fromIntegral $ round - 1))) / (fromIntegral round)
  -- Print info if required
  liftIO . putStrLn $ "Round = " ++ show round ++ " result = " ++ "Rsult" ++ " latency = " ++ show newAvgLat
  return newAvgLat

















