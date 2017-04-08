{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Shoppr.Session (
	CSN,
  runSession,
  read,
  write,
  newKey
) where

import Prelude hiding (read)
import Control.Lens
import Database.Cassandra.CQL
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Trans.State
import Control.Monad.Trans (liftIO)
import Control.Lens
import Data.Serialize hiding (get, put)
import qualified System.ZMQ4 as ZMQ4
import Control.Applicative ((<$>))
import System.Random (randomIO)
import Control.Concurrent (threadDelay)
import Data.UUID

import Shoppr.Types
import Shoppr.NameService.Types
import Shoppr.Consts (cTABLE_NAME)
import Shoppr.Marshall (decodeResponse)


runSession :: Show a => NameService -> CSN a -> IO a
runSession ns comp = do
  session <- beginSession ns
  res <- evalStateT comp session
  endSession session
  liftIO $ putStrLn $ "Result of evaluating CSN: "++(show res)
  return res

read :: Key -> CSN Int
read key = do
  s <- get
  let seqNo = case M.lookup key $ s^.seqMap of 
                Nothing -> 1
                Just s -> s
  let req = encode $ Request cTABLE_NAME (Rd key) (s^.sessid) seqNo
  liftIO $ ZMQ4.send (s^.server) [] req
  responseBlob <- liftIO $ ZMQ4.receive (s^.server)
  let Response _ (Just (val,seqNo')) = decodeResponse responseBlob
  -- liftIO $ putStrLn $ "Is "++(show seqNo')++" >= "++(show seqNo)++"?"
  -- liftIO $ putStrLn $ "read received val = "++(show val)
  if seqNo' == seqNo - 1
  then return val
  else do
    liftIO $ threadDelay 100000
    liftIO $ putStrLn "Retrying read..."
    read key

write :: Key -> Int -> CSN ()
write key val = do
  s <- get
  let seqNo = case M.lookup key $ s^.seqMap of 
                Nothing -> 1
                Just s -> s
  let req = encode $ Request cTABLE_NAME (Wr key val) (s^.sessid) seqNo
  liftIO $ ZMQ4.send (s^.server) [] req
  responseBlob <- liftIO $ ZMQ4.receive (s^.server)
  let Response seqNo' _ = decodeResponse responseBlob
  let newSeqMap = M.insert key seqNo' $ s^.seqMap
  let s' = Session (s^.broker) (s^.server) (s^.serverAddr) (s^.sessid) newSeqMap
  put s'
  return ()
 

