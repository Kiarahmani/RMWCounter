{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Shoppr.Session (
	CSN,
  runSession,
  readKey,
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
import Control.Concurrent.MVar

import Shoppr.Types
import Shoppr.NameService.Types
import Shoppr.Consts (cTABLE_NAME)
import Shoppr.Marshall (decodeResponse)

data Session = Session {
  _broker     :: Frontend,
  _server     :: ZMQ4.Socket ZMQ4.Req,
  _serverAddr :: String,
  _sessid     :: SessID,
  -- _readObjs   :: S.Set Key,
  _seqMap     :: M.Map Key SeqNo
}

makeLenses ''Session

type CSN a = StateT Session{-metadata-} IO{-client computations-} a{-type of the result -}

beginSession :: NameService -> IO Session
beginSession ns = do
  (serverAddr, sock) <- getClientJoin ns
  sessid <- SessID <$> randomIO
  let req = encode $ Request cTABLE_NAME AddSessID sessid 0
  liftIO $ ZMQ4.send sock [] req
  responseBlob <- liftIO $ ZMQ4.receive sock
  threadDelay 10000000
  return $ Session (getFrontend ns) sock serverAddr sessid M.empty

endSession :: Session -> IO ()
endSession s = do
  threadDelay 1000000 
  let req = encode $ Request cTABLE_NAME DropSessID (s^.sessid) 0
  liftIO $ ZMQ4.send (s^.server) [] req
  responseBlob <- liftIO $ ZMQ4.receive (s^.server)
  ZMQ4.disconnect (s ^. server) (s^.serverAddr)

runSession :: Show a =>  NameService -> CSN a -> IO a
runSession  ns comp = do
  session <- beginSession  ns 
  res <- evalStateT comp session
  endSession session
  return res

readKey :: Key -> CSN Int
readKey key = do
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
  if True --seqNo' == seqNo - 1
  then return val
  else do
    liftIO $ threadDelay 100000
    liftIO $ putStrLn "Retrying read..."
    readKey key

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
 
newKey :: IO Key
newKey = Key . encodeUUID <$> randomIO
  where
    encodeUUID (uuid :: UUID) = encode uuid

