{-# LANGUAGE ScopedTypeVariables, EmptyDataDecls,
    TemplateHaskell, DataKinds, OverloadedStrings,
    DoAndIfThenElse#-}

module Shoppr.Server (
  runServer
) where

import Data.Serialize
import Control.Monad (forever, replicateM, when)
import Data.ByteString hiding (map, pack, putStrLn)
import System.ZMQ4.Monadic
import qualified System.ZMQ4 as ZMQ4
import Control.Lens
import Control.Concurrent (forkIO, myThreadId, threadDelay)
import System.Posix.Process (getProcessID)
import Database.Cassandra.CQL as Cassandra

import Shoppr.Consts
import Shoppr.Types
import Shoppr.NameService.Types
import Shoppr.Marshall
import Shoppr.DBDriver

makeLenses ''Request

#define DEBUG

debugPrint :: String -> IO ()
#ifdef DEBUG
debugPrint s = do
  tid <- myThreadId
  putStrLn $ "[" ++ (show tid) ++ "] " ++ s
#else
debugPrint _ = return ()
#endif

runServer :: [Cassandra.Server] -> Keyspace -> NameService -> IO ()
runServer serverList keyspace ns = do
  {- Connection to the Cassandra deployment -}
  pool <- newPool serverList keyspace Nothing
  replicateM cNUM_WORKERS (forkIO $ worker pool)
  getServerJoin ns


worker :: Pool -> IO ()
worker pool = do
  ctxt <- ZMQ4.context
  sock <- ZMQ4.socket ctxt ZMQ4.Rep
  pid <- getProcessID
  -- debugPrint "worker: connecting..."
  ZMQ4.connect sock $ "ipc:///tmp/shoppr/" ++ show pid
  -- debugPrint "worker: connected"
  {- loop forver servicing clients -}
  forever $ do
    binReq <- ZMQ4.receive sock
    let req = decodeRequest binReq
    let tname = req^.objNameReq
    let oper = req^.opReq
    let sid = req^.sidReq
    let sqn = req^.sqnReq
    print "Operation.."
    result <- case oper of 
      Rd key -> do
        rows <- runCas pool (cqlRead tname sid ONE key)
        let ret = case rows of
                    [] -> Nothing
                    [row] -> Just row
                    otherwise -> error "Many rows!!"
        return $ Response sqn ret
      Wr key val -> do
        runCas pool (cqlInsertInSSN tname sid ONE key (val,sqn))
        return $ Response (sqn+1) Nothing
      AddSessID -> do
        runCas pool (addSessID tname sid)
        return $ Response sqn Nothing
      DropSessID -> do
        runCas pool (dropSessID tname sid)
        return $ Response sqn Nothing
    ZMQ4.send sock [] $ encode result

