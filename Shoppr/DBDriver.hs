{-# LANGUAGE ScopedTypeVariables, EmptyDataDecls, TemplateHaskell,
    DataKinds, OverloadedStrings, DoAndIfThenElse  #-}

module Shoppr.DBDriver (
  TableName(..),
  ReadRow,

  createTable,
  dropTable,

  addSessID,
  dropSessID,

  cqlRead,
  cqlInsert,
  cqlInsertInSSN,
  cqlDelete,

) where


import Shoppr.Consts
import Control.Concurrent (threadDelay)
import Shoppr.Types
import Shoppr.Marshall
import Data.Serialize
import Control.Applicative ((<$>))
import Control.Monad (forever)
import Data.ByteString hiding (map, pack, putStrLn)
import Data.Either (rights)
import Data.Map (Map)
import Data.Time
import qualified Data.Map as Map
import System.ZMQ4
import Control.Lens
import Database.Cassandra.CQL
import Data.UUID
import Data.Int (Int64)
import qualified Data.Set as S
import Data.Text hiding (map)
import Control.Monad.Trans (liftIO)
import Data.Maybe (fromJust)
import Control.Monad (when)
import System.Random (randomRIO)
-- Simplay an alias for Types.ObjType
type TableName = String

type ReadRow = (Int {- val -}, SeqNo {- sqn -})



--------------------------------------------------------------------------------
-- Cassandra Link Layer
--------------------------------------------------------------------------------

mkCreateTable :: TableName -> Query Schema () ()
mkCreateTable tname = query $ pack $ "create table " ++ tname ++ " (objid blob, val int, primary key (objid))"

mkDropTable :: TableName -> Query Schema () ()
mkDropTable tname = query $ pack $ "drop table " ++ tname

mkAddSessID :: TableName -> SessID -> Query Schema () ()
mkAddSessID tname sid = query $ pack $ "alter table "++tname++" add "++(show sid)++" bigint"

mkDropSessID :: TableName -> SessID -> Query Schema () ()
mkDropSessID tname sid = query $ pack $ "alter table "++tname++" drop "++(show sid)

mkInsert :: TableName ->  Query Write (Key, Int) ()
mkInsert tname = query $ pack $ "insert into " ++ tname ++ " (objid, val) values (?, ?)"

mkInsertInSSN :: TableName -> SessID -> Query Write (Key, Int, SeqNo) ()
mkInsertInSSN tname sid = query $ pack $ "insert into " ++ tname ++ " (objid, val, "++(show sid)++") values (?, ?, ?) "


mkInsertToken :: TableName -> SessID -> Query Write (Key, Int, SeqNo) ()
mkInsertToken tname sid = query $ pack $ "insert into " ++ tname ++ " (objid, val, "++(show sid)++") values (?, ?, ?) if not exists" 

mkDelete :: TableName -> Query Write (Key) ()
mkDelete tname = query $ pack $ "delete from " ++ tname ++ " where objid = ?"

mkRead :: TableName -> SessID -> Query Rows (Key) ReadRow
mkRead tname sid = query $ pack $ "select val, "++(show sid)++" from " ++ tname ++ " where objid = ?"



-------------------------------------------------------------------------------
cqlRead :: TableName -> SessID -> Consistency -> Key -> Cas [ReadRow]
cqlRead tname sid c k = do
  rows <- executeRows c (mkRead tname sid) k
  return rows

cqlInsert :: TableName -> Consistency -> Key -> Int -> Cas ()
cqlInsert tname c k val = do
    executeWrite c (mkInsert tname) (k,val)

cqlInsertInSSN :: TableName -> SessID -> Consistency -> Key -> ReadRow -> Cas ()
cqlInsertInSSN tname sid c k (val,sqn) = do
  if sqn == 0
  then error "cqlInsertInSSN : sqn is 0"
  else do
    executeWrite c (mkInsertInSSN tname sid) (k,val,sqn)

cqlDelete :: TableName -> Key -> Cas ()
cqlDelete tname k =
  executeWrite ONE (mkDelete tname) (k)

createTable :: TableName -> Cas ()
createTable tname = do
  liftIO $ putStrLn $ "Creating "++(tname)
  liftIO . print =<< executeSchema ALL (mkCreateTable tname) ()

dropTable :: TableName -> Cas ()
dropTable tname = do
  liftIO $ putStrLn $ "Dropping "++(tname)
  liftIO . print =<< executeSchema ALL (mkDropTable tname) ()



addSessID :: TableName -> SessID -> Bool -> Cas ()
addSessID tname sid firstCall = do
 -- when (firstCall) $ do 
 --     
 	wait <- liftIO $ randomRIO (1,20) 
 	liftIO $ threadDelay $ 1000000 + wait*100000
  	liftIO . print =<< executeSchema ONE (mkAddSessID tname sid) ()
 -- let k = Key $  encode (0 :: Integer)
 -- let val = 0 
 -- let sqn = 0
 -- res <-  executeTrans  (mkInsertToken tname sid) (k,val,sqn) ALL
 -- if res 
 -- then return ()
 -- else do
   --     liftIO $ print "fuck"
  --	liftIO $ threadDelay 1000
--	addSessID tname sid False



dropSessID :: TableName -> SessID -> Cas ()
dropSessID tname sid = do
  liftIO $ putStrLn $ "Dropping Session "++(show sid)
  liftIO . print =<< executeSchema ONE (mkDropSessID tname sid) ()


----------------------------------------------------------------------------------







