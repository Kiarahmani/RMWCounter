{-# Language TemplateHaskell, EmptyDataDecls, ScopedTypeVariables,
    TypeSynonymInstances, FlexibleInstances #-}

module Shoppr.Types (
  Key(..),
  SessID(..),
  SeqNo,
  knownUUID,
  Item,
  Cart,
  Oper(..),
  Request(..),
  Response(..)
) where

import Database.Cassandra.CQL
import Data.Serialize as S
import Control.Applicative ((<$>))
import Data.ByteString (ByteString, head)
import Data.Either (rights)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString.Char8 (pack, unpack)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.UUID hiding (show)
import Data.Int (Int64)
import Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Tuple.Select (sel1)
import Data.Time

type Item = String

data Cart = Cart {items :: [Item], orderPrice :: Int} deriving Show

newtype Key = Key { unKey :: ByteString } deriving (Eq, Ord)

instance Show Key where
  show (Key kv) = "Key " ++ (show $ Data.ByteString.head kv)


type SeqNo = Int64

newtype SessID = SessID { unSessID :: UUID } deriving (Eq, Ord)

instance Show SessID where
  show (SessID uuid) = "SessID " ++ (show . sel1 . toWords $ uuid)

knownUUID :: UUID
knownUUID = fromJust $ fromString $ "123e4567-e89b-12d3-a456-426655440000"

data Oper = Rd Key
          | Wr Key Int
          | AddSessID
          | DropSessID

data Request = Request {
  _objNameReq :: String,
  _opReq :: Oper,
  _sidReq :: SessID,
  _sqnReq :: SeqNo
}

data Response = Response {
  seqno :: SeqNo,
  result :: Maybe (Int,SeqNo)
}
