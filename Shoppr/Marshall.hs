{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, TemplateHaskell #-}

module Shoppr.Marshall (
  decodeRequest,
  decodeResponse
) where

import Shoppr.Types
import Database.Cassandra.CQL
import Data.Serialize as S
import Control.Applicative ((<$>))
import Data.ByteString (ByteString, length, head, tail)
import Data.Either (rights)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.ByteString.Char8 (pack, unpack)
import Data.UUID
import Data.Maybe (fromJust)
import Data.Word
import Data.DeriveTH
import Data.Time

$(derive makeSerialize ''Key)

$(derive makeSerialize ''SessID)

instance Serialize UUID where
  put = putLazyByteString . toByteString
  get = do
    r <- fromByteString <$> getLazyByteString 16
    case r of
      Nothing -> error "serialize UUID"
      Just x -> return x

instance CasType Key where
  putCas = put
  getCas = get
  casType _ = CBlob

$(derive makeSerialize ''Oper)
$(derive makeSerialize ''Request)
$(derive makeSerialize ''Response)

decodeRequest :: ByteString -> Request
decodeRequest b =
  case decode b of
    Left s -> error $ "decodeRequest : " ++ s
    Right v -> v

decodeResponse :: ByteString -> Response
decodeResponse b = case decode b of
                     Left s -> error $ "decodeResponse : " ++ s
                     Right v -> v



