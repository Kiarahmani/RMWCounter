module Shoppr.Consts (
  cCACHE_LWM,
  cDISK_LWM,
  cGC_WORKER_THREAD_DELAY,
  cCACHE_THREAD_DELAY,
  cCACHE_MAX_OBJS,
  cNUM_WORKERS,
  cLOCK_DELAY,
  cTABLE_NAME
) where

import Control.Concurrent.MVar

cCACHE_LWM :: Int
cCACHE_LWM = 128

cDISK_LWM :: Int
cDISK_LWM = 1024

cGC_WORKER_THREAD_DELAY :: Int
cGC_WORKER_THREAD_DELAY = 1000000

cCACHE_THREAD_DELAY :: Int
cCACHE_THREAD_DELAY = 1000000

cNUM_WORKERS :: Int
cNUM_WORKERS = 1

cLOCK_DELAY :: Int
cLOCK_DELAY = 200000

cCACHE_MAX_OBJS :: Int
cCACHE_MAX_OBJS = 1024

cTABLE_NAME :: String
cTABLE_NAME = "counter"

