{-# LANGUAGE DeriveDataTypeable #-}

module Control.Concurrent.STM.Ensure (
  modifyTVarOrAbort, 
  ensureSTM) where 

import Control.Concurrent.STM
import Data.Maybe
import Control.Exception
import Data.Typeable

data TransactionAbortion = TransactionAbortion deriving (Show, Typeable)

instance Exception TransactionAbortion

modifyTVarOrAbort :: TVar a -> (a -> Maybe a) -> STM ()
modifyTVarOrAbort var pf = do 
    value <- readTVar var
    case (pf value) of
      (Just newValue) -> writeTVar var newValue
      _               -> throwSTM TransactionAbortion


ensureSTM :: STM a -> STM a
ensureSTM tx = catchSTM tx (\TransactionAbortion -> retry)

