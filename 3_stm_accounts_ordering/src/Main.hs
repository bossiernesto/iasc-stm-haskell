module Main (main) where

import Combinators
import Account

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.Ensure
import Control.Monad
import Data.Traversable

main :: IO ()
main = do
  account <- newTVarIO newAccount

  runAndWaitAll.transactionToWorker.transactions 20 $ account

  accountValue <- readTVarIO account

  putStrLn.show $ accountValue
  return ()

{- Transactions -}

transactions :: Int -> TVar Account -> [STM ()]
transactions n account = take (2 * n) $ cycle [depositTransaction account, withdrawTransaction account]

depositTransaction :: TVar Account -> STM ()
depositTransaction account = modifyTVar account (deposit 10)

withdrawTransaction :: TVar Account -> STM ()
withdrawTransaction account = modifyTVar account (withdrawAtMost 10)
--withdrawTransaction account = ensureSTM (modifyTVarOrAbort account (withdrawMaybe 10))