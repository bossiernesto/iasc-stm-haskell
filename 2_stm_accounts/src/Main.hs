module Main (main) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.Traversable

data Account = Account Int deriving (Show, Eq)

main :: IO ()
main = do
  account <- newTVarIO newAccount

  runAndWaitAll.transactionToWorker.transactions 20 $ account

  accountValue <- readTVarIO account

  putStrLn.show $ accountValue
  return ()

{- Concurrency Combinators -}

transactionToWorker = map atomically

runAndWaitAll = runAll >=> waitAll

runAll  workers = for workers async

waitAll tasks   = for tasks wait 

{- Transactions -}

transactions :: Int -> TVar Account -> [STM ()]
transactions n account = replicate n (depositTransaction account) ++ 
                         replicate n (withdrawTransaction account)

depositTransaction :: TVar Account -> STM ()
depositTransaction account = modifyTVar account (deposit 10)

withdrawTransaction :: TVar Account -> STM ()
withdrawTransaction account = modifyTVar account (withdraw 6)

{- Account Functions -}

newAccount = Account 0

update f (Account n) = Account (f n)

deposit amount = update (+amount)

withdraw amount = update (flip (-) amount)