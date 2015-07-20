module Main (main) where

import Control.Concurrent.Async
import Data.Traversable
import Data.IORef

data Account = Account Int deriving (Show, Eq)

main :: IO ()
main = do
  account <- newIORef newAccount

  let workers = replicate 20 (depositWorker account) ++
                replicate 20 (withdrawWorker account)

  tasks <- for workers async

  for tasks wait

  accountValue <- readIORef account

  putStr $ show accountValue
  return ()

depositWorker account = modifyIORef account (deposit 10)
withdrawWorker account = modifyIORef account (withdraw 6)

newAccount = Account 0

update f (Account n) = Account (f n)

deposit amount = update (+amount)

withdraw amount = update (flip (-) amount)