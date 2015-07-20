module Main (main) where

import Control.Concurrent.Async
import Data.Traversable

data Account = Account Int deriving (Show, Eq)

main :: IO ()
main = do
  let account = newAccount

  let workers = replicate 20 (depositWorker account) ++
                replicate 20 (withdrawWorker account)

  tasks <- for workers async

  for tasks wait

  putStr $ show account
  return ()

depositWorker account = return $ deposit 10 account
withdrawWorker account = return $ withdraw 6 account

newAccount = Account 0

update f (Account n) = Account (f n)

deposit amount = update (+amount)

withdraw amount = update (flip (-) amount)