module Main (main) where

import Control.Concurrent.Async
import Control.Monad
import Data.IORef
import Data.Traversable

data Account = Account Int deriving (Show, Eq)

main :: IO ()
main = do
  account <- newIORef newAccount

  runAndWaitAll.workers 20 $ account

  accountValue <- readIORef account

  putStrLn.show $ accountValue
  return ()

{- Concurrency Combinators -}

runAndWaitAll = runAll >=> waitAll

runAll  workers = for workers async

waitAll tasks   = for tasks wait 

{- Workers -}

workers :: Int -> IORef Account -> [IO ()]
workers n account = replicate n (depositWorker account) ++ 
                    replicate n (withdrawWorker account)

depositWorker :: IORef Account -> IO ()
depositWorker account = modifyIORef account (deposit 10)

withdrawWorker :: IORef Account -> IO ()
withdrawWorker account = modifyIORef account (withdraw 6)

{- Account Functions -}

newAccount = Account 0

update f (Account n) = Account (f n)

deposit amount = update (+amount)

withdraw amount = update (flip (-) amount)