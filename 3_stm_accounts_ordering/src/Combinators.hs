module Combinators (
  transactionToWorker,
  runAndWaitAll, 
  runAll, 
  waitAll) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.Traversable

type Transaction a = STM a
type Worker a = IO a
type Task a = Async a

transactionToWorker :: [Transaction a] -> [Worker a]
transactionToWorker = map atomically

runAndWaitAll :: [Worker a] -> IO [a]
runAndWaitAll = runAll >=> waitAll
 
runAll :: [Worker a] -> IO [Task a]
runAll  workers = for workers async

waitAll :: [Task a] -> IO [a]
waitAll tasks   = for tasks wait 

