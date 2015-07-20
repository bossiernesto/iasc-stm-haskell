module Main (main) where

import Control.Concurrent.Async

data Account = Account Int deriving (Show, Eq)

main :: IO ()
main = do
  let account = newAccount
  task1 <- async (return $ deposit 10 account)
  task2 <- async (return $ withdraw 6 account)
  wait task1
  wait task2
  putStr $ show account
  return ()

newAccount = Account 0

update f (Account n) = Account (f n)

deposit amount = update (+amount)

withdraw amount = update (flip (-) amount)