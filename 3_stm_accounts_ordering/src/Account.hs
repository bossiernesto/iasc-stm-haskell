module Account (
  Account(..),
  newAccount,
  deposit, 
  withdraw, 
  withdrawAtMost,
  withdrawMaybe) where

data Account = Account Int deriving (Show, Eq)

newAccount = Account 0

update f (Account n) = Account (f n)

deposit amount = update (+amount)

withdraw amount = update (flip (-) amount)

withdrawAtMost amount = update (substractAtMost amount)

withdrawMaybe amount (Account n) | amount <= n = Just $ Account (n - amount)
                                 | otherwise = Nothing 

substractAtMost a n = max (n - a) 0

