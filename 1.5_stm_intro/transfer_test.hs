--para usar el ejemplo
-- :load timesoflore
-- atomically transferTest

transferTest = do
  alice <- newTVar (12 :: Gold)
  bob   <- newTVar 4
  transfer 3 alice bob
  liftM2 (,) (readTVar alice) (readTVar bob)
