module Examples.RawAsync (exec) where

import Control.Concurrent qualified as Concurrent

childProcess :: (Num a) => Concurrent.MVar a -> IO Concurrent.ThreadId
childProcess flag = Concurrent.forkIO $ do
  let list = ("child",) <$> [1 .. 5]
  let f s = print @(String, Int) s *> Concurrent.threadDelay 5
  mapM_ f list
  Concurrent.takeMVar flag >>= \b -> Concurrent.putMVar flag (b + 1)

wait :: Concurrent.MVar Int -> IO ()
wait flag = do
  f <- Concurrent.readMVar flag

  if f >= 1
    then return ()
    else Concurrent.threadDelay 5 *> wait flag

printDelay :: (Show a) => a -> IO ()
printDelay n = print n *> threadDelaySec 1

threadDelaySec :: Int -> IO ()
threadDelaySec = Concurrent.threadDelay . (* 1000000)

exec :: IO ()
exec = do
  flag <- Concurrent.newMVar 0
  threadId <- childProcess flag
  let list = (,"parent") <$> [1 .. 5]
  mapM_ (\x -> print @(Int, String) x *> Concurrent.threadDelay 2) list

  wait flag
  Concurrent.killThread threadId
