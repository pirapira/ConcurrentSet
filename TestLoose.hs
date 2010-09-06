import ConcurrentListSet
import Control.Concurrent
import System.Random

mini:: Integer
maxi:: Integer
mini = 1000
maxi = 9999

random_read :: Set Integer -> MVar ()-> IO ()
random_read lst mv = do
  p <- randomRIO (mini, maxi)
  b <- contains lst p
  putStrLn $ (if b then "o    " else "x    ") ++ show p
  random_read lst mv

randomAddrem :: Set Integer -> MVar () -> IO ()
randomAddrem lst mv = do
  p <- randomRIO (mini, maxi)
  add lst p
  putStrLn $ "  add" ++ show p
  q <- randomRIO (mini, maxi)
  remove lst q
  putStrLn $ "  rm " ++ show q
  randomAddrem lst mv
    
main :: IO ()
main = do
  lst <- ConcurrentListSet.init
  mv0 <- newEmptyMVar
  mv1 <- newEmptyMVar
  mv2 <- newEmptyMVar
  mv3 <- newEmptyMVar
  mv4 <- newEmptyMVar
--  forkIO $ randomAddrem lst mv0
--  forkIO $ randomAddrem lst mv2
  forkIO $ random_read lst mv1
--  forkIO $ random_read lst mv3
--  forkIO $ random_read lst mv4
  takeMVar mv0
  takeMVar mv1
  takeMVar mv2
  return ()
