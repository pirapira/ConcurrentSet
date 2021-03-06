import ConcurrentListSet
import Control.Concurrent
import System.Random

mini:: Integer
maxi:: Integer
mini = 1
maxi = 9

random_read :: LSet Integer -> MVar () -> MVar () -> IO ()
random_read lst mv mvio = do
  p <- randomRIO (mini, maxi)
  takeMVar mvio
  b <- contains lst p
  putStrLn $ (if b then "o    " else "x    ") ++ show p
  putMVar mvio ()
  random_read lst mv mvio

randomAddrem :: LSet Integer -> MVar () -> MVar () -> IO ()
randomAddrem lst mv mvio = do
  p <- randomRIO (mini, maxi)
  takeMVar mvio
  insert lst p
  putStrLn $ "  add" ++ show p
  putMVar mvio ()
  q <- randomRIO (mini, maxi)
  takeMVar mvio
  delete lst q
  putStrLn $ "  rm " ++ show q
  putMVar mvio ()
  randomAddrem lst mv mvio
    
main :: IO ()
main = do
  lst <- makeLSet
  mv0 <- newEmptyMVar
  mv1 <- newEmptyMVar
  mv2 <- newEmptyMVar
  mvio <- newMVar ()
  forkIO $ randomAddrem lst mv0 mvio
  forkIO $ randomAddrem lst mv2 mvio
  random_read lst mv1 mvio
