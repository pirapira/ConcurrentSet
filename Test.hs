import ConcurrentList
import Control.Concurrent
import System.Random

random_read :: ConcurrentList.Set Bool -> MVar () -> Integer -> MVar () -> IO ()
random_read _ mv 0 _ = do
  putMVar mv ()
  return ()
random_read lst mv n mvio = do
  p <- randomIO
  takeMVar mvio
  b <- ConcurrentList.contains lst p
  putStrLn $ (if b then "o    " else "x    ") ++ show p
  putMVar mvio ()
  random_read lst mv (n - 1) mvio

randomAddrem :: ConcurrentList.Set Bool -> MVar () -> Integer -> MVar () -> IO ()
randomAddrem _ mv 0 _ = do
    putMVar mv ()
    return ()
randomAddrem lst mv n mvio = do
    p <- randomIO
    takeMVar mvio
    ConcurrentList.add lst p
    putStrLn $ ":add " ++ show p
    putMVar mvio ()
    q <- randomIO
    takeMVar mvio
    ConcurrentList.remove lst q
    putStrLn $ ":rm  " ++ show q
    putMVar mvio ()
    randomAddrem lst mv (n - 1) mvio
    
main :: IO ()
main = do
    lst <- ConcurrentList.init
    mv0 <- newEmptyMVar
    mv1 <- newEmptyMVar
    mvio <- newMVar ()
    forkIO $ randomAddrem lst mv0 10000 mvio
    forkIO $ random_read lst mv1 10000 mvio
    takeMVar mv0
    takeMVar mv1
    return ()
