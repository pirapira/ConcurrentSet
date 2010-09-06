import ConcurrentList
import Control.Concurrent
import System.Random

mini:: Integer
maxi:: Integer
mini = 1
maxi = 9

random_read :: ConcurrentList.Set Integer -> MVar () -> IO ()
random_read lst mv = do
  p <- randomRIO (mini, maxi)
  b <- ConcurrentList.contains lst p
  putStrLn $ (if b then "o    " else "x    ") ++ show p
  random_read lst mv

randomAddrem :: ConcurrentList.Set Integer -> MVar () -> IO ()
randomAddrem lst mv = do
    p <- randomRIO (mini, maxi)
    ConcurrentList.add lst p
    putStrLn $ "  add" ++ show p
    q <- randomRIO (mini, maxi)
    ConcurrentList.remove lst q
    putStrLn $ "  rm " ++ show q
    randomAddrem lst mv
    
main :: IO ()
main = do
    lst <- ConcurrentList.init
    mv0 <- newEmptyMVar
    mv1 <- newEmptyMVar
    mv2 <- newEmptyMVar
    forkIO $ randomAddrem lst mv0
    forkIO $ randomAddrem lst mv2
    forkIO $ random_read lst mv1
    takeMVar mv0
    takeMVar mv1
    takeMVar mv2
    return ()
