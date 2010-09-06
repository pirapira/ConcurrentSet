module ConcurrentListSet where

{-
  An implementation of
  S. Heller, M. Herlihy, V. Luchangco and M. Moir: A List-Based Set Algorithm
  http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.59.8911&rep=rep1&type=pdf
-}

import Control.Concurrent.MVar

data (Ord elt) => Lst elt = Cons elt (Set elt) | Nil deriving Eq
data (Ord elt) => Set elt = Set (MVar (Lst elt, Bool)) (MVar ()) deriving Eq
{-
  The Bool value in Set indicates whether the node has been removed or not. True means removed.
  The () value in Set is a write lock. Readers do not have to take this lock.
 -}


-- 

init :: (Ord elt) => IO (Set elt)
init = do 
  newref <- newMVar (Nil, False)
  newlck <- newMVar ()
  return $ Set newref newlck

locate :: (Ord elt) => Set elt -> elt -> IO (Set elt, Lst elt)
locate p@(Set current _) k = do
  (c, _) <- readMVar current 
  case c of
    Nil -> return (p, Nil)
    Cons cval rest ->
      if cval < k
        then locate rest k
        else return (p, c)

contains :: (Ord elt) => Set elt -> elt -> IO Bool
contains s k = do
  (_, l) <- locate s k
  case l of
    Cons hd _ -> return $ hd == k
    Nil       -> return False

remove :: (Ord elt) => Set elt -> elt -> IO Bool
remove s k = do
  (Set p lck, c) <- locate s k
  takeMVar lck
  (pn, pm) <- readMVar p
  if pn == c && not pm then
    case c of
      Cons ck (Set cmvar clck) | ck == k ->
           do
             takeMVar clck
             cn <- modifyMVar cmvar (\(cn, _) -> return ((cn, True), cn))
             putMVar clck ()
             swapMVar p (cn, pm)
             putMVar lck ()
             return True
      _ -> do
             putMVar lck ()
             return False
    else do 
      putMVar lck ()
      remove s k

add :: (Ord elt) => Set elt -> elt -> IO Bool
add s k = do
  (Set p lck, c) <- locate s k
  takeMVar lck
  (pn, pm) <- readMVar p
  if pn == c && not pm then
    case c of
      Cons ck _ | ck == k -> do
             putMVar lck ()
             return False
      _ -> do
             tmvar <- newMVar (c, False)
             tmlck <- newMVar ()
             swapMVar p (Cons k (Set tmvar tmlck), pm)
             putMVar lck ()
             return True
    else do
      putMVar lck ()
      add s k
