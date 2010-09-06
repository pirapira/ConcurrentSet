module ConcurrentListSet where

{-
  An implementation of
  S. Heller, M. Herlihy, V. Luchangco and M. Moir: A List-Based Set Algorithm
  http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.59.8911&rep=rep1&type=pdf
-}

import Control.Concurrent.MVar

data (Ord elt) => Lst elt = Cons elt (Set elt) | Nil deriving Eq
data (Ord elt) => Set elt = Set (MVar (Lst elt, Bool)) deriving Eq
{- The Bool value in Set indicates whether the node has been removed or not
   True: removed
 -}


-- 

init :: (Ord elt) => IO (Set elt)
init = do 
  newref <- newMVar (Nil, False)
  return $ Set newref

locate :: (Ord elt) => Set elt -> elt -> IO (Set elt, Lst elt)
locate p@(Set current) k = do
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
  (Set p, c) <- locate s k
  (pn, pm) <- takeMVar p
  if pn == c && not pm then
    case c of
      Cons ck (Set cmvar) | ck == k ->
           do
             (cn, _) <- takeMVar cmvar
             putMVar cmvar (cn, True)
             putMVar p (cn, pm)
             return True
      _ -> do
             putMVar p (pn, pm)
             return False
    else do 
      putMVar p (pn, pm)
      remove s k

add :: (Ord elt) => Set elt -> elt -> IO Bool
add s k = do
  (Set p, c) <- locate s k
  (pn, pm) <- takeMVar p
  if pn == c && not pm then
    case c of
      Cons ck _ | ck == k -> do
             putMVar p (pn, pm)
             return False
      _ -> do
             tmvar <- newMVar (c, False)
             putMVar p (Cons k (Set tmvar), pm)
             return True
    else do
      putMVar p (pn, pm)
      add s k
