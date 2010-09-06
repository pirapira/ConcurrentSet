module ConcurrentListSet
    (
      LSet
    , makeLSet
    , contains
    , insert
    , delete
    )  where

{-
  An implementation of
  S. Heller, M. Herlihy, V. Luchangco and M. Moir: A List-Based LSet Algorithm
  http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.59.8911&rep=rep1&type=pdf
-}

import Control.Concurrent.MVar

data (Ord elt) => Lst elt = Cons elt (LSet elt) | Nil deriving Eq
data (Ord elt) => LSet elt = LSet (MVar (Lst elt, Bool)) (MVar ()) deriving Eq
{-
  The Bool value in LSet indicates whether the node has been removed or not. True means removed.
  The () value in LSet is a write lock. Readers do not have to take this lock.
 -}


-- 

makeLSet :: (Ord elt) => IO (LSet elt)
makeLSet = do 
  newref <- newMVar (Nil, False)
  newlck <- newMVar ()
  return $ LSet newref newlck

locate :: (Ord elt) => LSet elt -> elt -> IO (LSet elt, Lst elt)
locate p@(LSet current _) k = do
  (c, _) <- readMVar current 
  case c of
    Nil -> return (p, Nil)
    Cons cval rest ->
      if cval < k
        then locate rest k
        else return (p, c)

contains :: (Ord elt) => LSet elt -> elt -> IO Bool
contains s k = do
  (_, l) <- locate s k
  case l of
    Cons hd _ -> return $ hd == k
    Nil       -> return False

delete :: (Ord elt) => LSet elt -> elt -> IO Bool
delete s k = do
  (LSet p lck, c) <- locate s k
  takeMVar lck
  (pn, pm) <- readMVar p
  if pn == c && not pm then
    case c of
      Cons ck (LSet cmvar clck) | ck == k ->
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
      delete s k

insert :: (Ord elt) => LSet elt -> elt -> IO Bool
insert s k = do
  (LSet p lck, c) <- locate s k
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
             swapMVar p (Cons k (LSet tmvar tmlck), pm)
             putMVar lck ()
             return True
    else do
      putMVar lck ()
      insert s k
