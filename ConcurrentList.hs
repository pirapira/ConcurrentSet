
{- 
Todo:
- generalize Integer to some totally ordered type
- introduce bottom via lifting
-}

module ConcurrentList where

import Control.Concurrent.MVar

data Lst elt = Cons elt (MVar (Lst elt, Bool)) | Nil deriving Eq
data (Ord elt) => Set elt = Set (MVar (Lst elt, Bool))

-- 

init :: (Ord elt) => IO (Set elt)
init = do 
  newref <- newMVar (Nil, False)
  return $ Set newref

locate :: (Ord elt) => Set elt -> elt -> IO (Set elt, Lst elt)
locate p@(Set current) k = do
    (current_content, _) <- readMVar current 
    case current_content of
      Nil -> return (p, Nil)
      Cons cval rest ->
          if cval < k then locate (Set rest) k
          else return (p, Cons cval rest)

contains :: (Ord elt) => Set elt -> elt -> IO Bool
contains s k = do
  (_, l) <- locate s k
  case l of
    Cons hd _ -> return $ hd == k
    Nil -> return False

remove :: (Ord elt) => Set elt -> elt -> IO Bool
remove s k = do
  (Set p, c) <- locate s k
  (pn, pm) <- takeMVar p
  if pn == c && not pm then
      case c of
        Cons ck cmvar | ck == k -> do
                          (cn, _) <- takeMVar cmvar
                          putMVar cmvar (cn, True)
                          putMVar p (cn, pm)
                          return True
        Cons _ _ -> do
               putMVar p (pn, pm)
               return False
        Nil -> do
               putMVar p (pn, pm)
               return False
    else
        do 
          putMVar p (pn, pm)
          remove s k

add :: (Ord elt) => Set elt -> elt -> IO Bool
add s k = do
  (Set p, c) <- locate s k
  (pn, pm) <- takeMVar p
  if pn == c && not pm then
      case c of
        Cons ck _ | ck /= k -> do
               tmvar <- newMVar (c, False)
               putMVar p (Cons k tmvar, pm)
               return True
        Cons _ _ -> do
          putMVar p (pn, pm)
          return False
        Nil -> do
               tmvar <- newMVar (c, False)
               putMVar p (Cons k tmvar, pm)
               return True
    else
        do 
          putMVar p (pn, pm)
          add s k

