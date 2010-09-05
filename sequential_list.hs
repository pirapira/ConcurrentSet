
{- 
Todo:
- generalize Integer to some totally ordered type
- introduce bottom via lifting
-}

module Sequential_List where

import Data.IORef

data Lst elt = Cons elt (IORef (Lst elt)) | Nil
data (Ord elt) => Set elt = Set (IORef (Lst elt))

init :: (Ord elt) => IO (Set elt)
init = do 
  newref <- newIORef Nil 
  return $ Set newref

locate :: (Ord elt) => Set elt -> elt -> IO (Set elt, Lst elt)
locate p@(Set current) k = do
    current_content <- readIORef current 
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

add :: (Ord elt) => Set elt -> elt -> IO Bool
add s k = do
  l <- locate s k
  case l of
    (_, Cons cval _) | cval == k -> return False
    (Set pn, c) ->
       do 
         trest <- newIORef c
         writeIORef pn (Cons k trest)
         return True

remove :: (Ord elt) => Set elt -> elt -> IO Bool
remove s k = do
  (Set pn, Cons cval cn) <- locate s k
  if cval == k
    then do
      cn_content <- readIORef cn
      writeIORef pn cn_content
      return True
    else return False

