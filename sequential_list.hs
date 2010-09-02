
{- 
Todo:
- generalize Integer to some totally ordered type
- introduce bottom via lifting
-}

module Sequential_List where

import Data.IORef

lst_min :: Integer 
lst_min = 0

data Lst = Cons Integer (IORef Lst) | Nil
data Set = Set (IORef Lst)

init :: IO Set
init = do 
  newref <- newIORef Nil 
  return $ Set newref

locate :: Set -> Integer -> IO (Set, Lst)
locate p@(Set current) k = do
    current_content <- readIORef current 
    case current_content of
      Nil -> return (p, Nil)
      Cons cval rest ->
          if cval < k then locate (Set rest) k
          else return (p, Cons cval rest)

contains :: Set -> Integer -> IO Bool
contains s k = do
    (_, (Cons hd _)) <- locate s k
    return $ hd == k

add :: Set -> Integer -> IO Bool
add s k = do
  ((Set pn), c@(Cons cval _)) <- locate s k
  case cval of
    _ | cval == k ->
      return False
    _ -> do 
      trest <- newIORef c
      writeIORef pn (Cons k trest)
      return True

remove :: Set -> Integer -> IO Bool
remove s k = do
  ((Set pn), (Cons cval cn)) <- locate s k
  case cval of
    _ | cval == k -> do
      cn_content <- readIORef cn
      writeIORef pn cn_content
      return True
    _ -> return False
