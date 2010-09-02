
{- 
Todo:
- generalize Integer to some totally ordered type
- introduce bottom via lifting
-}

import Data.IORef

lst_min :: Integer 
lst_min = 0

data Lst = Cons Integer (IORef Lst) | Nil
data Set = SCons Integer (IORef Lst)

init :: IO Set
init = do 
  newref <- newIORef Nil 
  return $ SCons lst_min newref

locate :: Set -> Integer -> IO (Lst, Lst)
locate (SCons pval current) k = do
    current_content <- readIORef current 
    case current_content of
      Nil -> return (Cons pval current, Nil)
      Cons cval rest ->
          if cval < k then locate (SCons cval rest) k
          else return (Cons pval current, Cons cval rest)

contains :: Set -> Integer -> IO Bool
contains s k = do
    (_, (Cons hd _)) <- locate s k
    return $ hd == k

add :: Set -> Integer -> IO Bool
add s k = do
  ((Cons _ pn), c@(Cons cval _)) <- locate s k
  case cval of
    k ->
      return False
    _ -> do 
      trest <- newIORef c
      writeIORef pn (Cons k trest)
      return True

remove :: Set -> Integer -> IO Bool
remove s k = do
  ((Cons _ pn), (Cons cval cn)) <- locate s k
  case cval of
    k -> do
      cn_content <- readIORef cn
      writeIORef pn cn_content
      return True
    _ -> return False
      
        