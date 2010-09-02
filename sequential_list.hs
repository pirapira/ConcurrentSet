
import Data.IORef

lst_min :: Integer 
lst_min = 0

data Lst = Cons Integer (IORef Lst) | Nil
data Set = SCons Integer (IORef Lst)

init :: IO Set
init = do 
  newref <- newIORef Nil 
  return $ SCons lst_min newref

{-         
insert :: Set -> Integer -> Set
insert Nil a = Cons a Nil
insert (Cons hd tl) b | b < hd = Cons b $Cons hd tl
insert (Cons hd tl) b | b == hd = Cons hd tl
insert (Cons hd tl) b | b > hd = Cons hd $ insert tl b
-}

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


