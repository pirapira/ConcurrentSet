
lst_min :: Integer 
lst_min = 0

data Lst = Cons Integer Lst | Nil
data Set = SCons Integer Lst

init :: Set
init = SCons lst_min Nil

{-         
insert :: Set -> Integer -> Set
insert Nil a = Cons a Nil
insert (Cons hd tl) b | b < hd = Cons b $Cons hd tl
insert (Cons hd tl) b | b == hd = Cons hd tl
insert (Cons hd tl) b | b > hd = Cons hd $ insert tl b
-}

locate :: Set -> Integer -> (Lst , Lst)
locate (SCons hd (Cons next rest)) k | k < next = locate (SCons next rest) k
locate (SCons hd tail) _ = (Cons hd tail, tail)

contains :: Set -> Integer -> Bool
contains s k = hd == k
    where (_, (Cons hd _)) = locate s k
