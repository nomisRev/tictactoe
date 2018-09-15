module Predef where

--All this logic should be writable in a more generic way, so should be able to find this somewhere... Up your Hoogle skills.
-- Control.Lens.Indexed would be nice?

set :: (Num i, Eq i) => i -> a -> [a] -> [a]
set i a = update i (const a)

update :: (Num i, Eq i) => i -> (a -> a) -> [a] -> [a]
update i f xs = update' xs 0
  where update' [] ii     = []
        update' (a:as) ii = if i == ii
                            then f a : as
                            else a : update' as (ii+1)

elemAt :: (Int, Int) -> [[a]] -> Maybe a
elemAt (x, y) aas = at x aas >>= \xs -> at y xs 
  where at _ []     = Nothing
        at 0 (x:_)  = Just x
        at i (_:xs) = at (i-1) xs
