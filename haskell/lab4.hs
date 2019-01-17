module LAB4 (mySplitAt, mySize, myMember, myFromList, myIntersperse, myIntersection) where
import System.IO
import Data.Map

mySplitAt 0 xs     = ([], xs)
mySplitAt n []     = ([], [])
mySplitAt n (x:xs) =  (x:ys, zs)
	where (ys, zs) = mySplitAt (n - 1) xs


mySize :: Data.Map.Map k a -> Int
mySize arg = length (Data.Map.keys arg)

myMember :: (Eq a) => a -> [a] -> Bool
myMember x [] = False
myMember x (y:ys) = if (y == x) then True else myMember x ys

myFromList :: (Eq a) => [a] -> [a]
myFromList [] = []
myFromList (x:xs) = if myMember x (myFromList xs) then myFromList xs else x:(myFromList xs)


myIntersperse x (y:ys) = y:concat[[x,z] | z <- ys]

myIntersection :: (Eq a) => [a] -> [a] -> [a]
myIntersection x [] = x
myIntersection [] y = y
myIntersection (x:xs) y = myFromList (if myMember x y then x:(myIntersection xs y) else (myIntersection xs y))