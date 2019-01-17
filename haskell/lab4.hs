module LW4 (mySplitAt, mySize, myIntersperse, myIntersection) where

mySplitAt 0 xs     = ([], xs)
mySplitAt n []     = ([], [])
mySplitAt n (x:xs) =  (x:ys, zs)
	where (ys, zs) = mySplitAt (n - 1) xs


mySize :: M.Map k a -> Int
mySize arg = length (M.keys arg)


myIntersperse x (y:ys) = y:concat[[x,z] | z <- ys]

myIntersection :: (Eq a) => [a] -> [a] -> [a]
myIntersection x [] = x
myIntersection [] y = y
myIntersection (x:xs) y = myFromList (if myMember x y then x:(myIntersection xs y) else (myIntersection xs y))