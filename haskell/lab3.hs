listnums :: Int -> [Int]
listnums n
  | n < 1 = []
  | otherwise =  n : listnums (n-1)

secondlastlist :: [[a]] -> [a]
secondlastlist [] = []
secondlastlist (x:xs) = (last x) : (secondlastlist xs)

myunion  [] ys = ys
myunion  (x:xs) ys
    | elem x ys = myunion xs ys
    | otherwise = x : (myunion xs ys)

getNElementsFromList :: [[a]] -> Int -> [a]
getNElementsFromList list pos = map (!!pos) list