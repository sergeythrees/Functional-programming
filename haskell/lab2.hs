import System.IO

doMyList :: Int -> [Int]
doMyList(x) = [x, x + 1 .. x + x]

oddEven :: [a] -> [a]
oddEven [] = []
oddEven [x] = [x]
oddEven xs = (reverse (take 2 xs)) ++ oddEven (drop 2 xs)

insert1 :: [a] -> a -> Int -> [a]
insert1 list atom pos = (take pos list) ++ [atom] ++ (drop pos list)

listSumm :: [Int] -> [Int] -> [Int]
listSumm [x] [] = [x]
listSumm [] [x] = [x]
listSumm (x:xs) (y:ys) = sum(x:xs) : sum(y:ys) : []

position :: Eq a => [a] -> a -> Int
position (x:xs) atom 
    | (x /= atom) = 1 + position xs atom
    | otherwise = 0

getSumm :: Int -> Int
getSumm n = sum [1..n]

getSummCust :: Int -> Int
getSummCust n = getSumm n - n

main = do
  putStrLn("#0 - "++show(doMyList(7)))
  putStrLn("#1 - "++show(oddEven ['n', 'A', 'a', 'n']))
  putStrLn("#2 - "++show(insert1 ['A','n','a','s','a','s','i','a'] 't' 4))
  putStrLn("#3 - "++show(listSumm [1,2,3] [4,5,6,7]))
  putStrLn("#4 - "++show(position ['A','n','a','s','a','s','i','a'] 's'))
  putStrLn("#5 - "++show(getSumm 4))
  putStrLn("#6 - "++show(getSummCust 5))