module CW1 (
  custSumm,
  pifagorianThree,
  transpose
) where

import qualified  Data.List as L


--1
custSumm :: Integral a => a -> a
custSumm n = sum (filter (\x -> (x `mod` 3 == 0) || (x `mod` 5 == 0)) [1..(n - 1)])

--2
pifagorianThree = head [x*y*z| x <-[1..500], y <-[1..500], z <- [1..500], x*x + y*y == z*z, x+y+z == 1000]

--3
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : xss)   = transpose xss
transpose xss = (map (head) xss) : (transpose (map (\xs -> tail xs) xss))