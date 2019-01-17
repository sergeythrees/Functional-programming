calcMultiplicityNumbers :: Integer -> Integer -> Integer  -> [Integer]
calcMultiplicityNumbers startVal 0 multiplicity = []
calcMultiplicityNumbers startVal count 0 = [startVal]
calcMultiplicityNumbers startVal count multiplicity
   | startVal `mod` multiplicity /= 0 = calcMultiplicityNumbers (startVal + 1) count multiplicity
   | otherwise = startVal : calcMultiplicityNumbers (startVal + 1) (count - 1) multiplicity

main = do
     putStrLn "Enter start value: "
     startVal <- readLn
     putStrLn "Enter count of elements: "
     count <- readLn
     putStrLn "Enter multiplicity: "
     multiplicity <- readLn
     print (startVal : (calcMultiplicityNumbers (startVal + 1) (count - 1) multiplicity))