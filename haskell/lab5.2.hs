import System.IO
import System.Environment   


replacePunctuation :: Char -> Char -> Char
replacePunctuation replaceTo inputChar
    | (any (inputChar==) [',',';',':','.','!','?','`','"','-','—','(',')']) = replaceTo
    | otherwise = inputChar

main = do
        [inp, out] <- getArgs 
        putStrLn "Enter symbol for replace punctuation: "
        replaceTo <- getChar
        s <- readFile inp
        writeFile out (map (replacePunctuation replaceTo) s)