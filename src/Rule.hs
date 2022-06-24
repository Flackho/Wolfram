module Rule where
import Data.Char (intToDigit)
import Data.Maybe ( fromJust, isNothing ) 
import System.Environment (getArgs)
data Rule = Rule Char Char Char Char Char Char Char Char

instance Show Rule where
    show (Rule a b c d e f g h) = a:b:c:d:e:f:g:h:[]

intToBin :: Int -> [Char]
intToBin 0 = []
intToBin n = intToBin (div n 2) ++ [intToDigit (mod n 2)]

initRule :: Int -> Rule
initRule n = Rule
   (if bin !! 0 == '1' then '*' else ' ')
   (if bin !! 1 == '1' then '*' else ' ')
   (if bin !! 2 == '1' then '*' else ' ')
   (if bin !! 3 == '1' then '*' else ' ')
   (if bin !! 4 == '1' then '*' else ' ')
   (if bin !! 5 == '1' then '*' else ' ')
   (if bin !! 6 == '1' then '*' else ' ')
   (if bin !! 7 == '1' then '*' else ' ')
    where bin = replicate (8 - length(intToBin n)) '0' ++ intToBin n

findCase :: Rule -> (Char, Char, Char) -> Char
findCase  (Rule a b c d e f g h) ('*', '*', '*') = a
findCase  (Rule a b c d e f g h) ('*', '*', ' ') = b
findCase  (Rule a b c d e f g h) ('*', ' ', '*') = c
findCase  (Rule a b c d e f g h) ('*', ' ', ' ') = d
findCase  (Rule a b c d e f g h) (' ', '*', '*') = e
findCase  (Rule a b c d e f g h) (' ', '*', ' ') = f
findCase  (Rule a b c d e f g h) (' ', ' ', '*') = g
findCase  (Rule a b c d e f g h) (' ', ' ', ' ') = h
findCase _ _ = '\0'

genFstLine :: Int -> String
genFstLine n = fst tmp ++ ('*' : snd tmp)
        where tmp = splitAt (div n 2) (repeat ' ')

createTruplesList :: String -> [(Char, Char, Char)]
createTruplesList (x:y:z:zs) = (x, y, z) : createTruplesList (y:z:zs)
createTruplesList _ = []

genNthLine :: String -> Rule -> String
genNthLine [] _ = []
genNthLine str rule = map (findCase rule) (createTruplesList str)

invRule :: Rule -> Rule
invRule (Rule a b c d e f g h) = Rule a e c g b f d h

wolfram :: Rule -> String -> String -> Maybe Int -> [String]
wolfram _ _ _ (Just 0) = []
wolfram rule str left lines =
    str : wolfram rule newLine (genNthLine (head str:left) (invRule rule))
    (if isNothing lines then Nothing else Just (fromJust lines - 1))
        where newLine = genNthLine (head left:str) rule