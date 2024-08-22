import Data.Char

char2int :: Char -> Int -- mapping to integer from character
char2int c = ord c - ord 'a'

int2char :: Int -> Char -- mapping to character from integer
int2char n = chr (ord 'a' + n)

shiftletter  :: Int -> Char -> Char -- shifts individual letters
shiftletter n c | isLower c = int2char (char2int c + n `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shiftletter n x | x <- xs]
