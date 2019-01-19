{-
factor :: Integer -> Maybe [Integer]
factor n | n <= 0 = Nothing
factor 1 = Just [1]
factor n = Just (factor_helper n 2)

factor_helper :: Integer -> Integer -> [Integer]
factor_helper 1 x = []
factor_helper n x
  | mod n x == 0 = x:(factor_helper (div n x) x)
  | otherwise    = factor_helper n (x+1)

sieve :: Integer -> [Integer]
sieve n = sieve_helper [2..n]

m_filter xs = filter (\a -> mod a (head xs) /= 0) xs
sieve_helper :: [Integer] -> [Integer]
sieve_helper nums
  | null nums = []
  | otherwise = (head nums) : (sieve_helper (m_filter nums))

-}

import System.IO

 --(b -> a -> b) -> b -> t a -> b
-- reducer :: [(String, Integer)] -> Char -> [(String, Integer)]
reducer :: [String] -> Char -> [String]
reducer prevs cur =
  let prevWord = (head prevs)
  in case prevWord of
    [] -> [cur] : prevs
    ' ':_ -> case cur of
      ' ' -> (' ' : prevWord) : (tail prevs)
      _   -> [cur] : prevs
    char -> case cur of
      ' '  -> " " : prevs
      char -> (char : prevWord) : (tail prevs)
    
tokenize :: String -> [[String]]
tokenize src =
  let tokens = map (\x -> foldl reducer [""] x) (lines src)
  in map (\x -> reverse (map reverse x)) tokens

-- chadLex :: String -> 

-- main :: IO ()
main = do
  handle <- openFile "file.src" ReadMode
  contents <- hGetContents handle
  print $ tokenize contents
  hClose handle
  
