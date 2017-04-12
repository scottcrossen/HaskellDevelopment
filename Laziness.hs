isPrime :: Int -> Bool
-- Create list composing all values that divide k from 2 to sqrt k. Check if null.
isPrime k = null [ x | x <- [2..floor(sqrt(fromIntegral k))], k `mod` x  == 0]

primes :: [Int]
-- Find all primes
primes = filter (isPrime) [2..]

isPrimeFast :: Int -> Bool
-- As in 'isPrime', check all values before k. This time only primes.
isPrimeFast k = null [x | x <- takeWhile (<= floor(sqrt(fromIntegral k))) primesFast , k `mod` x == 0]

primesFast :: [Int]
-- Just as in 'primes'
primesFast = 2:filter (isPrimeFast) [3..]

longest :: String -> String -> String
longest str1 str2 = if length str1 > length str2 then str1 else str2 -- Pick largest string

lcsHelper1 :: String -> String -> String
lcsHelper1 str1 str2
  | ((length str1 == 0) || (length str2 == 0)) = "" -- Base case
  | (last str1 == last str2) = (lcsHelper1 (init str1) (init str2)) ++ [last str1] -- Recurse backwards. Match characters
  | (last str1 /= last str2) = longest (lcsHelper1 (init str1) str2) (lcsHelper1 str1 (init str2)) -- Diagonal step in 'array'

lcsRecursion :: String -> String -> Int
lcsRecursion str1 str2 = length (lcsHelper1 str1 str2) -- Take the largest length of the return

buildList :: Int -> (Int -> a) -> [a]
buildList sizeY func = [(func x) | x <- [0..(sizeY-1)]] -- Evaluate row/column of array

buildTable :: Int -> Int -> ( Int -> Int -> a) -> [[a]]
buildTable sizeX sizeY func = [buildList sizeY (func x) | x <- [0..(sizeX-1)]] -- Evaluate passed-in function for size of array

lcsHelper2 :: [[Int]] -> String -> String -> Int -> Int -> Int
lcsHelper2 table str1 str2 i j -- Partially evaluated function passed to 'buildTable'
  | (str1 !! i) == (str2 !! j) = (if (i > 0 && j > 0) then (table !! (i - 1) !! (j - 1)) else 0) + 1 -- Diagonally traverse array
  -- Traverse array vertically or horizontally. Whichever is the max 
  | otherwise = max (if (i > 0) then (table !! (i - 1) !! j) else 0) (if (j > 0) then (table !! i !! (j - 1)) else 0)

lcsArray :: String -> String -> Int
lcsArray str1 str2 = (table !! (length str1 - 1)) !! (length str2 - 1) -- Get final element of table
  where table = buildTable (length str1) (length str2) (lcsHelper2 table str1 str2) -- Build table

lcsLength :: String -> String -> Int
lcsLength str1 str2 = lcsArray str1 str2 -- I'll pick this method to be used for grading purposes.

