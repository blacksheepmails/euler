import System.IO  
import System.Random
import Data.List
import Numeric
import Data.Ord
import qualified Data.List.Ordered as Ordered
import Data.Char
import qualified Data.Map as Map


prob1 = sum [ x | x <- [1..999], mod x 3 == 0 || mod x 5 == 0]

fibSeq = 1:2:[ a + b | (a,b) <- zip fibSeq (tail fibSeq)]

prob2 = sum [ x | x <- (takeWhile (< 4000000) fibSeq), even x] 

sieve :: [Int] -> [Int]
sieve (p:xs) = let noFactorIn (p:ps) (q:qs) x = q > x || x `mod` p > 0 && noFactorIn ps qs x
               in p:[ x | x <- xs, noFactorIn primes primeSquares x]
primeSquares = [ p * p | p <- primes]
primes = sieve [2..]

divides :: Integral a => a -> a -> Bool
divides a b = (b `mod` a) == 0

-- make fermat prime factorer laterz 
primeFactor :: Integral a => a -> [a]
primeFactor n = factorHelper n $ map fromIntegral primes
    where factorHelper n (p:xs)
            | n == 1 = []
            | p `divides` n = p:(factorHelper (quot n p) (p:xs))
            | otherwise = factorHelper n xs

prob3 = maximum (primeFactor 600851475143)

isPalindrome :: String -> Bool
isPalindrome xs = xs == reverse xs

prob4 = maximum [ n | x <-[999,998..100], y <- [x, (x-1)..100], let n = x*y, isPalindrome $ show n ]


lcm' :: Integral a => [a] -> a
lcm' = foldl1 lcm

prob5 = lcm' [1..20]

prob6 = (sum xs)^2 - (sum $ map (^2) xs) where xs = [1..100]

prob7 = primes !! 10000

greatestAdjProd :: Int -> [Integer] -> Integer
greatestAdjProd len x
    | length x < len = 0
    | otherwise = max (greatestAdjProd len $ tail x) (product $ take len x)

prob8 = do  
    content <- readFile "prob8data.txt"  
    return $ greatestAdjProd 13 $ map (toInteger . digitToInt) $ concat $ lines content

prob9 = [a*b*c | a <- [1..1000], b <- [a..(1000-a)], let c = 1000-a-b, a < c, b < c, a^2 + b^2 == c^2]

prob10 = sum $ takeWhile (< 2000000) $ map toInteger primes

prob11 = do  
    content <- readFile "prob11data.txt"
    let 
        grid = map (map (\x -> read x :: Integer) . words) $ lines content
        len = length grid
        right = maximum . map (greatestAdjProd 4)
        down = right . transpose
        diagonal1 = right . (\x -> map (diagLines1 x) $ concat [ [(x-1,0), (0,x-1)] | x <- [1..len]])
        diagonal2 = right . (\x -> map (diagLines2 x) $ concat [ [(x-1,0), (len-1,x-1)] | x <- [1..len]])
        diagLines1 grid (x,y) 
            | x >= len || y >= len = []
            | otherwise = grid !! x !! y : (diagLines1 grid (x+1,y+1))
        diagLines2 grid (x,y)
            | x <= 0 || y >= len = []
            | otherwise = grid !! x !! y : (diagLines2 grid (x-1, y+1))
    return $ maximum $ map ($ grid) [right, down, diagonal1, diagonal2]

triangularSeq = scanl1 (+) [1..]

prob12 = head $ dropWhile ((<= 500) . numDivisors) triangularSeq
    where numDivisors x = product $ map ((+1) . length) $ group $ primeFactor x

prob13 = do
    content <- readFile "prob13data.txt"
    return $ take 10 $ show $ sum $ map (\x -> read x :: Integer) $ lines content

collatzSeq x
    | x == 1 = [1]
    | even x = x : (collatzSeq $ x `div` 2)
    | odd x = x : (collatzSeq $ 3 * x + 1)

prob14 = maximumBy (comparing snd) $ Map.toList $ foldl (\ acc x -> snd $ collatzLen (x,0) acc) (Map.singleton 1 1) [1..1000000]
    where collatzLen (n, len) memo = 
            case Map.lookup n memo of
                Just a -> (a+len, memo)
                Nothing -> let (len',memo') = collatzLen (collatzSeq n !! 1, 0) $ memo 
                       in (len+len'+1, Map.insert n (len+len'+1) $ memo')
--main = putStrLn $ show $ prob14

factorial :: Integral a => a -> a
factorial n = product [1..n]

choose :: Integral a => a -> a -> a
choose n x = (factorial n) `div` ((factorial $ n-x) * (factorial x))

prob15 = 40 `choose` 20

prob16 = sum $ map digitToInt $ show $ 2^1000

prob17 = length $ concat $ map toString [1..1000]
    where toString n
            | n == 1000 = "onethousand"
            | n == 0 = ""
            | n >= 100 && null tensOnes = hundreds ++ "hundred"
            | n > 100 = hundreds ++ "hundredand" ++ tensOnes
            | n == 90 = "ninety"
            | n == 80 = "eighty"
            | n == 70 = "seventy"
            | n == 60 = "sixty"
            | n == 50 = "fifty"
            | n == 40 = "forty"
            | n == 30 = "thirty"
            | n == 20 = "twenty"
            | n == 18 = "eighteen"
            | n == 15 = "fifteen"
            | n == 13 = "thirteen"
            | n == 12 = "twelve"
            | n == 11 = "eleven"
            | n == 10 = "ten"
            | n == 9 = "nine"
            | n == 8 = "eight"
            | n == 7 = "seven"
            | n == 6 = "six"
            | n == 5 = "five"
            | n == 4 = "four"
            | n == 3 = "three"
            | n == 2 = "two"
            | n == 1 = "one"
            | n > 10 && n < 20 = ones ++ "teen"
            | otherwise = tens' ++ ones
            where
                ones = (toString $ n `rem` 10)
                tens' = (toString $ 10 * ( n `quot` 10))
                tensOnes = (toString $ n `rem` 100)
                hundreds = (toString $ n `quot` 100)

type Day = Int
type Month = Int
type Year = Int
type Date = (Day, Month, Year)

daysSeq date = date:(daysSeq $ next date)
    where
        isLeapYear (_, _, year)
            | (year `mod` 400) == 0 = True
            | (year `mod` 100) == 0 = False
            | (year `mod` 4) == 0 = True
            | otherwise = False
        isEndOfMonth (day, month, _)
            | day == 31 && (month `elem` [1,3,5,7,8,10,12]) = True
            | day == 30 && (month `elem` [9,4,6,11]) = True
            | day == 29 && (month == 2) = True
            | day == 28 && (month == 2) && (not $ isLeapYear date) = True
            | otherwise = False
        next date@(day, month, year)
            | not $ isEndOfMonth date = (day + 1, month, year)
            | month == 12 = (1, 1, year + 1)
            | otherwise = (1, month + 1, year)

prob18 = do
    content <- readFile "prob18data.txt"
    let
        triangle = map (map (\x -> read x :: Int) . words) $ lines content
        sumTriangle (i, j)
            | j >= length triangle = 0
            | otherwise = triangle !! j !! i + ( max (sumTriangle (i, j+1)) $ sumTriangle (i+1, j+1) )
    return $ sumTriangle (0,0)

prob19 = length $ filter (\(wd, (day,_,_)) -> wd == 7 && day == 1) $ takeWhile (\d -> (snd d) /= (31,12,2000)) $ dropWhile (\d -> (snd d) /= (1,1,1901)) $ zip (cycle [1..7]) (daysSeq (1,1,1900))

prob20 = sum $ map digitToInt $ show $ product [1..100]

divisors = divisorsHelper . group . primeFactor
    where 
        powers = scanl (*) 1
        divisorsHelper (x:xs)
            | xs == [] = powers x
            | otherwise = nub [ a * b | a <- (powers x), b <- (divisorsHelper xs)]
        divisorsHelper n = []

prob21 = sum $ filter (isAmicable) [1..10000]
    where 
        sumProperDivisors n =  (sum $ divisors n) - n
        isAmicable n
            | sumProperDivisors s == n && (s /= n) = True
            | otherwise = False
            where s = sumProperDivisors n


prob22 = do  
    content <- readFile "prob22data.txt"  
    let 
        names = sort $ read ("[" ++ content ++ "]") :: [String]
        vMap = Map.fromList $ zip ['A'..'Z'] [1..26]
        sumChars xs = sum $ map (\x -> case Map.lookup x vMap of (Just v) -> v) xs
    return $ sum $ map (\(name, i) -> sumChars name * i) $ zip names [1..]

prob36 = sum $ filter (isPalindrome . show) $ filter (isPalindrome . (\x -> showIntAtBase 2 intToDigit x "")) [1..1000000]

prob67 = do
    content <- readFile "prob67data.txt"
    let
        triangle = map (map (\x -> read x :: Int) . words) $ lines content
        sumTriangle i j
            | i >= length triangle = 0
            | otherwise = triangle !! i !! j + (max (sumMemo !! (i+1) !! j) $ sumMemo !! (i+1) !! (j+1))
        sumMemo = [[ sumTriangle i j | j <- [0..i]] | i <- [0..]]
    return $ sumTriangle 0 0