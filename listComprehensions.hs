import Data.Char
sum100 = sum [x^2 | x<- [1..100]]


replicate1 :: Int -> a -> [a]
replicate1 n a = [a | _ <-[1..n]]


pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x<-[1..n],y<-[1..n],z<-[1..n],x^2 + y^2 ==z^2]

factors :: Int -> [Int]
factors n = [x | x <- [1..n],n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [2..n],isPerfect x]
	where isPerfect num = sum (init (factors num) )== num

divides :: Int -> Int -> Bool
divides x d = if x `mod` d ==0 then True else False

divisors :: Int -> [Int] 
divisors n = [x |x <- [1..n],divides n x]

riffle :: [a] ->[a] ->[a]
riffle xs ys = concat [ [x,y] | (x,y) <- xs `zip` ys]

let2int :: Char -> Int 
let2int x = ord x -ord 'a'

let2Int :: Char -> Int
let2Int x = ord x - ord 'A'

int2let :: Int -> Char
int2let n = chr (ord 'a' +n)

int2Let :: Int -> Char
int2Let n = chr (ord 'A' +n)

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | isUpper c = int2Let ((let2Int c + n) `mod` 26)
  | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x<- xs]

find :: (Eq a) => a-> [(a,b)] -> [b]
find k t = [ v | (p,v) <- t , p==k]

positions :: (Eq a) => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..n])
  where n = length xs -1

scalerProduct :: [Int] -> [Int] -> Int
scalerProduct xs ys = sum [ x*y | (x,y) <- xs `zip`ys]
