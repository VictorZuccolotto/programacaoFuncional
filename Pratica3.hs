main = return ()

--ex1a
customOr1 :: Bool -> Bool -> Bool
customOr1 _ True = True
customOr1 True _ = True
customOr1 _ _ = False

customOr2 :: Bool -> Bool -> Bool
customOr2 True True = True
customOr2 True False = True
customOr2 False True = True
customOr2 False False = False

customOr3 :: Bool -> Bool -> Bool
customOr3 True _ = True
customOr3 False b = b

customOr4 :: Bool -> Bool -> Bool
customOr4 bool1 bool2
    | bool1 == True = True
    | bool2 == True = True
    | otherwise     = False


customOr5 :: Bool -> Bool -> Bool
customOr5 bool1 bool2
    | bool1 == False && bool2 == False = False
    | otherwise                        = True
 
--ex2
type Data = (Float,Float)
distP :: Data -> Data -> Float
distP (x1,y1) (x2,y2) = sqrt(((x2 - x1) ** 2) + ((y2 - y1) ** 2))

--ex3
--   1:[2,3,4]
-- => [1,2,3,4]
--   'a':['b','c','d']
-- => "abcd"
--   head [1,2,3]
-- => 1
--   tail [1,2,3]
-- => [2,3]
--   [1,5,2,3]!!1
-- => 5
--   [1,5,2,3]!!3
-- => 3
--   elem 2 [1,5,2,3]
-- => True
--   take 2 [1,5,2,3,7]
-- => [1,5]
--   drop 2 [1,5,2,3,7]
-- => [2,3,7]
--   [1,2] ++ [3,4]
-- => [1,2,3,4]
--   [1..10]
-- => [1,2,3,4,5,6,7,8,9,10]
--   [7,6..3]
-- => [7,6,5,4,3]
--   ['b'..'g']
-- => "bcdefg"
--   take 5 [1,3..]
-- => [1,3,5,7,9]
--   sum [1..10]
-- => 55
--   maximum [1,5,2,3,7]
-- => 7
--   minimum [1,5,2,3,7]
-- => 1

--ex4
fatorialg :: Int -> Int
fatorialg n
 | n == 0 = 1
 | otherwise = n * fatorialg (n-1)

fatorialp :: Int -> Int
fatorialp 0 = 1
fatorialp n = n * fatorialp (n-1)

--ex5
fibo :: Int -> Int
fibo 0 = 1
fibo 1 = 1
fibo n = fibo(n-2) + fibo(n-1)

--ex6
triang :: Int -> Int
triang 0 = 0
triang 1 = 1
triang n = triang(n-1) + n

--ex7
p :: (Int, Int) -> (Int, Int)
p(x, y) = (y, y+x)

fib :: Int -> (Int,Int)
fib 0 = (0,1)
fib n = (fib (n-1))

--ex8
potencia2 :: Float -> Float
potencia2 0 = 1
potencia2 1 = 2
potencia2 n = 2 * potencia2(n-1)

--ex9
prodIntervalo :: Int -> Int -> Int
prodIntervalo m n
 | m == n = n
 | otherwise = n * prodIntervalo m (n -1)

--ex11
resto_div :: Int -> Int -> Int
resto_div a b =
  if a == 0 || b == 1
    then 0
    else
      if a < b
        then a
        else resto_div(a - b) b

div_inteira :: Int -> Int -> Int
div_inteira a b =
  if (a < b)
    then 0
    else (div_inteira (a - b) b) + 1

--ex12
mdcp :: (Int,Int) -> Int
mdcp (m,0) = m
mdcp(m,n) = mdcp(n, mod m n)

mdcg :: (Int,Int) -> Int
mdcg (m, n)
 | m == 0 = n
 | m > 0 = mdcg(n `mod` m,m)

--ex13
binomialp :: (Int,Int) -> Int
binomialp (n,0) = 1
binomialp (n,k) = if (k == n) then 1
else binomialp (n-1,k) + binomialp (n-1,k-1)

binomialg :: (Int,Int) -> Int
binomialg (n,k)
 | k == 0 = 1
 | n == k = 1
 | otherwise = binomialg (n-1,k) + binomialg (n-1,k-1)

--ex14
--   [5,4..1]
-- => [5,4,3,2,1]
--   ['a','c'..'e']
-- => "ace"
--   [1,4..16]
-- => [1,4,7,10,13,16]
--   [1, -2..(-11)] [1,5..17]
--   zip [1, -2..(-11)] [1,5..17]
-- => [(1,1),(-2,5),(-5,9),(-8,13),(-11,17)]
   

--ex15
lista :: Int -> Int -> [Int]
lista a b
 | a == b = [a]
 | a > b = []
 | otherwise = a:(lista (a+1) b)

listaPares :: Int -> Int -> [Int]
listaPares a b
 | a == b = []
 | a + 1 == b = []
 | a > b = []
 | mod a 2 == 0 = listaPares (a+1) b
 | otherwise = (a+1):(listaPares (a+1) b)