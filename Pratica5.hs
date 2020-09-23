main = return ()

--1
--a
type Data = (Int,Int,Int)
valida :: Data -> Bool
valida (d,m,a)
 |d<1 || m>12 || a<1 || m<1= False
 |m == 2 && d > 28 && b == False =False
 |m == 2 && b == True && d>29 = False
 |mesimpar &&  d<= 31 = True
 |mespar &&  d<= 30 = True
 |otherwise = True
 where
 mespar = even m
 mesimpar = odd m
 b = bissexto a

bissexto :: Int->Bool
bissexto x = if (div4 && ndiv100 || div400) then True else False
 where
 div4 = mod x 4 == 0
 ndiv100 = mod x 100 /= 0
 div400 = mod x 400 == 0

--b
bissextolst :: [Int] -> [Int]
bissextolst a = lst
  where
  lst = [x | x <-a, bissexto x]

--c
type Emprestimo = (String, String, Data, Data, String)

type Emprestimos = [Emprestimo]

bdEmprestimo :: Emprestimos
bdEmprestimo = [("H123C9", "BSI945", (12, 9, 2009), (20, 09, 2009), "aberto"),("L433C5", "BCC021", (01, 9, 2009), (10, 09, 2009), "encerrado"),("M654C3", "BCC008", (04, 9, 2009), (15, 09, 2009), "aberto")]

procede :: Data -> Data -> Bool
procede (d, m, a) (d2, m2, a2) = not (aux1 || aux2 || aux3 || aux4)
  where
  aux1 = not (valida (d, m, a)) || not (valida (d2, m2, a2))
  aux2 = a > a2
  aux3 = a == a2 && m > m2
  aux4 = a == a2 && m == m && d > d2

emprestimoEmDia :: Data -> Emprestimo -> Bool
emprestimoEmDia dataAtual (codLivro, codAluno, dataEmprest, dataDevo, status) = aux
  where
  aux = procede dataAtual dataDevo

atrasados :: Emprestimos -> Data -> Emprestimos
atrasados listaEmprestimos dataAtual = aux
  where
  aux = [x | x <- listaEmprestimos, not (emprestimoEmDia dataAtual x)]

--d
passo :: (Int, Int) -> (Int, Int)
passo (x, y) = aux
  where
  aux = (y, x + y)

fibo2 :: Int -> (Int, Int)
fibo2 0 = (0, 1)
fibo2 n = aux
  where
  aux = passo (fibo2 (n -1))

--e
prodIntervalo :: Int -> Int -> Int
prodIntervalo m n = if (m >= n) then aux1 else aux2
  where
  aux1 = n
  aux2 = (m * (prodIntervalo (m + 1) n))

fatorial :: Int -> Int
fatorial n = aux
  where
  aux = prodIntervalo 1 n
-- ===============================
-- ===============================
--2
-- Exercício 2:
--a
bissextoLet :: Int -> Bool
bissextoLet ano =
  let div400 = (mod ano 400 == 0)
      div4 = (mod ano 4 == 0)
      ndiv100 = (mod ano 100 /= 0)
   in div400 || (div4 && ndiv100)

type DataLet = (Int, Int, Int)

validaLet :: DataLet -> Bool
validaLet (d, m, a) =
  let aux1 = d >= 1 && d <= 31 && (m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12)
      aux2 = d >= 1 && d <= 30 && (m == 4 || m == 6 || m == 9 || m == 11)
      aux3 = d >= 1 && d <= 28 && m == 2 && not (bissextoLet a)
      aux4 = d >= 1 && d <= 29 && m == 2 && (bissextoLet a)
   in aux1 || aux2 || aux3 || aux4

--b
bissextosLet :: [Int] -> [Int]
bissextosLet lista =
  let aux = [x | x <- lista, bissextoLet x]
   in aux

--c
type EmprestimoLet = (String, String, DataLet, DataLet, String)

type EmprestimosLet = [EmprestimoLet]

bdEmprestimoLet :: EmprestimosLet
bdEmprestimoLet =
  [ ("H123C9", "BSI945", (12, 9, 2009), (20, 09, 2009), "aberto"),
    ("L433C5", "BCC021", (01, 9, 2009), (10, 09, 2009), "encerrado"),
    ("M654C3", "BCC008", (04, 9, 2009), (15, 09, 2009), "aberto")
  ]

procedeLet :: DataLet -> DataLet -> Bool
procedeLet (d, m, a) (d2, m2, a2) =
  let aux1 = not (validaLet (d, m, a)) || not (validaLet (d2, m2, a2))
      aux2 = a > a2
      aux3 = a == a2 && m > m2
      aux4 = a == a2 && m == m && d > d2
   in not (aux1 || aux2 || aux3 || aux4)

emprestimoEmDiaLet :: DataLet -> EmprestimoLet -> Bool
emprestimoEmDiaLet dataAtual (codLivro, codAluno, dataEmprest, dataDevo, status) =
  let aux = procedeLet dataAtual dataDevo
   in aux

atrasadosLet :: EmprestimosLet -> DataLet -> EmprestimosLet
atrasadosLet listaEmprestimos dataAtual =
  let aux = [x | x <- listaEmprestimos, not (emprestimoEmDiaLet dataAtual x)]
   in aux

--d
passoLet :: (Int, Int) -> (Int, Int)
passoLet (x, y) =
  let aux = (y, x + y)
   in aux

fibo2Let :: Int -> (Int, Int)
fibo2Let 0 = (0, 1)
fibo2Let n =
  let aux = passoLet (fibo2Let (n -1))
   in aux

--e
prodIntervaloLet :: Int -> Int -> Int
prodIntervaloLet m n =
  let aux =
        if (m >= n)
          then n
          else (m * (prodIntervaloLet (m + 1) n))
   in aux

fatInterLet :: Int -> Int
fatInterLet n =
  let exp1 = prodIntervaloLet 1 n
   in exp1

--3
-- I
-- (\x. 2 * x + 1) 3
-- 2*3 + 1
-- 6 + 1
-- 7

-- II
-- (\ xy. x-y) 5 7
-- 5 - 7
-- -2

-- III
-- (\ yx. x-y) 5 7
-- 7 - 5
-- 2

-- IV
-- (\ xy. x-y) (\z. z/2)
-- (\y.  (z/2) - y)

-- V
-- (\ xy. x-y) ((\z. z/2) 6 ) 1
-- (\ xy. x-y) (6/2) 1
-- (\ xy. x-y) 3 1
-- 3 - 1
-- 2

-- VI
-- (\ x. \ y. - x y) 9 4
-- (\ x. - x 4) 9 
-- ( - 9 4) 
-- 5

-- VII
-- (\ x. xx) (\ y. y)
-- (\ y. yy)

--4
-- (\x -> x + 3) 5
-- => 8
-- (\x -> \y -> x * y + 5) 3 4
-- => 17
-- (\(x,y) -> x * y^2) (3,4)
-- => 48
-- (\(x,y,_) -> x * y^2) (3,4,2)
-- => 48
-- (\xs -> zip xs [1,2,3]) [4,5,6]
-- => [(4,1),(5,2),(6,3)]

--5
-- (\x-> \y-> y)((\z-> z)(\z-> z))(\w-> w) 5
-- => 5
-- ((\f-> (\x-> f(f x))) (\y-> (y * y))) 3
-- => 81
-- ((\f-> (\x-> f(f x)))(\y->(y+y)) )  5
-- => 20
-- ((\x-> (\y-> x+y) 5) ((\y-> y-3) 7))
-- => 9
-- (((\f-> (\x-> f(f(f x)))) (\y-> (y * y))) 2)
-- => 256
-- (\x-> \y-> x+((\x-> x-3) y)) 5 6
-- => 8