module Test where


--1)
signo :: Int -> Int
signo x
    | x == 0 = 0
    | x > 0  = 1
    | otherwise = -1

negativo :: Int -> Bool
negativo x = signo x == -1


--2)
max2 :: Int -> Int -> Int
max2 x y 
     |x > y = x
     |otherwise = y

max3 :: Int -> Int -> Int -> Int
max3 x y z = max2 x (max2 y z)

min2 :: Int -> Int -> Int
min2 x y = if x<y then x else y


--3)
factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial(x-1)

combinatorio :: Int -> Int -> Int
combinatorio n k =  div (factorial n) ((factorial k)*(factorial(n - k))) 

combinatorio2 :: Int-> Int -> Int -> Int -> Int
combinatorio2 n k r acum  
        |r == k+1 = div acum (factorial(n-k))
        |r == n-k+1 = div acum (factorial(k))
        |otherwise = combinatorio2 n k (r-1) (acum*(r-1))

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonnaci x = fibonacci(x-1) + fibonacci(x-2)

divisiblePor :: Int -> Int -> Bool
divisiblePor x y = mod x y == 0


--4)
esVacia :: [a] -> Bool
esVacia x = length x == 0

cabeza :: [a] -> a
cabeza (x:xs) = x

resto :: [a] -> [a]
resto (x:xs) = xs


--5)
--
long :: [a] -> Int
long [] = 0
long (x:xs) = 1 + long(xs)

sumLista :: [Int] -> Int
sumLista [] = 0
sumLista (x:xs) = x + sumLista(xs)

member :: Int->[Int]->Bool
member a [] = False
member a (x:xs) = a==x || member a xs

appendUno :: [a] -> a -> [a]
appendUno [] a = [a]
appendUno (x:xs) a = x : appendUno xs a

append :: [a] -> [a] -> [a]
append l [] = l
append xs (y:ys) = append (appendUno xs y) ys

tomar :: Int -> [a] -> [a]
tomar 0 l = [] 
tomar a (x:xs) = x : tomar (a-1) xs  

term :: Int -> [a] -> a
term 0 l = head l
term i (x:xs) = term (i-1) xs

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = appendUno (rev xs) x

maxl :: [Int] -> Int
maxl [x] = x
maxl (x:xs) = if x >= maxl xs then x else maxl xs

cuenta ::  Eq a => a -> [a] -> Int
cuenta a [] = 0
cuenta a (x:xs) = if a==x then 1 + cuenta a xs else cuenta a xs

repite :: a -> Int -> [a]
repite a 0 = []
repite a n = a:repite a (n-1)


--6)
--
(@@) :: Int -> Int -> [Int]
desde @@ hasta = if desde == hasta then [desde] else desde: ((desde+1) @@ hasta)

multiplicaTodos :: [Int] -> Int
multiplicaTodos (x:[]) = x
multiplicaTodos (x:xs) = x * multiplicaTodos xs

factorial2 :: Int -> Int
factorial2 0 = 1
factorial2 n = multiplicaTodos(1 @@ n)

--7)
ultimoExplicito :: [a] -> a
ultimoExplicito (x:[]) = x
ultimoExplicito (x:xs) =ultimoExplicito xs

ultimoImplicito :: [a] -> a
ultimoImplicito l = ultimoExplicito l

sacarUltimoExplicito :: [a] -> [a]
sacarUltimoExplicito (x:[]) = []
sacarUltimoExplicito (x:xs) = x:sacarUltimoExplicito xs

--8)
--
capicua :: Eq a => [a] -> Bool
capicua (x:[])= True
capicua (x:xs) = x == ultimoExplicito (x:xs) && capicua(sacarUltimoExplicito xs)


--9)
flat :: [[a]] -> [a]
flat [] = []
flat (xs:xss) = append xs (flat xss)

longLl :: [[a]] -> Int
longLl xss = long (flat xss)

--10)
intercalar :: [a]->[a]->[a]
intercalar xs [] = xs
intercalar [] ys = ys
intercalar (x:xs) (y:ys) = x:y:intercalar xs ys

aparear :: [Int] -> [Int] -> [Int]
aparear xs [] = xs
aparear [] ys = ys
aparear (x:xs) (y:ys) = if x<=y then x:aparear xs (y:ys) else y:aparear (x:xs) ys 

--11)
--
mapear :: Int -> Char
mapear x
  |x==0='0'
  |x==1='1'
  |x==2='2'
  |x==3='3'
  |x==4='4'
  |x==5='5'
  |x==6='6'
  |x==7='7'
  |x==8='8'
  |x==9='9'
  |x==10='A'
  |x==11='B'
  |x==12='C'
  |x==13='D'
  |x==14='E'
  |otherwise='F'

dexAHex :: Int -> String
dexAHex x = if (div x 16) < 16 then
              mapear(div x 16):mapear(mod x 16):[]
            else
              appendUno (dexAHex (div x 16)) (mapear(mod x 16))


--12) al segundo parametro se le pasa el primero menos 1 para no contarse a si mismo como divisor
divisores :: Int -> Int -> [Int]
divisores x 0 = []
divisores x y = if mod x y == 0 then y:divisores x (y-1) else divisores x (y-1)

perfecto :: Int -> Bool
perfecto x = x == sum (divisores x (x-1))




