module Test where

--PRACTICA 3
--
--
--2)
--i)
maxf :: Ord a => (a -> a) -> [a] -> a
maxf f [] = error "Lista vacía..."
maxf f xs = maximum(map f xs)

--ii)

--3)
--i)



--4)
--i)
genLista :: Int -> a -> (a->a) -> [a]
genLista n e f= scanl (\acc x -> f acc) e (replicate n e) --scanl hace lo mismo que foldl pero genera una lista con todos los acumuladores intermedios

--ii)
fromto2 :: Int -> Int -> [Int]
fromto2 d h = genLista (h-d) d (+1) 

--5)

--Recursion explicita
filter2 :: [a] -> (a->Bool) -> [a]
filter2 [] f = []
filter2 (x:xs) f = if f x then (x:filter2 xs f) else filter2 xs f

--Lista por comprensión
filter22 :: [a] -> (a -> Bool) -> [a]
filter22 xs f = [x|x<-xs,f x]



--30)
--i)
appendCond::[[a]] -> (a -> Bool) -> [a]
appendCond xs f = concat [x|x<-xs,cumpleCond f x]
  where cumpleCond fun ys =  foldr (\y acc -> acc && (fun y)) True ys

--usando map y foldr
appendCond2::[[a]] -> (a -> Bool) -> [a]
appendCond2 xs f = foldr (\x acc -> if cumpleCond f x then x++acc else acc) [] xs
  where cumpleCond fun ys =  and (map fun ys)

--ii)

--Con tolerancia
appendCond2T::[[a]] -> Int-> (a -> Bool) -> [a]
appendCond2T xs t f = foldr (\x acc -> if cumpleCond f x t then x++acc else acc) [] xs
  where cumpleCond fun ys tol=  length(filter fun ys) >= tol
