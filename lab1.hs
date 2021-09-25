--Autor: Jaime Pastrana García
--Versión 2

--1 Calcular años, días, horas, minutos y segundos
--1.a
result :: (Int, Int, Int, Int, Int)
result = let {total = 1000000; syear = (60*60*24*365); sdias = (60*60*24);
              shoras = (60*60); smins = 60; ssegs = 1} in
              --tupla
             (total `div` syear, --años
             (total `mod` syear) `div` sdias, --días
             (total `mod` sdias) `div` shoras, --horas
             (total `mod` shoras) `div` smins, --mins
             (total `mod` smins) `div` ssegs)  --segs
             
--1.b
segundos :: Int -> (Int, Int, Int, Int, Int)
segundos n = let {total = n; syear = (60*60*24*365); sdias = (60*60*24);
              shoras = (60*60); smins = 60; ssegs = 1} in
              --tupla
             (total `div` syear, --años
             (total `mod` syear) `div` sdias, --días
             (total `mod` sdias) `div` shoras, --horas
             (total `mod` shoras) `div` smins, --mins
             (total `mod` smins) `div` ssegs)  --segs
             
--2.a
bisiesto :: Int -> Bool
{-bisiesto n = if n `mod`4 == 0
              then True
              else False
             -}
--2.b
bisiesto n 
 | n `mod` 4 == 0 = True
 | otherwise = False
 
--3 
media :: Floating a => [a] -> a
media xs = (sum xs) / fromIntegral (length xs)

--4.a
num_digitos :: Int -> Int
num_digitos x
 |x<10 = 1
 |otherwise = 1+num_digitos (x `div` 10)

--4.b 
--función auxiliar necesaria para reduccion
sum_digitos :: Int -> Int
sum_digitos x
 | x < 10 = x
 | otherwise = x `mod` 10 + sum_digitos (x `div` 10)

reduccion :: Int -> Int
reduccion x
 | x < 0 = reduccion (abs x)
 | x < 10 = x 
 | otherwise = reduccion (sum_digitos x)

--5
disyuncion1 :: Bool -> Bool -> Bool
disyuncion1 _ True = True --Consecuencia: estricto en el segundo argumento
disyuncion1 True _ = True
disyuncion1 False x = x --El primero es estricto si el segundo es False (inevitable)

disyuncion2 :: Bool -> Bool -> Bool
disyuncion2 True _ = True --Consecuencia: estricto en el primer argumento
disyuncion2 _ True = True
disyuncion2 False x = x --El segundo es estricto si el primero es False (inevitable)

disyuncion3 :: Bool -> Bool -> Bool
disyuncion3 False x = x  --Consecuencia: estricto en el primer argumento (Equivalente a disyuncion2)
disyuncion3 True _ = True 
--disyuncion3 _ True = True --Redundante

disyuncion4 :: Bool -> Bool -> Bool
disyuncion4 False x = x --Consecuencia: estricto en ambos argumentos (siempre)
disyuncion4 x False = x
disyuncion4 True _ = True


