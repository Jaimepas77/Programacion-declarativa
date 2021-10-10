--Autor: Jaime Pastrana García

--1_____________
--a
cuadrados::Int -> [Int]
cuadrados n = map (^2) [0..n]

--b
cuadInv::Int -> [(Int, Int)]
cuadInv n = reverse$ zip [0..n] (map (^2) [0..n])

--c
sumCos::Float -> Float
sumCos n = sum$ map (\x -> x*abs(cos(x))) [1..n]

--d
--sumRara::Int -> Int
--sumRara n = sum$ (filter (\x -> x `mod` 3 == 0) [1..n]) ++ filter (\x -> x `mod` 5 == 0) [1..n]
--Quizás concatenar dos listas no es la mejor manera de hacerlo...
sumStrange::Int -> Int
sumStrange n = sum$ filter (\x -> x `mod` 3 == 0 || x `mod` 5 == 0) [1..n]

--e
--Usar la evaluación perezosa
primoMay::Int -> Int
primoMay x = head$ filter (\n -> (n > x && (divisores n == []))) [0..]
divisores::Int -> [Int]
divisores n = filter (\x -> n `mod` x == 0) [2..n-1]

--2__________________
--a
iguales::(Enum a, Eq b) => (a -> b) -> (a->b) -> a -> a -> Bool
iguales f g n m = all (\x -> f x == g x) [n..m]

--b
menor::(Enum a) => a -> (a->Bool) -> a
menor n p = head$ filter p [n..]

--c
mayorA::(Enum a)=>a -> a -> (a->Bool) -> a
mayorA n m p = last$ filter p [n..m]

--d
ex::Enum a => a -> a -> (a ->Bool) -> Bool
ex n m p = not$ null$ filter p [n..m]

--3___________________
--a
filter2::Enum a => [a] -> (a->Bool) -> (a->Bool) -> ([a], [a])
filter2 xs p q = (filter p xs, filter q xs)

--b
--filters::Enum a => [a] -> [(a->Bool)] -> [[a]]
--filters xs ps = filter p xs
--(comentado para que compile)

--c
mapx x xs = map (\f -> f x) xs