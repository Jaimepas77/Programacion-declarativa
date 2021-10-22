--Autor: Jaime Pastrana García

--1

--last
last'::[a] -> a
last' xs = foldl (\x y -> y) undefined xs

--reverse
reverse'::[a] -> [a]
reverse' xs = foldl (\ac x -> x:ac) [] xs

--all
all'::(a -> Bool) -> [a] -> Bool
all' p xs = foldl (\x y -> x && p y) True xs

--minimum
minimum'::Ord a => [a] -> a
minimum' xs = foldl (\x y -> min x y) (head xs) xs

--map
map'::(a->b) -> [a] -> [b]
map' f xs = foldr (\y bs -> (f y):bs) [] xs

--filter
filter'::(a->Bool) -> [a] -> [a]
filter' p xs = foldr (\y bs -> if p y then y:bs else bs) [] xs

--takeWhile
takeWhile'::(a->Bool) -> [a] -> [a]
takeWhile' p xs = foldr (\y bs -> if p y then y:bs else []) [] xs

--2

invertidos::[Int]
invertidos = foldl (\xs y -> y:(y*(-1)):xs) [] [100, 99..1]

--3

natEnum::[(Int, Int)]
natEnum = concat [[(x,y)| x<-[0..n], y<-[0..n], x + y == n] | n<- [0..]]

--4

--a
sufijos::[a] -> [[a]]
sufijos xs = [(drop n xs) | n <- [0..(length xs)]]

--b
sublistas::[a] -> [[a]]
sublistas xs = [(drop n (take m xs)) | n <- [0..(length xs)], m <- [n+1..(length xs)]] ++ [[]]

--c
permuta::[Int] -> [[Int]]
permuta [a] = [[a]] --Caso base
permuta xs = [ x:ys | x <- xs, ys <- permuta (filter (/= x) xs)] --Caso recursivo

--d
sumandos::Int -> [[Int]]
sumandos 0 = [[]]  --Caso base
sumandos x = [ y:zs | y <- [1..x], zs <- sumandos (x-y), (null zs) || (y <= head zs)]  --Caso recursivo
--Con la expresión (null zs) || (y <= head zs) se consigue evitar que la lista tenga los elementos en orden descendiente