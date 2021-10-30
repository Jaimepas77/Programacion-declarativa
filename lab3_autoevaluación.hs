--Autor: Jaime Pastrana García
--Edición de autoevaluación (con comentarios justificando y anotando puntuaciones)

--1

--last -> tipo correcto y funcionamiento correcto (mejorable el trato de casos que resultan en errores de ejecución)
last'::[a] -> a
last' xs = foldl (\x y -> y) undefined xs

--reverse -> tipo correcto y funcionamiento correcto
reverse'::[a] -> [a]
reverse' xs = foldl (\ac x -> x:ac) [] xs

--all -> tipo mejorable aunque correcto (posible uso de foldable) y funcionamiento correcto
all'::(a -> Bool) -> [a] -> Bool
all' p xs = foldl (\x y -> x && p y) True xs

--minimum -> tipo correcto y funcionamiento correcto (posible manejo del error en lista vacía)
minimum'::Ord a => [a] -> a
minimum' xs = foldl (\x y -> min x y) (head xs) xs

--map -> tipo correcto y funcionamiento correcto
map'::(a->b) -> [a] -> [b]
map' f xs = foldr (\y bs -> (f y):bs) [] xs

--filter -> Tipo correcto y funcionamiento correcto
filter'::(a->Bool) -> [a] -> [a]
filter' p xs = foldr (\y bs -> if p y then y:bs else bs) [] xs

--takeWhile -> tipo correcto y funcionamiento correcto
takeWhile'::(a->Bool) -> [a] -> [a]
takeWhile' p xs = foldr (\y bs -> if p y then y:bs else []) [] xs

--Resultado: 4/4

--2 -> Se usa foldl y se especifica el tipo. Además el funcionamiento es correcto.

invertidos::[Int]
invertidos = foldl (\xs y -> y:(y*(-1)):xs) [] [100, 99..1]

--Resultado: 1/1

--3 -> Se usan dos listas intensionales (se podía haber usado una sola), se especifica el tipo y el funcionamiento es correcto.

natEnum::[(Int, Int)]
natEnum = concat [[(x,y)| x<-[0..n], y<-[0..n], x + y == n] | n<- [0..]]
--Solución mejorada: natEnum = [(x,y)| n<-[0..], x<-[0..n], y<-[0..n], x + y == n]

--Resultado: 1.25/1.5

--4

--a -> Se emplea orden superior y una lista intensional. El funcionamiento y tipos son correctos
sufijos::[a] -> [[a]]
sufijos xs = [(drop n xs) | n <- [0..(length xs)]]

--b -> Se emplea orden superior y una lista. El funcionamiento es correcto. La implementación aunque correcta podría ser mejor
sublistas::[a] -> [[a]]
sublistas xs = [(drop n (take m xs)) | n <- [0..(length xs)], m <- [n+1..(length xs)]] ++ [[]]
--Solución alternativa: sublistas xs = [(drop n (take m xs)) | n <- [-1..(length xs)], m <- [n+1..(length xs)]]

--c -> Se hace uso de una lista intensional, funciones de orden superior y recursividad (con ajuste de patrones). El funcionamiento resulta correcto en la mayor parte de los casos.
permuta::[Int] -> [[Int]]
permuta [a] = [[a]] --Caso base
permuta xs = [ x:ys | x <- xs, ys <- permuta (filter (/= x) xs)] --Caso recursivo
--Error encontrado: funciona mal con elementos repetidos

--d -> Se hace uso de una lista intensional, orden superior y recursividad (con ajuste de patrones). El funcionamiento es correcto.
sumandos::Int -> [[Int]]
sumandos 0 = [[]]  --Caso base
sumandos x = [ y:zs | y <- [1..x], zs <- sumandos (x-y), (null zs) || (y <= head zs)]  --Caso recursivo
--Con la expresión (null zs) || (y <= head zs) se consigue evitar que la lista tenga los elementos en orden descendiente

--Resultado: 3/3.5

--RESULTADO FINAL: 9.25/10