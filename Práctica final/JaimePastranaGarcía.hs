--Autor: Jaime Pastrana García

--DEFINICIÓN DEL HASHMAP
--1. Definir el tipo Hash
hashSize = 10
data Hash = Hash [[(String, String)]]

--2. Declarar el tipo Hash como instancia de Show, redefiniendolo para que muestre el tipo de manera similar al pdf.
instance Show Hash where
    show (Hash (h)) = "---\n" ++ hashToStr h 0 ++ "---\n"
    
hashToStr::[[(String, String)]] -> Int -> String
hashToStr [] _ = ""
hashToStr (it:list) i = "|" ++ show i ++ "|-> " ++ listToStr it ++ "\n" ++ hashToStr list (i+1)

listToStr::[(String, String)] -> String
listToStr [] = ""
listToStr ((w1, w2):ws) = w1 ++ ": " ++ w2 ++ " | " ++ listToStr ws

--3. Definir una función hash que devuelva el índice de la palabra en la tabla (0-9)
hash::String -> Int
hash word = (charPos (head word)) `mod` hashSize    --Posición del carácter respecto a 'A' módulo 10

charPos::Char -> Int
charPos 'A' = 0
charPos c = charPos (pred c) + 1

------------------------------
--FUNCIONALIDAD DE  TRADUCIR--
file = "datos.txt"
traduce::IO Hash
traduce = do txt <- readFile file
             dictionary <- return $initDictionary txt   --Obtenemos un Hash con el diccionario
             return dictionary--TEMPORAL

--4. Inicializar el diccionario (tabla hash) con la info del fichero datos.txt
initDictionary::String -> Hash
initDictionary txt = Hash $encode $parse (lines txt)

parse::[String] -> [(String, String)]
parse [] = []
parse (l:ls) = ((original, traduction):(parse ls))
                where original = head $words l
                      traduction = head $tail $words l

encode::[(String, String)] -> [[(String, String)]]
encode [] = [[]]
encode xs = [[(s1, s2) | (s1, s2) <- xs, hash s1 == n] | n <- [0..(hashSize-1)]]

--5. Leer varias palabras para traducir (I/O)
--6. Calcular la longitud media de las palabras introducidas
--7. Buscar en el diccionario la traducción de las palabras
--8. Mostrar traducción encontrada (I/O)