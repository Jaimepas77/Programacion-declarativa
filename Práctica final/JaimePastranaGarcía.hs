--Autor: Jaime Pastrana García

--DEFINICIÓN DEL HASHMAP
--1. Definir el tipo Hash
hashSize = 10   --Con diez valores distintos
data Hash = Hash [[(String, String)]]

--2. Declarar el tipo Hash como instancia de Show, redefiniendolo para que muestre el tipo de manera similar al pdf.
instance Show Hash where
    show (Hash (h)) = "---\n" ++ hashToStr h 0 ++ "---\n"
    
hashToStr::[[(String, String)]] -> Int -> String
hashToStr [] _ = ""
hashToStr (it:list) i = "|" ++ show i ++ "|-> " ++ listToStr it ++ "\n" ++ hashToStr list (i+1) --Se recorren los índices recursivamente

listToStr::[(String, String)] -> String
listToStr [] = ""
listToStr ((w1, w2):ws) = foldl (\ret (t1, t2) -> ret ++ " | " ++ pair t1 t2) (pair w1 w2) ws   --w1 ++ ": " ++ w2 ++ " | " ++ .. ++ " | " ++ w1 ++ ": " ++ w2
                          where pair a b = a ++ ": " ++ b

--3. Definir una función hash que devuelva el índice de la palabra en la tabla (0-9)
hash::String -> Int
hash word = (fromEnum (head word)) `mod` hashSize    --Ascii del carácter módulo 10

------------------------------
--FUNCIONALIDAD DE  TRADUCIR--
file = "datos.txt"
traduce::IO ()
traduce = do txt <- readFile file
             dictionary <- return $initDictionary txt   --Obtenemos un Hash con el diccionario
             --putStrLn (show dictionary)   --Mostramos el diccionario leído (sirve para comprobar que funciona correctamente)
             userWords <- readWords --Obtenemos las palabras del usuario
             putStrLn("La longitud media de las palabras introducidas es " ++ show (medLength userWords))
             putStr("Traducciones resultantes:\n" ++ showResult dictionary userWords)   --Mostramos los resultados de la búsqueda en el diccionario

--4. Inicializar el diccionario (tabla hash) con el texto del fichero datos.txt
initDictionary::String -> Hash
initDictionary txt = Hash $encode $parse (lines txt)    --Una vez se ha parseado y codificado según el esquema del hashmap, se retorna como un tipo Hash

parse::[String] -> [(String, String)]   --Se parsean las traducciones como pares de palabras
parse ls = [(original, traduccion) | [original, traduccion] <- map words ls]

encode::[(String, String)] -> [[(String, String)]]  --Se asocia cada par a su hash correspondiente y se guarda en ese índice de la lista resultante
encode xs = [[(s1, s2) | (s1, s2) <- xs, hash s1 == n] | n <- [0..(hashSize-1)]]

--5. Leer varias palabras para traducir (I/O)
readWords::IO [String]
readWords = do putStr("Introduzca las palabras que quiere traducir separadas por espacios: ")
               userWords <- getLine
               return $words userWords

--6. Calcular la longitud media de las palabras introducidas
medLength::Floating a => [String] -> a
medLength xs = fromIntegral (sum (map length xs)) / fromIntegral (length xs)

--7. Buscar en el diccionario la traducción de las palabras (retorna una lista con las traducciones encontradas para esa palabra)
search::Hash -> String -> [String]
search (Hash dic) w = [s2 | (s1, s2) <- hashLine, s1 == w]  --Si encuentra varias entradas para una misma palabra se retornan todas
               where hashLine = head $drop (hash w) dic

--8. Mostrar traducción encontrada (se retorna en un string)
showResult::Hash -> [String] -> String
showResult dic ws = concat ["-" ++ w ++ ": " ++ (resultToStr (search dic w)) ++ "\n" | w <- ws] --De cada palabra se busca y muestra su traducción

resultToStr::[String] -> String --Implementación que acepta casos con varias acepciones para una misma palabra y que discierne el caso de que no está en el diccionario
resultToStr [] = "palabra no encontrada"    --Si la lista de acepciones para esa palabra está vacía es porque no está en el diccionario
resultToStr (x:xs) = foldl (\ret w -> ret ++ ", " ++ w) x xs    --Juntar todas las acepciones encontradas en un string separadas por comas
