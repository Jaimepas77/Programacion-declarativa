--Autor: Jaime Pastrana García

--1
adivina::Int -> IO()
adivina n = do  putStrLn("Introduzca el nº que crea que es el correcto...")
                x <- getInt
                if x == n then putStrLn("¡¡Acertaste!!")
                else do if x > n then putStrLn("Demasiado alto")
                        else putStrLn("Muy pequeño")
                        adivina n

getInt::IO Int
getInt = do line <- getLine
            return (read line::Int)

--2
data Matriz = M[[Float]]

--a
leerM::IO Matriz    --Se ve forzadpo a leer nºs de línea en línea
leerM = do  putStr("Introduzca el nº de filas que tendrá su matriz: ")
            x <- getInt
            --putStr("Introduzca el nº de columnas que tendrá su matriz: ")
            --y <- getInt
            putStrLn("Introduzca la matriz poniendo una fila por línea")
            leerFilas x []

getFloat::IO Float
getFloat = do   line <- getLine
                return (read line::Float)

leerFilas::Int -> [[Float]] -> IO Matriz
leerFilas x m = do  f <- leerLinea []
                    if x == 1 then return $M (m++[f])
                        else leerFilas (x-1) (m++[f])

leerLinea::[Float] -> IO [Float]
leerLinea ys = do   xs <- getLine
                    return $strToFloat $words xs

strToFloat::[String] -> [Float]
strToFloat [] = []
strToFloat (x:xs) = (read x::Float):(strToFloat xs)

{---Antiguo
leerLinea y ys = do e <- getFloat     
                    if y == 1 then return (ys ++ [e])
                    else leerLinea (y-1) (ys++[e])
-}

--b
dibujaMatriz:: Matriz -> IO()
dibujaMatriz (M l) = do dibujaFila (head l)
                        --putStrLn("______")
                        if length l == 1 then return ()
                        else dibujaMatriz (M (tail l))

dibujaFila:: [Float] -> IO()
dibujaFila [x] = do putStrLn(show x)
dibujaFila (x:xs) = do  putStr(show x ++ "|")
                        dibujaFila xs

--Demostración (llamar a actuar para leer una matriz y ver como se imprime por pantalla)
actuar::IO()
actuar = do m <- leerM
            dibujaMatriz m

--3
formatea::String -> String -> Int -> IO ()
formatea fileIn fileOut n = do  text <- readFile fileIn
                                --print("ERROR 404: SOLUTION NOT FOUND")--Pendiente de implementar
                                writeFile fileOut $justifica (lines text) n

justifica::[String] -> Int -> String
justifica [] _ = ""
justifica (l:ls) n = if huecos >= 1 && caracteres < n then
                            (concatenar (words l) ((n-caracteres) `div` huecos)) ++ "\n" ++ justifica ls n
                        else "\n" ++ justifica ls n
                    where   huecos = (length(words l)-1)
                            caracteres = length (concat(words l))

concatenar::[String] -> Int -> String
concatenar [word] _ = word
concatenar (word:ws) s = word++(espacios s)++(concatenar ws s)
concatenar [] _ = ""

espacios::Int -> String
espacios n = if n > 0 then ' ':(espacios (n-1))
                else " "
