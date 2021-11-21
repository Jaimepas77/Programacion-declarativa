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
            putStr("Introduzca el nº de columnas que tendrá su matriz: ")
            y <- getInt
            putStrLn("Introduzca la matriz poniendo un nº por línea")
            leerFilas x y []

getFloat::IO Float
getFloat = do   line <- getLine
                return (read line::Float)

leerFilas::Int -> Int -> [[Float]] -> IO Matriz
leerFilas x y m = do    f <- leerLinea y []
                        if x == 1 then return $M (m++[f])
                            else leerFilas (x-1) y (m++[f])

leerLinea::Int -> [Float] -> IO [Float]
leerLinea y ys = do e <- getFloat     
                    if y == 1 then return (ys ++ [e])
                    else leerLinea (y-1) (ys++[e])

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
formatea fileIn fileOut n = do print("ERROR 404: SOLUTION NOT FOUND")--Pendiente de implementar
