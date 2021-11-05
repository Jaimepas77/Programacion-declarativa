--Lab4
--Autor: Jaime Pastrana García

--1
data Punto = Pos Int Int deriving Show
data Direccion = Arriba | Abajo | Izquierda | Derecha deriving (Show, Ord, Eq)

-- a) con ajuste de patrones
mueve::Punto -> Direccion -> Punto
mueve (Pos x y) Arriba = Pos ((x+1) `mod` 101) y
mueve (Pos x y) Abajo = Pos ((x-1) `mod` 101) y
mueve (Pos x y) Izquierda = Pos x ((y-1) `mod` 101)
mueve (Pos x y) Derecha = Pos x ((y+1) `mod` 101)

-- b)
destino::Punto -> [Direccion] -> Punto
destino (Pos x y) ms = foldl mueve (Pos x y) ms

-- c)
trayectoria::Punto -> [Direccion] -> [Punto]
trayectoria (Pos x y) ms = reverse $foldl (\ps m -> (mueve (head ps) m):ps) [(Pos x y)] ms

--2
data Nat = Cero | Suc Nat deriving (Eq, Ord)

--a)
instance Num Nat where
    (+) Cero b = b
    (+)(Suc a) b = Suc(a + b)

    (*)(Suc Cero) b = b
    (*)(Suc a) b = (+) b (a * b)

--b)
natToInt::Nat -> Int
natToInt Cero = 0
natToInt (Suc a) = 1 + natToInt a

--c)
instance Show Nat where
    show a = show $natToInt a

--3
data Complejos = Compl Int Int deriving Eq
instance Show Complejos where
    show (Compl a b) = if b>=0 then show a ++ "+" ++ show b ++ "i"
                            else show a ++ show b ++ "i"

instance Num Complejos where
    (+) (Compl a b) (Compl c d) = Compl (a+c) (b+d)
    (-) (Compl a b) (Compl c d) = Compl (a-c) (b-d)
    (*) (Compl a b) (Compl c d) = Compl (a*c - c*d) (a*d + b*c)
    
--instance Fractional Complejos where
--    (/) (Compl a b) (Compl c d) = Compl ((a*c+b*d)/(c*c + d*d)) ((b*c - a*d)/(c*c + d*d))

--4
class Medible a where
    medida::a -> Int

instance Medible Bool where
    medida True = 1
    medida False = 0

instance Medible a => Medible [a] where
    medida [] = 0
    medida (x:xs) = medida x + medida xs

instance (Medible a, Medible b) => Medible (a, b) where
    medida (a, b) = medida a + medida b

instance Medible Nat where
    medida = natToInt

instance Medible Complejos where
    medida (Compl a b) = max (abs a) (abs b)
    
instance Medible Char where --La más interesante para mi gusto
    medida 'a' = 1
    medida 'A' = 1
    medida x = if x > 'A' then 1 + medida (pred x) else 0