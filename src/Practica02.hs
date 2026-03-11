module Practica02 where

--Sintaxis de la logica proposicional
data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
            deriving (Eq)

instance Show Prop where
                    show (Cons True) = "⊤"
                    show (Cons False) = "⊥"
                    show (Var p) = p
                    show (Not p) = "¬" ++ show p
                    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
                    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
                    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
                    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"


p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

type Estado = [String]

--EJERCICIOS
--Ejercicio 1
variables :: Prop -> [String]
variables (Cons _) = []
variables (Var v) = [v]
variables (Not a) = variables a
variables (Or a b) = union (variables a) (variables b)
variables (And a b) = union (variables a) (variables b)
variables (Impl a b) = union (variables a) (variables b)
variables (Syss a b) = union (variables a) (variables b)

-- Auxiliar para crear un conjunto
union :: [String] -> [String] -> [String]
union ys [] = ys
union ys (x:xs)
    | pertenece x ys = union ys xs
    | otherwise      = union (ys ++ [x]) xs

--Ejercicio 2
interpretacion :: Prop -> Estado -> Bool
interpretacion (Cons True) _ = True
interpretacion (Cons False) _ = False
interpretacion (Var v) state = pertenece v state
interpretacion (Not a) state = not (interpretacion a state)
interpretacion (Or a b) state = interpretacion a state || interpretacion b state
interpretacion (And a b) state = interpretacion a state && interpretacion b state
interpretacion (Impl a b) state = interpretacion (Or (Not a) b) state
interpretacion (Syss a b) state = interpretacion (Impl a b) state == interpretacion (Impl b a) state

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece a (x:xs) = (a == x) || pertenece a xs

--Ejercicio 3
estadosPosibles :: Prop -> [Estado]
estadosPosibles (Cons _) = []
estadosPosibles prop = conjPotencia (variables prop)

--Ejercicio 4
modelos :: Prop -> [Estado]
modelos prop = tester prop (estadosPosibles prop)

tester :: Prop -> [Estado] -> [Estado]
tester prop [] = []
tester prop (x:xs) = 
    if interpretacion prop x 
        then x : tester prop xs 
        else tester prop xs


--Ejercicio 5
sonEquivalentes :: Prop -> Prop -> Bool
sonEquivalentes p1 p2 = contencion (modelos p1) (modelos p2)  && contencion (modelos p2) (modelos p1)

--Nos dice si una lista de estados esta contenida en otra.
contencion :: [Estado] -> [Estado] -> Bool
contencion [] _ = True
contencion (x:xs) l2 = elemf x l2 && contencion xs l2

--Nos dice si un string esta contenido en otro
eqCont :: [String] -> [String] -> Bool
eqCont [] _ = True
eqCont (x:xs) y = elem x y && eqCont xs y

-- Nos dice si dos listas de cadenas son iguales 
eqf :: [String] -> [String] -> Bool
eqf x y = eqCont x y && eqCont y x

--Nos dice si un etado está en una lista de estados.
elemf :: Estado -> [Estado] -> Bool
elemf [] [] = True
elemf x [] = False
elemf x (y:ys) = eqf x y || elemf x ys

--Ejercicio 6 
tautologia :: Prop -> Bool
tautologia p = length (modelos p) == length (estadosPosibles p)

--Ejercicio 7
contradiccion :: Prop -> Bool
contradiccion p = length (modelos p) == 0

--Ejercicio 8
consecuenciaLogica :: [Prop] -> Prop -> Bool
consecuenciaLogica p c = not (satifacible ( p++[Not c] ) (conjPotencia (totalVar p)) )
{- Usaremos el principio de refutacion, se da la concecuencia syss p++[Not c] es insatisfacible
Asi que usamos el not (pues si es satisfacible, entonces la concecuencia no se da) y viciversa -}

-- funcion que devuelve la lista de variariables totales(sin repeticion)
totalVar :: [Prop] -> [String]
totalVar [] = []
totalVar (x:xs) = union (variables x) (totalVar xs)

--Funcion que devuelve si la lista de propociciones es satisfacible dado un estado
interp :: [Prop] -> Estado -> Bool
interp [] e = True
interp (x:xs) e = interpretacion x e && interp xs e

--Función que checa si en alguno de los estados, todas las formulas son verdaderas
satifacible :: [Prop] -> [Estado] -> Bool
satifacible p [] = False -- Pues para que sea satisfaciblem lguno de los estados debe cumplir
satifacible p (x:xs) = interp p x || satifacible p xs

--Funcion auxiliar
conjPotencia :: [a] -> [[a]]
conjPotencia [] = [[]]
conjPotencia (x:xs) = [(x:ys) | ys <- conjPotencia xs] ++ conjPotencia xs