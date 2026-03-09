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
variables (Or a b) = conjunto (variables a) (variables b)
variables (And a b) = conjunto (variables a) (variables b)
variables (Impl a b) = conjunto (variables a) (variables b)
variables (Syss a b) = conjunto (variables a) (variables b)

-- Auxiliar para crear un conjunto
conjunto :: [String] -> [String] -> [String]
conjunto [] l = l
conjunto (x:xs) l = conjunto xs (set x l)

set :: String -> [String] -> [String]
set s [] = [s]
set s (x:xs) = if s == x then x:xs else x : set s xs

--Ejercicio 2
interpretacion :: Prop -> Estado -> Bool
interpretacion (Cons True) _ = True
interpretacion (Cons False) _ = False
interpretacion (Var v) state = pertenece v state
interpretacion (Not v) state = not (interpretacion v state)
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
modelos prop = probador prop (estadosPosibles prop)


probador :: Prop -> [Estado] -> [Estado]
probador prop [] = []
probador prop (x:xs) = if interpretacion prop x then x : probador prop xs else probador prop xs


--Ejercicio 5
sonEquivalentes :: Prop -> Prop -> Bool
sonEquivalentes p1 p2 = comparador (modelos p1) (modelos p2) && comparador (modelos p2) (modelos p1) 

comparador :: [Estado] -> [Estado] -> Bool
comparador [] _ = True
comparador (x:xs) l2 = pertenece x l2 

--Ejercicio 6 
tautologia :: Prop -> Bool
tautologia = undefined

--Ejercicio 7
contradiccion :: Prop -> Bool
contradiccion = undefined

--Ejercicio 8
consecuenciaLogica :: [Prop] -> Prop -> Bool
consecuenciaLogica = undefined


--Funcion auxiliar
conjPotencia :: [a] -> [[a]]
conjPotencia [] = [[]]
conjPotencia (x:xs) = [(x:ys) | ys <- conjPotencia xs] ++ conjPotencia xs