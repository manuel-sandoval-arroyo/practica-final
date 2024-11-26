data Var = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z deriving (Show, Eq, Ord)

data Formula = Atom Var
               |Neg Formula
               |Formula :&: Formula
               |Formula :|: Formula
               |Formula :=>: Formula
               |Formula :<=>: Formula deriving (Show, Eq, Ord)

infixl 9 :&:
infixl 9 :|:
infixl 7 :=>:
infixl 8 :<=>:

-------------------- EJERCICIO 1 --------------------
conjunto :: Eq a => [a] -> [a]
conjunto [] = []
conjunto (x : xs) = x : conjunto [y | y <- xs, y /= x]

variables :: Formula -> [Var]
variables (Atom v) = [v]
variables (Neg f) = variables f
variables (f1 :&: f2) = conjunto(variables f1 ++ variables f2)
variables (f1 :|: f2) = conjunto(variables f1 ++ variables f2)
variables (f1 :=>: f2) = conjunto(variables f1 ++ variables f2)
variables (f1 :<=>: f2) = conjunto(variables f1 ++ variables f2)
-----------------------------------------------------

-------------------- EJERCICIO 2 --------------------
negacion :: Formula -> Formula
negacion (Atom v) = Neg (Atom v)
negacion (Neg f) = f
negacion (f1 :&: f2) = negacion f1 :|: negacion f2
negacion (f1 :|: f2) = negacion f1 :&: negacion f2
negacion (f1 :=>: f2) = negacion f1 :&: negacion f2
negacion (f1 :<=>: f2) = negacion f1 :<=>: negacion f2
-----------------------------------------------------

-------------------- EJERCICIO 3 --------------------
equivalencia :: Formula -> Formula
equivalencia (Atom v) = Atom v
equivalencia (Neg f) = negacion f
equivalencia (f1 :&: f2) = f1 :&: f2
equivalencia (f1 :|: f2) = f1 :|: f2
equivalencia (f1 :=>: f2) = negacion f1 :|: f2
equivalencia (f1 :<=>: f2) = (f1 :=>: f2) :&: (f2 :=>: f1)

-----------------------------------------------------

-------------------- EJERCICIO 4 --------------------
varValue :: Var -> [(Var,Bool)] -> Bool
varValue v ((var, boolean):xs) = if v == var 
    then boolean 
    else varValue v xs

interpretacion :: Formula -> [(Var,Bool)] -> Bool
interpretacion (Atom v) xs = varValue v xs
interpretacion (Neg f) xs = not (interpretacion f xs)
interpretacion (f1 :&: f2) xs = interpretacion f1 xs && interpretacion f2 xs
interpretacion (f1 :|: f2) xs = interpretacion f1 xs || interpretacion f2 xs
interpretacion (f1 :=>: f2) xs = not (interpretacion f1 xs) || interpretacion f2 xs
interpretacion (f1 :<=>: f2) xs = interpretacion f1 xs == interpretacion f2 xs

-----------------------------------------------------

-------------------- EJERCICIO 5 --------------------
combinaciones :: Formula -> [[(Var,Bool)]]
combinaciones f = combinacionesCreator3000 (variables f)

combinacionesCreator3000 :: [Var] -> [[(Var,Bool)]]
combinacionesCreator3000 [] = [[]]
combinacionesCreator3000 (v:vs) = [(v, b) : resto | b <- [False, True], resto <- combinacionesCreator3000 vs]
-----------------------------------------------------

-------------------- EJERCICIO 6 --------------------

tablaDeVerdad :: Formula -> [([(Var,Bool)],Bool)]
tablaDeVerdad f = [(c, interpretacion f c) | c <- combinaciones f]
-----------------------------------------------------



