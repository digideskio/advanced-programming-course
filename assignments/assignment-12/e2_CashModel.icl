implementation module e2_CashModel/* * Jordi Riemens    s4243064 * Thomas Churchman s4206606 * * Based on: * Pieter Koopman, pieter@cs.ru.nl * Skeleton for Advanced Programming, week 12, 2017 * A simple state model for an automated cash register */import StdEnv, GenEq, e1_Gastjeclass euro a :: a -> Euroinstance euro Product where	euro Pizza = euro (4,99)	euro Beer  = euro (0,65)	euro _     = euro 1instance euro Int where euro e = {euro = e, cent = 0}instance euro (Int, Int) where euro (e,c) = {euro = e, cent = c}instance euro [e] | euro e where euro l = sum (map euro l)instance euro Euro where euro e = einstance + Euro where	+ x y = {euro = c / 100, cent = (abs c) rem 100} where		c = (x.euro + y.euro) * 100 + sign x.euro * x.cent + sign y.euro * y.centinstance - Euro where	- x y = {euro = c / 100, cent = (abs c) rem 100} where		c = (x.euro - y.euro) * 100 + sign x.euro * x.cent - sign y.euro * y.centinstance zero Euro where zero = {euro = 0, cent = 0}derive gEq Euro,  Productinstance == Product where (==) p q = p === qinstance == Euro where (==) p q = p === qinstance ~ Euro where ~ e = {e & euro = ~e.euro}model :: [Product] Action -> ([Product],[Euro])model list (Add p) = ([p:list],[euro p])model list (Rem p) | isMember p list = (removeMember p list,[~ (euro p)]) = (list,[])model list Pay = ([],[euro list])pUpper :: Char -> BoolpUpper c = not (c == toUpper c)Start = ["pUpper1: "] ++ test (pUpper For ['a'..'z'])