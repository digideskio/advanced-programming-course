module e3_GenericMap

import StdEnv, StdGeneric, GenEq

/*
 * Jordi Riemens    s4243064
 * Thomas Churchman s4206606
 *
 * Based on:
 * Definitions for assignment 3 in AFP 2017
 * Generic map
 * Pieter Koopman, pieter@cs.ru.nl
 * September 2017
*/

// Given generic map definition
generic gMap a b :: a -> b
gMap{|Int|}         x = x
gMap{|Real|}        x = x
gMap{|UNIT|}        x = x
gMap{|PAIR|}   f g (PAIR x y) = PAIR   (f x) (g y) 
gMap{|EITHER|} f g (LEFT x)   = LEFT   (f x)
gMap{|EITHER|} f g (RIGHT x)  = RIGHT  (g x)
gMap{|CONS|}   f   (CONS x)   = CONS   (f x)
gMap{|OBJECT|} f   (OBJECT x) = OBJECT (f x)

// Basic definitions
:: Bin a = Leaf | Bin (Bin a) a (Bin a)

instance == (Bin a) | == a where
    (==) Leaf Leaf = True
    (==) (Bin l a r) (Bin k b s) = l == k && a == b && r == s
    (==) _ _ = False

derive gMap Bin, (,), []
derive gEq  Bin

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)


t = Bin (Bin Leaf 1 Leaf) 2 (Bin (Bin Leaf 3 Leaf) 4 Leaf)
l = [1..7]

testmap1 = gMap{|*->*|} fac t
testmap2 = gMap{|*->*|} (\i -> (i, fac i)) l
testmap3 = gMap{|*->*->*|} fac (gMap{|*->*|} fac) (1,t)
expmap1  = Bin (Bin Leaf 1 Leaf) 2 (Bin (Bin Leaf 6 Leaf) 24 Leaf)
expmap2  = map (\i -> (i, fac i)) l
expmap3  = (1, expmap1)
testeq1 = gEq{|*|} [1,2] [1,2]
testeq2 = not (gEq{|*|} [1,2] [2,3]) // should be false, so prepended with "not"
testeq3 = gEq{|*->*|} (\x y -> not ((x < y) || (y < x))) [1,2] [1,2]
testeq4 = not (gEq{|*->*|} (\x y -> not ((x < y) || (y < x))) [1,2] [2,3]) // should be false, so prepended with "not"

Start = [testmap1 == expmap1, testmap2 == expmap2, testmap3 == expmap3,
         testeq1, testeq2, testeq3, testeq4]
	
