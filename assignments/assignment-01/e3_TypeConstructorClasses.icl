/* Jordi Riemens    s4243064
 * Thomas Churchman s4206606
 */

module e3_TypeConstructorClasses

import StdEnv

// Define binary trees
:: Bin a = Leaf | Bin (Bin a) a (Bin a)

// Define the container type constructor class
class Container t where
    Cinsert   :: a (t a) -> t a      | < a
    Ccontains :: a (t a) -> Bool     | <, Eq a
    Cshow     ::   (t a) -> [String] | toString a
    Cnew      :: t a

/*-------------------------------------------------*/

// List instance for the container type constructor class
instance Container [] where
    Cinsert a l   = [a : l]
    Ccontains a l = isMember a l
    /* Why does the following not work? 
     * Ccontains = isMember
     * "Error: Ccontains used with wrong arity"
     * A quick scan through the language report does not give an answer.
     */
    Cshow l       = map toString l
    Cnew          = []

instance Container Bin where
    Cinsert n (Bin l e r)
      | n < e     = Bin (Cinsert n l) e r
      | otherwise = Bin l e (Cinsert n r)
    Cinsert n Leaf = Bin Leaf n Leaf
    Ccontains n (Bin l e r)
      | n == e = True
      | n < e  = Ccontains n l
      | n > e  = Ccontains n r
    Ccontains _ Leaf = False
    Cshow (Bin l e r) = ["Bin"] ++ (Cshow l) ++ [toString e] ++ (Cshow r)
    Cshow Leaf = ["Leaf"]
    Cnew = Leaf

/* Define some int containers */
c1 :: [Int]
//c1 :: Bin Int
c1 = Cnew
c2 = Cinsert 6 c1
c3 = Cinsert 3 c2
c4 = Cinsert 6 c3
c5 = Cinsert 3 c4

// Define some tests
t1 = (Ccontains 3 c, Cshow c) where c = c1
t2 = (Ccontains 3 c, Cshow c) where c = c2
t3 = (Ccontains 3 c, Cshow c) where c = c3
t4 = (Ccontains 6 c, Cshow c) where c = c3
t5 = (Ccontains 3 c, Cshow c) where c = c4
t6 = (Ccontains 6 c, Cshow c) where c = c5

Start = [t1, t2, t3, t4, t5, t6]
