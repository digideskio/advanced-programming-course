/* Jordi Riemens    s4243064
 * Thomas Churchman s4206606
 */
 
module e2_GenericSerialization

import StdEnv

// Define generic data types
:: UNIT       = UNIT
:: EITHER a b = LEFT a | RIGHT b
:: PAIR a b   = PAIR a b
:: CONS a     = CONS String a

// Define binary trees type
:: Bin a    = Leaf | Bin (Bin a) a (Bin a)

// Define generic representation types:
:: ListG a :== EITHER (CONS UNIT) (CONS (PAIR a [a]))
:: BinG a  :== EITHER (CONS UNIT) (CONS (PAIR (Bin a) (PAIR a (Bin a))))

// Define transformation functions
fromList :: [a] -> ListG a
fromList []     = LEFT (CONS "[]" UNIT)
fromList [x:xs] = RIGHT (CONS "[x:xs]" (PAIR x xs))

toList :: (ListG a) -> [a]
toList (LEFT _)                     = []
toList (RIGHT (CONS _ (PAIR x xs))) = [x:xs]

fromBin :: (Bin a) -> BinG a
fromBin Leaf          = LEFT (CONS "Leaf" UNIT)
fromBin (Bin b1 b b2) = RIGHT (CONS "Bin" (PAIR b1 (PAIR b b2)))

toBin :: (BinG a) -> Bin a
toBin (LEFT _)                               = Leaf
toBin (RIGHT (CONS _ (PAIR b1 (PAIR b b2)))) = Bin b1 b b2

Start = toBin (fromBin (Bin Leaf 0 (Bin (Bin Leaf 4 Leaf) 2 Leaf)))
