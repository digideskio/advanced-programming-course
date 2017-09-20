/* Jordi Riemens    s4243064
 * Thomas Churchman s4206606
 */
 
module e2_GenericSerialization

import StdEnv
import StdMaybe

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


// Serialisation for generic data types
class serialize a where
    write :: a [String] -> [String]
    read  :: [String] -> Maybe (a, [String])

instance serialize UNIT where
    write UNIT r = ["UNIT" : r]
    read ["UNIT" : r] = Just (UNIT, r)
    read _ = Nothing

instance serialize EITHER a b | serialize a, serialize b where
    write (LEFT  a) r = ["LEFT"  : write a] ++ r
    write (RIGHT b) r = ["RIGHT" : write b] ++ r
    read ["LEFT" : r] = case read r of
        Just (a, r2) = Just (LEFT a, r2)
        Nothing = Nothing
    read ["RIGHT" : r] = case read r of
        Just (b, r2) = Just (RIGHT b, r2)
        Nothing = Nothing
    read _ = Nothing

instance serialize PAIR a b | serialize a, serialize b where
    write (PAIR a b) r = ["PAIR" : write a] ++ write b ++ r
    read ["PAIR" : r] = case read r of
        Just (a, r2) = case read r2 of
            Just (b, r3) = Just (PAIR a b, r3)
            Nothing = Nothing
        Nothing = Nothing
    read _ = Nothing

instance serialize CONS a | serialize a where
    write (CONS name a) r = ["CONS" : name : write a] ++ r
    read ["CONS" : name : r] = case read r of
        Just (a, r2) = Just (CONS name a, r2)
        Nothing = Nothing
    read _ = Nothing

// Serialization for regular data types
instance serialize Int where // Int serialization from last week
    write i c = [toString i : c]
    read [s : r] = case maybeInt s of
        Just m = Just (m, r)
        Nothing = Nothing
    where
        maybeInt :: String -> Maybe Int
        maybeInt s
          | p <> 0 = Just p
          | p == 0 && (s == "0") = Just p
          | otherwise = Nothing
        where
            p = toInt s

instance serialize Bin a | serialize a where
    write t r = write (fromBin t) r
	read r = toBin (read r)

instance serialize [a] | serialize a where
    write t r = write (fromList t) r
	read r = toList (read r)


testcase = Bin Leaf 0 (Bin (Bin Leaf 4 Leaf) 2 Leaf)
    
Start = toBin (fromBin (testcase))
