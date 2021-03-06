/* Jordi Riemens    s4243064
 * Thomas Churchman s4206606
 */

module e1_TypeClasses

import StdEnv
import StdMaybe

:: Bin a = Leaf | Bin (Bin a) a (Bin a)
:: Rose a = Rose a [Rose a]

instance == (Bin a) | == a where
    (==) Leaf Leaf = True
    (==) (Bin b1 n b2) (Bin b1` n` b2`) = b1 == b1` && n == n` && b2 == b2`
    (==) _ _ = False

instance == (Rose a) | == a where
    (==) (Rose a roses) (Rose a` roses`) = a == a` && roses == roses`

class serialize a where
    write :: a [String] -> [String]
    read  :: [String] -> Maybe (a, [String])
    
instance serialize Bool where
    write b c = [toString b : c]
    read ["True"  : r] = Just (True, r)
    read ["False" : r] = Just (False, r)
    read _ = Nothing
    
test :: a -> (Bool, [String]) | serialize, ==a
test a = (isJust r && fst jr == a && isEmpty (tl (snd jr)), s)
where
    s = write a ["ln"]
    r = read s
    jr = fromJust r
    
/*-------------------------------------------------*/

// Int serialization
instance serialize Int where
    write i c = [toString i : c]
    read [s : r] = if (isJust m) (Just ((fromJust m), r)) Nothing
    where
        m = maybeInt s
        maybeInt :: String -> Maybe Int
        maybeInt s
          | p <> 0 = Just p
          | p == 0 && (s == "0") = Just p
          | otherwise = Nothing
        where
            p = toInt s

// List serialization
// Lists are stored as ["\x01", <repr. el. 1>, ..., <repr. el. n>, "\x04"].
// Nested and/or empty lists supported because of the "\x01" "\x04" demarcation.
instance serialize [a] | serialize a where
    write b c = ["\x01" : reprs] ++ ["\x04" : c]
    where
        reprs = flatten (map (\e -> write e []) b)
    read ["\x01":r] = readList r
    where
        readList ["\x04":r] = Just ([],r)
        readList r
          | (isJust mayberes1) && (isJust mayberes2) =
              Just ([fst result1 : fst result2], snd result2)
          | otherwise = Nothing
        where
            mayberes1 = read r
            result1 = fromJust mayberes1
            mayberes2 = readList (snd result1)
            result2 = fromJust mayberes2
    read _ = Nothing

// Binary tree serialization
instance serialize (Bin a) | serialize a where
    write Leaf c = ["Leaf" : c]
    write (Bin bl n br) c = ["Bin" : write bl [] ++ write n [] ++ write br [] ++ c]
    read ["Leaf" : r] = Just (Leaf, r)
    read ["Bin" : r] = if (isJust br) (Just (Bin (fst (fromJust bl)) (fst (fromJust n)) (fst (fromJust br)), snd (fromJust br))) Nothing
    where
        bl = read r
        n  = if (isJust bl) (read (snd (fromJust bl))) Nothing
        br = if (isJust n) (read (snd (fromJust n))) Nothing
    read _ = Nothing

// Rose serialization
instance serialize (Rose a) | serialize a where
    write (Rose a roses) c = write a [] ++ write roses [] ++ c
    read r = if (isJust roses) (Just (Rose (fst (fromJust a)) (fst (fromJust roses)), snd (fromJust roses))) Nothing
    where
        a = read r
        roses = if (isJust a) (read (snd (fromJust a))) Nothing

Start = [
         test True,
         test False,
         test [True],
         test [True, False, False],
         test [1, 2, 3],
         test [[], [[True, False], [False], []]],
         test (Bin (Bin Leaf 2 Leaf) 5 Leaf),
         test (Rose 5 [Rose 6 [Rose 9 []], Rose 7 []])
        ]
