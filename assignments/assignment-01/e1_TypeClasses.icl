/* Jordi Riemens    s4243064
 * Thomas Churchman s4206606
 */

module e1_TypeClasses

import StdEnv
import StdMaybe

:: Bin a = Leaf | Bin (Bin a) a (Bin a)
:: Rose a = Rose a [Rose a]

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
  where reprs = flatten (map (\e -> write e []) b)
  read ["\x01":r] = readList r
  where readList ["\x04":r] = Just ([],r)
        readList r
          | (isJust mayberes1) && (isJust mayberes2) =
              Just ([fst result1 : fst result2], snd result2)
          | otherwise = Nothing
        where mayberes1 = read r
              result1 = fromJust mayberes1
              mayberes2 = readList (snd result1)
              result2 = fromJust mayberes2
  read _ = Nothing

Start = [
         test True,
         test False,
         test [True],
         test [True, False, False],
         test [1, 2, 3],
         test [[], [[True, False], [False], []]]
        ]
