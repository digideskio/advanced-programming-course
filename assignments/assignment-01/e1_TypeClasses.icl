/* Jordi Riemens    s4243064
 * Thomas Churchman s4206606
 */

module e1_TypeClasses

import StdEnv
import StdMaybe

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

Start = [test True, test 0]
