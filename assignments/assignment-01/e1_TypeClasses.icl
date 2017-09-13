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

Start = [test True, test False]
