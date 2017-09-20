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

// Define Bin equality for testing
instance == (Bin a) | == a where 
  (==) Leaf Leaf = True
  (==) (Bin l a r) (Bin k b s) = l == k && a == b && r == s
  (==) _ _ = False


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

// 2.1
/*
instance serialize UNIT where
    write UNIT r = ["UNIT" : r]
    read ["UNIT" : r] = Just (UNIT, r)
    read _ = Nothing

instance serialize (EITHER a b) | serialize a & serialize b where
    write (LEFT  a) r = ["LEFT"  : write a r]
    write (RIGHT b) r = ["RIGHT" : write b r]
    read ["LEFT" : r] = case read r of
        Just (a, r2) = Just (LEFT a, r2)
        Nothing = Nothing
    read ["RIGHT" : r] = case read r of
        Just (b, r2) = Just (RIGHT b, r2)
        Nothing = Nothing
    read _ = Nothing

instance serialize (PAIR a b) | serialize a & serialize b where
    write (PAIR a b) r = ["PAIR" : write a (write b r)]
    read ["PAIR" : r] = case read r of
        Just (a, r2) = case read r2 of
            Just (b, r3) = Just (PAIR a b, r3)
            Nothing = Nothing
        Nothing = Nothing
    read _ = Nothing

instance serialize (CONS a) | serialize a where
    write (CONS name a) r = ["CONS" : name : write a r]
    read ["CONS" : name : r] = case read r of
        Just (a, r2) = Just (CONS name a, r2)
        Nothing = Nothing
    read _ = Nothing
*/

// 2.2
instance serialize UNIT where
    write UNIT r = r
    read r = Just (UNIT, r)

instance serialize (EITHER (CONS a) (CONS b)) | serialize a & serialize b where
    write (LEFT  a) r = write a r
    write (RIGHT b) r = write b r
    read r 
    	# a` = read r
    	# b` = read r
    	= case a` of
            Just (a, r2) = Just (LEFT a, r2)
            otherwise = case b` of
                Just (b, r3) = Just (RIGHT b, r3)
                Nothing = Nothing

instance serialize (PAIR a b) | serialize a & serialize b where
    write (PAIR a b) r = write a (write b r)
    read r = case read r of
        Just (a, r2) = case read r2 of
            Just (b, r3) = Just (PAIR a b, r3)
            Nothing = Nothing
        Nothing = Nothing

instance serialize (CONS a) | serialize a where
    write (CONS name a) r = ["(" : name : write a [")" : r]]
    read ["(" : name : r] = case read r of
        Just (a, [")" : r2]) = Just (CONS name a, r2)
        _ = Nothing
    read _ = Nothing

// Serialization for regular data types
instance serialize Bool where // from Blackboard
    write b c = [toString b:c]
    read ["True":r]  = Just (True,r)
    read ["False":r] = Just (False,r)
    read _ = Nothing

instance serialize Int where // from Blackboard
    write i c = [toString i:c]
    read [s:r]
        # i = toInt s
        | s == toString i
          = Just (i,r)
          = Nothing
    read _ = Nothing

instance serialize (Bin a) | serialize a where
    write t r = write (fromBin t) r
	read r = case read r of
	    Just (t, r) = Just (toBin t, r)
		Nothing = Nothing

instance serialize [a] | serialize a where
    write l r = write (fromList l) r
	read r = case read r of
	    Just (l, r) = Just (toList l, r)
		Nothing = Nothing


// Tests from Blackboard
Start = 
  [test True
  ,test False
  ,test 0
  ,test 123
  ,test -36
  ,test [42]
  ,test [0..4]
  ,test [[True],[]]
  ,test (Bin Leaf True Leaf)
  ,test [Bin (Bin Leaf [1] Leaf) [2] (Bin Leaf [3] (Bin Leaf [4,5] Leaf))]
  ,test [Bin (Bin Leaf [1] Leaf) [2] (Bin Leaf [3] (Bin (Bin Leaf [4,5] Leaf) [6,7] (Bin Leaf [8,9] Leaf)))]
  ]

test :: a -> ([String],[String]) | serialize, == a
test a = 
  (if (isJust r)
    (if (fst jr == a)
      (if (isEmpty (tl (snd jr)))
        ["Oke "]
        ["Fail: not all input is consumed! ":snd jr])
      ["Fail: Wrong result ":write (fst jr) []])
    ["Fail: read result is Nothing "]
  , ["write produces ": s]
  )
  where
    s = write a ["\n"]
    r = read s
    jr = fromJust r

