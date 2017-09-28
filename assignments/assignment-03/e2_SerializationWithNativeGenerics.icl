module e2_SerializationWithNativeGenerics

/*
 * Thomas Churchman s4206606
 *
 * Based on:
 * Definitions for assignment 3 in AFP 2017
 * Kind indexed gennerics
 * Pieter Koopman, pieter@cs.ru.nl
 * September 2017
*/

import StdEnv, StdMaybe, StdGeneric

:: Write a :== a [String] -> [String]
:: Read a  :== [String] -> Maybe (a,[String])

class serialize a where
    write :: a [String] -> [String]
    read  :: [String] -> Maybe (a,[String])

generic gWrite a :: a [String] -> [String]
generic gRead a  :: [String] -> Maybe (a, [String])

// Serialization for basic types:
gWrite{|Int|} i c = [toString i:c]
gRead{|Int|} [s:r]
    # i = toInt s
    | s == toString i
        = Just (i,r)
        = Nothing
gRead{|Int|} _ = Nothing

gWrite{|Bool|} b c = [toString b:c]
gRead{|Bool|} ["True":r] = Just (True,r)
gRead{|Bool|} ["False":r] = Just (False,r)
gRead{|Bool|} _ = Nothing

instance serialize Bool where
    write b c = gWrite{|*|} b c
    read s    = gRead{|*|} s

instance serialize Int where
    write i c = gWrite{|*|} i c
    read s    = gRead{|*|} s

// Serialization for generic types:
gWrite{|UNIT|} UNIT r = r
gRead{|UNIT|} r = Just (UNIT, r)

gWrite{|EITHER|} writex _      (LEFT  x) r = writex x r
gWrite{|EITHER|} _      writey (RIGHT y) r = writey y r
gRead{|EITHER|} readx ready r
    # x` = readx r
    # y` = ready r
    = case x` of
        Just (x, r2) = Just (LEFT x, r2)
        otherwise = case y` of
            Just (y, r3) = Just (RIGHT y, r3)
            Nothing = Nothing
            
gWrite{|PAIR|} writex writey (PAIR x y) r = writex x (writey y r)
gRead{|PAIR|} readx ready r = case readx r of
    Just (x, r2) = case ready r2 of
        Just (y, r3) = Just (PAIR x y, r3)
        Nothing = Nothing
    Nothing = Nothing

// TODO: Make it so that constructors/objects without arguments do not print parentheses
gWrite{|CONS|} writex (CONS x) r 
    = ["(" : writex x [")" : r]]
gRead{|CONS|} readx ["(" : r] = case readx r of
    Just (x, [")" : r2]) = Just (CONS x, r2)
    _                    = Nothing
gRead{|CONS|} _ _ = Nothing
    
gWrite{|OBJECT|} writex (OBJECT x) r
    = ["(" : writex x [")" : r]]
gRead{|OBJECT|} readx ["(" : r] = case readx r of
    Just (x, [")" : r2]) = Just (OBJECT x, r2)
    _                    = Nothing
gRead{|OBJECT|} _ _ = Nothing

// Lists serialization
derive gWrite []
derive gRead []

instance serialize [a] | gWrite{|*|} a & gRead{|*|} a where
    write a s = gWrite{|*|} a s
    read  s   = gRead{|*|} s

// Binary tree type
:: Bin a = Leaf | Bin (Bin a) a (Bin a)

instance == (Bin a) | == a where
    (==) Leaf Leaf = True
    (==) (Bin l a r) (Bin k b s) = l == k && a == b && r == s
    (==) _ _ = False

derive gWrite Bin
derive gRead Bin

instance serialize (Bin a) | gWrite{|*|} a & gRead{|*|} a where
	write b s = gWrite{|*|} b s
	read    s = gRead{|*|} s

// Coin type
:: Coin = Head | Tail

instance == Coin where
    (==) Head Head = True
    (==) Tail Tail = True
    (==) _    _    = False

derive gWrite Coin
derive gRead Coin

instance serialize Coin where
	write c s = gWrite{|*|} c s
	read    s = gRead{|*|} s

/*
	Define a special purpose version for this type that writes and reads
	the value (7,True) as ["(","7",",","True",")"]
*/

gWrite{|(,)|} writex writey (x, y) s = ["(" : writex x ["," : writey y [")" : s]]]
gRead{|(,)|} readx ready ["(", x, ",", y, ")" : s] =
    case readx [x] of
        Nothing = Nothing
        Just (x`, []) = case ready [y] of
   	        Nothing = Nothing
   	        Just (y`, []) = Just ((x`, y`), s)
gRead{|(,)|} _ _ _ = Nothing

//derive gWrite (,) 
//derive gRead (,)

instance serialize (a,b) | gWrite{|*|} a & gWrite{|*|} b & gRead{|*|} a & gRead{|*|} b where
	write t s = gWrite{|*|} t s
	read r = gRead{|*|} r

// ---
// output looks nice if compiled with "Basic Values Only" for console in project options
Start = 
    [test True
    ,test False
    ,test 0
    ,test 123
    ,test -36
    ,test [42]
    ,test [0..4]
    ,test [[True],[]]
    ,test [[[1]],[[2],[3,4]],[[]]]
    ,test (Bin Leaf True Leaf)
    ,test [Bin (Bin Leaf [1] Leaf) [2] (Bin Leaf [3] (Bin Leaf [4,5] Leaf))]
    ,test [Bin (Bin Leaf [1] Leaf) [2] (Bin Leaf [3] (Bin (Bin Leaf [4,5] Leaf) [6,7] (Bin Leaf [8,9] Leaf)))]
    ,test Head
    ,test Tail
    ,test (7,True)
    ,test (Head,(7,[Tail]))
    ,["End of the tests.\n"]
    ]

test :: a -> [String] | serialize, == a
test a = 
    (if (isJust r)
        (if (fst jr == a)
            (if (isEmpty (tl (snd jr)))
                ["Oke"]
                ["Not all input is consumed! ":snd jr])
            ["Wrong result: ":write (fst jr) []])
        ["read result is Nothing"]
    ) ++ [", write produces: ": s]
    where
        s = write a ["\n"]
        r = read s
        jr = fromJust r
