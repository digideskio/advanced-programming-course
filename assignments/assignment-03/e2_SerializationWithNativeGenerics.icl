module e2_SerializationWithNativeGenerics

/*
 * Jordi Riemens    s4243064
 * Thomas Churchman s4206606
 *
 * Based on:
 * Definitions for assignment 3 in AFP 2017
 * Kind indexed generics
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

gWrite{|CONS of c|} writex (CONS x) r 
    # x` = writex x []
    = case x` of
        [] = [c.gcd_name : x`] ++ r
        _ = ["(" : c.gcd_name : writex x [")" : r]]
gRead{|CONS of c|} readx ["(" : name : r] 
    | name == c.gcd_name = case readx r of
        Just (x, [")" : r2]) = Just (CONS x, r2)
        _                    = Nothing
    | otherwise = Nothing
gRead{|CONS of c|} readx [name : r] 
    | name == c.gcd_name = case readx r of
        Just (x, r2) = Just (CONS x, r2)
        _            = Nothing
    | otherwise = Nothing
gRead{|CONS|} _ _ = Nothing
    
gWrite{|OBJECT|} writex (OBJECT x) r = writex x r
gRead{|OBJECT|} readx r = case readx r of
    Just (x, r2) = Just (OBJECT x, r2)
    _            = Nothing

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
gRead{|(,)|} readx ready ["(" : xyr] =
    case readx xyr of
        Nothing = Nothing
        Just (x, ["," : yr]) = case ready yr of
            Nothing = Nothing
            Just (y, [")" : r]) = Just ((x,y), r)
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
        
/*
 * Output:
 * Oke, write produces: True
 * Oke, write produces: False
 * Oke, write produces: 0
 * Oke, write produces: 123
 * Oke, write produces: -36
 * Oke, write produces: (_Cons42_Nil)
 * Oke, write produces: (_Cons0(_Cons1(_Cons2(_Cons3(_Cons4_Nil)))))
 * Oke, write produces: (_Cons(_ConsTrue_Nil)(_Cons_Nil_Nil))
 * Oke, write produces: (_Cons(_Cons(_Cons1_Nil)_Nil)(_Cons(_Cons(_Cons2_Nil)(_Cons(_Cons3(_Cons4_Nil))_Nil))(_Cons(_Cons_Nil_Nil)_Nil)))
 * Oke, write produces: (BinLeafTrueLeaf)
 * Oke, write produces: (_Cons(Bin(BinLeaf(_Cons1_Nil)Leaf)(_Cons2_Nil)(BinLeaf(_Cons3_Nil)(BinLeaf(_Cons4(_Cons5_Nil))Leaf)))_Nil)
 * Oke, write produces: (_Cons(Bin(BinLeaf(_Cons1_Nil)Leaf)(_Cons2_Nil)(BinLeaf(_Cons3_Nil)(Bin(BinLeaf(_Cons4(_Cons5_Nil))Leaf)(_Cons6(_Cons7_Nil))(BinLeaf(_Cons8(_Cons9_Nil))Leaf))))_Nil)
 * Oke, write produces: Head
 * Oke, write produces: Tail
 * Oke, write produces: (7,True)
 * Oke, write produces: (Head,(7,(_ConsTail_Nil)))
 * End of the tests.
 */
