module e1_KindIndexGenericSerialization

/*
 * Thomas Churchman s4206606
 *
 * Based on:
 * Definitions for assignment 3 in AFP 2017
 * Kind indexed gennerics
 * Pieter Koopman, pieter@cs.ru.nl
 * September 2017
*/

import StdEnv, StdMaybe

:: Write a :== a [String] -> [String]
:: Read a  :== [String] -> Maybe (a,[String])

// use this as serialize0 for kind *
class serialize a where
    write :: a [String] -> [String]
    read  :: [String] -> Maybe (a,[String])

// Serialization for basic types:
instance serialize Bool where
    write b c = [toString b:c]
    read ["True":r] = Just (True,r)
    read ["False":r] = Just (False,r)
    read _ = Nothing

instance serialize Int where
    write i c = [toString i:c]
    read [s:r]
        # i = toInt s
        | s == toString i
            = Just (i,r)
            = Nothing
    read _ = Nothing

// Generic types
:: UNIT       = UNIT
:: EITHER a b = LEFT a | RIGHT b
:: PAIR   a b = PAIR a b
:: CONS   a   = CONS String a

// Generic serialization
class serialize1 t where
    write1 :: (Write a) (t a) [String] -> [String]
    read1 :: (Read a) [String] -> Maybe (t a,[String])
  
class serialize2 t where
    write2 :: (Write a) (Write b) (t a b) [String] -> [String]
    read2 :: (Read a) (Read b) [String] -> Maybe (t a b,[String])
  
instance serialize UNIT where
    write UNIT r = r
    read r = Just (UNIT, r)
  
instance serialize2 EITHER where
    write2 writex _      (LEFT  x) r = writex x r
    write2 _      writey (RIGHT y) r = writey y r
    read2 readx ready r
        # x` = readx r
        # y` = ready r
        = case x` of
            Just (x, r2) = Just (LEFT x, r2)
            otherwise = case y` of
                Just (y, r3) = Just (RIGHT y, r3)
                Nothing = Nothing
  
instance serialize2 PAIR where
    write2 writex writey (PAIR x y) r = writex x (writey y r)
    read2 readx ready r = case readx r of
        Just (x, r2) = case ready r2 of
            Just (y, r3) = Just (PAIR x y, r3)
            Nothing = Nothing
        Nothing = Nothing
    
// CONS serializer with a hacky way to prevent printing of
// parentheses around constructors without arguments.
instance serialize1 CONS where
    write1 writex (CONS name x) r 
        #x` = writex x []
        = case x` of
            [] = [name : x`] ++ r
            _  = ["(" : name : writex x [")" : r]]
    read1 readx ["(" : name : r] = case readx r of
        Just (x, [")" : r2]) = Just (CONS name x, r2)
        _                    = Nothing
    read1 readx [name : r] = case readx r of
        Just (x, r2) = Just (CONS name x, r2)
        _            = Nothing
    read1 _ _ = Nothing

// Generic representation of lists
:: ListG a :== EITHER (CONS UNIT) (CONS (PAIR a [a]))

fromList :: [a] -> ListG a
fromList []  = LEFT  (CONS NilString  UNIT)
fromList [a:x] = RIGHT (CONS ConsString (PAIR a x))

toList :: (ListG a) -> [a]
toList (LEFT  (CONS NilString  UNIT)) = []
toList (RIGHT (CONS ConsString (PAIR a x))) = [a:x]

NilString :== "Nil"
ConsString :== "Cons"

instance serialize [a] | serialize a where
    write a s = write2 (write1 write) (write1 (write2 write write)) (fromList a) s
    read  s   = 
        case read2 (read1 read) (read1 (read2 read read)) s of
            Just (l, s) = Just (toList l, s)
            Nothing     = Nothing

// Binary tree type
:: Bin a = Leaf | Bin (Bin a) a (Bin a)

:: BinG a :== EITHER (CONS UNIT) (CONS (PAIR (Bin a) (PAIR a (Bin a))))

fromBin :: (Bin a) -> BinG a
fromBin Leaf = LEFT (CONS LeafString UNIT)
fromBin (Bin l a r) = RIGHT (CONS BinString (PAIR l (PAIR a r)))

toBin :: (BinG a) -> Bin a
toBin (LEFT (CONS _ UNIT)) = Leaf
toBin (RIGHT (CONS _ (PAIR l (PAIR a r)))) = Bin l a r

LeafString :== "Leaf"
BinString :== "Bin"

instance == (Bin a) | == a where
    (==) Leaf Leaf = True
    (==) (Bin l a r) (Bin k b s) = l == k && a == b && r == s
    (==) _ _ = False

instance serialize (Bin a) | serialize a where
	write b s = write2 (write1 write) (write1 (write2 write (write2 write write))) (fromBin b) s
	read    s =
	    case read2 (read1 read) (read1 (read2 read (read2 read read))) s of
	        Just (b, s) = Just (toBin b, s)
	        Nothing     = Nothing

// Coin type
:: Coin = Head | Tail
:: CoinG :== EITHER (CONS UNIT) (CONS UNIT)

fromCoin :: Coin -> CoinG
fromCoin Head = LEFT (CONS "Head" UNIT)
fromCoin Tail = RIGHT (CONS "Tail" UNIT)

toCoin :: CoinG -> Coin
toCoin (LEFT (CONS _ UNIT)) = Head
toCoin (RIGHT (CONS _ UNIT)) = Tail

instance == Coin where
    (==) Head Head = True
    (==) Tail Tail = True
    (==) _    _    = False

instance serialize Coin where
	write c s = write2 (write1 write) (write1 write) (fromCoin c) s
	read    s = 
	    case read2 (read1 read) (read1 read) s of
	        Just (c, s) = Just (toCoin c, s)
	        Nothing     = Nothing

/*
	Define a special purpose version for this type that writes and reads
	the value (7,True) as ["(","7",",","True",")"]
*/
instance serialize (a,b) | serialize a & serialize b where
	write (x, y) s = ["(" : write x ["," : write y [")" : s]]]
	read  ["(", x, ",", y, ")" : s] =
	    case read [x] of
	        Nothing = Nothing
   	        Just (x`, []) = case read [y] of
   	            Nothing = Nothing
   	            Just (y`, []) = Just ((x`, y`), s)
	read _ = Nothing

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
