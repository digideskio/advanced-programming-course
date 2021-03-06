module e2_MonadSerialize

import StdEnv, StdMaybe, Monad

/*
 * Jordi Riemens    s4243064
 * Thomas Churchman s4206606
 *
 * Based on:
 * Pieter Koopman, pieter@cs.ru.nl
 * Advanced Programming, week 4, 2017
 * 
 * import StdMaybe from Libraries/StdLib
 * use StdEnv or StdEnv 64
 * use Basic Values Only as console* option for a nicer output.
*/

// ---

:: State s a = S (s -> (Maybe a,s))

unS :: (State s a) -> s -> (Maybe a,s)
unS (S f) = f

instance Functor (State s) where
    fmap f x = S \s0. case unS x s0 of
         (Just x,  s1) = (Just (f x), s1)
         (Nothing, s1) = (Nothing, s1)
instance Applicative (State s) where
    pure a = S \s0. (Just a, s0)
    (<*>) f x = S \s0. case unS f s0 of
         (Just f, s1) = (unS o fmap f) x s1
         (Nothing, s1) = (Nothing, s1)
instance fail (State s) where
    fail = S \s0. (Nothing, s0)
instance Monad (State s) where
    bind x f = S \s0. case unS x s0 of
         (Just x, s1) = (unS o f) x s1
         (Nothing, s1) = (Nothing, s1)
instance OrMonad (State s) where
    (<|>) f g = S \s0. case unS f s0 of
        (Just x, s1) = (Just x, s1)
        (Nothing, _) = unS g s0

// ---

:: Serialized :== [String]

ser :: Serialized
ser = []

toStrings :: Serialized -> [String]
toStrings s = s

:: Serialize a :== State Serialized a

wrt :: a -> Serialize String | toString a
wrt a = S (\s. (Just undef, s ++ [toString a]))

rd :: Serialize String
rd = S (
    \s. 
        case s of
            [] = (Nothing, [])
            [a:as] = (Just a, as)
    )
    
match :: a -> Serialize a | toString a
match a = rd >>= (\x. if (toString a == x) (pure a) fail)

pred :: (String->Bool) -> Serialize String
pred f = rd >>= (\x. if (f x) (pure x) fail)

// ---

:: UNIT     = UNIT
:: EITHER a b = LEFT a | RIGHT b
:: PAIR   a b = PAIR a b
:: CONS   a   = CONS String a

:: Write a :== a -> Serialize String
:: Read a  :== Serialize a
 
class serialize a | isUNIT a where
  write :: a -> Serialize String
  read  :: Serialize a

class serialize1 t where
  write1 :: (Write a) (t a) -> Serialize String
  read1  :: (Read  a) -> Serialize (t a)

class serializeCONS a where
    writeCons :: (Write a) (CONS a) -> Serialize String
    readCons  :: String (Read a) -> Serialize (CONS a)

class serialize2 t where
  write2 :: (Write a) (Write b) (t a b) -> Serialize String
  read2  :: (Read  a) (Read  b) -> Serialize (t a b)

class isUNIT a :: a -> Bool
instance isUNIT UNIT where isUNIT _ = True
instance isUNIT a    where isUNIT _ = False

instance serialize Bool where
  write b = wrt b
  read = match True <|> match False

instance serialize Int where
    write i = wrt i
    read = pred (\s. s == (toString o toInt) s) >>= (\s. rtrn (toInt s))

instance serialize String where
    write s = wrt s
    read = rd

instance serialize UNIT where
    write _ = pure undef
    read = rtrn UNIT

instance serializeCONS UNIT where
    writeCons wa (CONS name a) = wrt name
    readCons name ra = match name >>| rtrn (CONS name UNIT)
 
instance serializeCONS a where
    writeCons wa (CONS name a) = wrt "(" >>| wrt name >>| wa a >>| wrt ")"
    readCons name ra = match "(" >>| match name >>| (ra >>= (\a. rtrn (CONS name a))) >>= (\a. (match ")") >>| rtrn a)
 
instance serialize2 EITHER where
  write2 wa wb (LEFT  a) = wa a
  write2 wa wb (RIGHT b) = wb b
  read2 ra rb =  (ra >>= \a. rtrn (LEFT a))
             <|> rb >>= \b. rtrn (RIGHT b)

instance serialize2 PAIR where
  write2 wa wb (PAIR a b) = wa a >>| wb b
  read2 ra rb = ra >>= \a. rb >>= \b. rtrn (PAIR a b)

// ---

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
 write a = write1 write a
 read    = read1  read

instance serialize1 [] where
    write1 writea l = write2 (writeCons write) (writeCons (write2 writea (write1 writea))) (fromList l)
    read1  reada = read2 (readCons NilString read) (readCons ConsString (read2 reada (read1 reada))) >>= \l. rtrn (toList l)
// ---

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
    write b = write1 write b
    read = read1 read

instance serialize1 Bin where
    write1 writea b = write2 (writeCons write) (writeCons (write2 (write1 writea) (write2 writea (write1 writea)))) (fromBin b)
    read1  reada    = read2 (readCons LeafString read) (readCons BinString (read2 (read1 reada) (read2 reada (read1 reada)))) >>= \b. rtrn (toBin b)
// ---

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
    write c = write2 (writeCons write) (writeCons write) (fromCoin c)
    read    = read2 (readCons "Head" read) (readCons "Tail" read) >>= \c. rtrn (toCoin c)

// ---

instance serialize (a,b) | serialize a & serialize b where
    write (a,b) = wrt "(" >>| write a >>| wrt "," >>| write b >>| wrt ")"
    read = match "(" >>| read >>= \a. match "," >>| read >>= \b. match ")" >>| rtrn (a,b)

// ---

Start = 
  [test True
  ,test False
  ,test 0
  ,test 123
  ,test -36
  ,test Head
  ,test Tail
  ,test [42]
  ,test [0..4]
  ,test [[True],[]]
  ,test [[[1]],[[2],[3,4]],[[]]]
  ,test [[True],[]]
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
test a = toStrings (snd ((unS t) ser)) where
 t
     =   write a
    >>| read
    >>= (\b. guard (a == b)
    >>| write "OK\n")
    <|> write a

/*
 * Output:
 * OK
 * OK
 * OK
 * OK
 * OK
 * OK
 * OK
 * OK
 * OK
 * OK
 * OK
 * OK
 * OK
 * OK
 * OK
 * OK
 * OK
 * OK
 * OK
 * End of the tests.
 */
