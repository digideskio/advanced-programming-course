module e1_ShallowEmbeddedDSL


/*
 * Jordi Riemens    s4243064
 * Thomas Churchman s4206606
 *
 * Based on:
 * Pieter Koopman, pieter@cs.ru.nl
 * Skeleton for Advanced Programming, week 9, 2017
 */

import Data.Functor, Control.Applicative, Control.Monad
import Data.Tuple, StdClass, StdList, StdMaybe, StdString
import StdGeneric, StdBool, StdTuple
from StdFunc import o
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Either
import qualified Data.Set as Set


//////////////////////////////////////////////////
// Semantics                                    //
//////////////////////////////////////////////////

// We went for the State :== Map Ident Dynamic approach,
// since this is the most easily extensible to other
// variable types, and requires only one Map call per read/store,
// in contrast to having one map per variable type,
// and furthermore requires only one definition of such a read/store,
// in contrast to other methods which require a new type-checking
// definition per variable type.

:: Ident :== String
:: State :== 'Map'.Map Ident Dynamic
:: Sem a = S (State -> (Either String a, State))

unS :: (Sem a) -> State -> (Either String a, State)
unS (S f) = f

initState :: State
initState = 'Map'.newMap

instance Functor Sem where
    fmap f prog = S \s0 -> case unS prog s0 of
        (Right ret, s1)  -> (Right (f ret), s1)
        (Left err, s1)   -> (Left err, s1)

instance Applicative Sem where
    pure x = S \s0 -> (Right x, s0)
    (<*>) f x = S \s0 -> case unS f s0 of
        (Right ret1, s1) -> case unS x s1 of
            (Right ret2, s2) -> (Right (ret1 ret2), s2)
            (Left err, s2)   -> (Left err, s2)
        (Left err, s1)   -> (Left err, s1)

instance Monad Sem where
    bind x f = S \s0 -> case unS x s0 of
        (Right ret1, s1) -> unS (f ret1) s1
        (Left err, s1)   -> (Left err, s1)

store :: Ident a -> Sem a | TC a
store name val = S \s0 -> (Right val, 'Map'.put name (dynamic val) s0)

read :: Ident -> Sem a | TC a
read name = S \s0 -> case 'Map'.get name s0 of
    Nothing -> (Left ("The following variable could not be found: " +++ name), s0)
    Just val -> (typecheck name val, s0)
        where typecheck :: Ident Dynamic -> (Either String a) | TC a
              typecheck _ (e :: a^) = Right e
              typecheck name e      = Left ("Used variable is of wrong type: " +++ name)

fail :: String -> Sem a
fail err = S \s0 -> (Left err, s0)


//////////////////////////////////////////////////
// Expressions                                  //
//////////////////////////////////////////////////

:: Element :== Sem Int
:: Set :== Sem [Int]
:: Logical :== Sem Bool

flip :: (a b -> c) -> (b a -> c)
flip f = \b a -> f a b

integer :: Int -> Element
integer i = pure i

set :: [Int] -> Set
set s = pure s

logical :: Bool -> Logical
logical b = pure b

size :: Set -> Element
size s = length <$> s

// aliases for read and store to conform to last week's syntax
variable :: Ident -> Sem a | TC a
variable name = read name

(=.) infixl 2 :: Ident (Sem a) -> Sem a | TC a
(=.) name sem = sem >>= store name


class +. a b where
    (+.) infixl 6 :: a b -> Set

class -. a b where
    (-.) infixl 6 :: a b -> Set

class *. a b where
    (*.) infixl 7 :: a b -> Set


instance + Element where
    (+) e1 e2 = (+) <$> e1 <*> e2

instance +. Element Set where
    (+.) e s = (\i l -> [i:l]) <$> e <*> s

instance +. Set Element where // flipped to preserve order of evaluation
    (+.) s e = (\l i -> [i:l]) <$> s <*> e

instance +. Set Set where 
    (+.) s1 s2 = 'List'.union <$> s1 <*> s2


instance - Element where
    (-) e1 e2 = (-) <$> e1 <*> e2

instance -. Set Element where // flipped to preserve order of evaluation
    (-.) s e = (flip 'List'.delete) <$> s <*> e 

instance -. Set Set where 
    (-.) s1 s2 = 'List'.difference <$> s1 <*> s2


instance * Element where
    (*) e1 e2 = (*) <$> e1 <*> e2

instance *. Element Set where // flipped to preserve order of evaluation
    (*.) e s = (\i -> map ((*) i)) <$> e <*> s
    
instance *. Set Element where // flipped to preserve order of evaluation
    (*.) s e = (\i -> map ((*) i)) <$> e <*> s

instance *. Set Set where 
    (*.) s1 s2 = 'List'.intersect <$> s1 <*> s2

//////////////////////////////////////////////////
// Logicals                                     //
//////////////////////////////////////////////////

true :: Logical
true = logical True

false :: Logical
false = logical False

inSet :: Element Set -> Logical
inSet e s = 'List'.isMember <$> e <*> s

class ==. a where
    (==.) infixl 5 :: a a -> Logical

instance ==. Element where
    (==.) e1 e2 = (==) <$> e1 <*> e2
   
instance ==. Set where
    (==.) s1 s2 = (\s1 s2 -> length ('List'.difference s1 s2) == 0 && length ('List'.difference s2 s1) == 0)  <$> s1 <*> s2

class <=. a where
    (<=.) infixl 5 :: a a -> Logical
    
instance <=. Element where
    (<=.) e1 e2 = (<=) <$> e1 <*> e2
    
instance <=. Set where
    (<=.) s1 s2 = (\s1 s2 -> length ('List'.difference s1 s2) == 0) <$> s2 <*> s2
   
not :: Logical -> Logical
not l = ((==) False) <$> l
    
(||.) infixl 7 :: Logical Logical -> Logical
(||.) l1 l2 = (||) <$> l1 <*> l2
    
(&&.) infixl 7 :: Logical Logical -> Logical
(&&.) l1 l2 = (&&) <$> l1 <*> l2

//////////////////////////////////////////////////
// Statements                                   //
//////////////////////////////////////////////////
undef = undef 
(:.) infixr 1 :: (Sem a) (Sem b) -> Sem b
(:.) a b = a >>| b

for :: Ident In Set Do (Sem a) -> Sem ()
for ident In set Do stmts = set >>= \set` -> (
                                  case set` of
                                      [] = pure ()
                                      [s:ss] = ((ident =. pure s) :. stmts) :. for ident In (pure ss) Do stmts
                              )

:: In = In
:: Do = Do

if` :: Logical Then (Sem a) Else (Sem a) -> Sem a
if` l Then stmts1 Else stmts2 = l >>= \bool -> if bool stmts1 stmts2

:: Then = Then
:: Else = Else

//////////////////////////////////////////////////
// Evaluation                                   //
//////////////////////////////////////////////////

expr1 :: Element
expr1 = "v" =. integer 6 * integer 7

expr2 :: Set
expr2 = v +. set [1337]
    where v :: Element
          v = variable "v"

expr3 :: Logical
expr3 = not (integer 6 ==. integer 7 + integer 2)

stmt1 :: Element
stmt1 = "sum" =. integer 0 :.
        for "v" In (set [1,3,5,7,9,11,13]) Do
            ("sum" =. (variable "sum") + integer 0 + (variable "v")) :.
        variable "sum"

eval :: (Sem a) State -> Either String a
eval e s = fst (unS e s)

Start = eval (expr1 >>= \_ -> stmt1) initState


