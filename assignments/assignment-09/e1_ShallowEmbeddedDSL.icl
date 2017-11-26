module e1_ShallowEmbeddedDSL


/*
 * Jordi Riemens    s4243064
 * Thomas Churchman s4206606
 *
 * Based on:
 * Pieter Koopman, pieter@cs.ru.nl
 * Skeleton for Advanced Programming, week 9, 2017
 */

from StdEnv import undef
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
:: Views a = {a :: Sem a, s :: [String]}
:: Sem a :== State -> (Either String a, State)

/*
unS :: (Sem a) -> State -> (Either String a, State)
unS (S f) = f
*/

initState :: State
initState = 'Map'.newMap

/*
instance Functor Sem where
    fmap f prog = \s0 -> case prog s0 of
        (Right ret, s1)  -> (Right (f ret), s1)
        (Left err, s1)   -> (Left err, s1)

instance Applicative Sem where
    pure x = \s0 -> (Right x, s0)
    (<*>) f x = \s0 -> case f s0 of
        (Right ret1, s1) -> case x s1 of
            (Right ret2, s2) -> (Right (ret1 ret2), s2)
            (Left err, s2)   -> (Left err, s2)
        (Left err, s1)   -> (Left err, s1)

instance Monad Sem where
    bind x f = \s0 -> case x s0 of
        (Right ret1, s1) -> (f ret1) s1
        (Left err, s1)   -> (Left err, s1)
*/      

instance Functor Views where
    fmap f views = {views & a = (\state -> let (r, state2) = views.a state in (fmap f r, state2))}


instance Applicative Views where
    pure a = {a = (\s -> (Right a, s)), s = []}
    <*> vf va = {
                    a = (\state -> case vf.a state of
                            (Right ret1, state1) -> case va.a state1 of
                                (Right ret2, state2) -> (Right (ret1 ret2), state2)
                                (Left err, state2)   -> (Left err, state2)
                            (Left err, state1)   -> (Left err, state1)
                        )
                    , s = vf.s ++ va.s
                }
  
instance Monad Views where
    bind va f =
        {
            a = \state0 -> case va.a state0 of
                (Right ret1, state1) -> let vf = (f ret1) in vf.a state1
                (Left err, state1)   -> (Left err, state1),
            s = let vf = f undef in va.s ++ vf.s
        }

show :: String (Views a) -> Views a
show str views = {views & s = [str : views.s]}

store :: Ident a -> Sem a | TC a
store name val = \s0 -> (Right val, 'Map'.put name (dynamic val) s0)

read :: Ident -> Sem a | TC a
read name = \s0 -> case 'Map'.get name s0 of
    Nothing -> (Left ("The following variable could not be found: " +++ name), s0)
    Just val -> (typecheck name val, s0)
        where typecheck :: Ident Dynamic -> (Either String a) | TC a
              typecheck _ (e :: a^) = Right e
              typecheck name e      = Left ("Used variable is of wrong type: " +++ name)

fail :: String -> Sem a
fail err = \s0 -> (Left err, s0)


//////////////////////////////////////////////////
// Expressions                                  //
//////////////////////////////////////////////////

:: Element :== Views Int
:: Set :== Views [Int]
:: Logical :== Views Bool

flip :: (a b -> c) -> (b a -> c)
flip f = \b a -> f a b

integer :: Int -> Element
integer i = show (toString i) (pure i)

set :: [Int] -> Set
set s = let s` = 'List'.nub s in show ("{" +++ toString` s +++ "}") (pure s`)
    where
    toString` :: [Int] -> String
    toString` [] = ""
    toString` [i] = toString i
    toString` [i:is] = toString i +++ ", " +++ toString` is
    
logical :: Bool -> Logical
logical b = show (toString b) (pure b)

size :: Set -> Element
size s = length <$> show "|" s >>= \s -> show "|" (pure s) 

// aliases for read and store to conform to last week's syntax
variable :: Ident -> Views a | TC a
variable name = {a = read name, s = [name]}

(=.) infixl 2 :: Ident (Views a) -> Views a | TC a
(=.) name va = show (name +++ " =. ") va >>= \a -> {a = store name a, s = []}

class +. a b where
    (+.) infixl 6 :: a b -> Set

class -. a b where
    (-.) infixl 6 :: a b -> Set

class *. a b where
    (*.) infixl 7 :: a b -> Set


instance + Element where
    (+) e1 e2 = (+) <$> e1 <*> show "+" e2

instance +. Element Set where
    (+.) e s = (\i l -> [i:l]) <$> e <*> show "+." s

instance +. Set Element where // flipped to preserve order of evaluation
    (+.) s e = (\l i -> l ++ [i]) <$> s <*> show "+." e

instance +. Set Set where 
    (+.) s1 s2 = 'List'.union <$> s1 <*> show "+." s2


instance - Element where
    (-) e1 e2 = (-) <$> e1 <*> show "-" e2

instance -. Set Element where // flipped to preserve order of evaluation
    (-.) s e = (flip 'List'.delete) <$> s <*> show "-." e 

instance -. Set Set where 
    (-.) s1 s2 = 'List'.difference <$> s1 <*> show "-." s2


instance * Element where
    (*) e1 e2 = (*) <$> e1 <*> show "*" e2

instance *. Element Set where // flipped to preserve order of evaluation
    (*.) e s = (\i -> map ((*) i)) <$> e <*> show "*." s
    
instance *. Set Element where // flipped to preserve order of evaluation
    (*.) s e = (\i -> map ((*) i)) <$> e <*> show "*." s

instance *. Set Set where 
    (*.) s1 s2 = 'List'.intersect <$> s1 <*> show "*." s2

//////////////////////////////////////////////////
// Logicals                                     //
//////////////////////////////////////////////////

true :: Logical
true = logical True

false :: Logical
false = logical False

inSet :: Element Set -> Logical
inSet e s = 'List'.isMember <$> e <*> show " in " s

class ==. a where
    (==.) infixl 5 :: a a -> Logical

instance ==. Element where
    (==.) e1 e2 = (==) <$> e1 <*> show "==" e2
   
instance ==. Set where
    (==.) s1 s2 = (\s1 s2 -> length ('List'.difference s1 s2) == 0 && length ('List'.difference s2 s1) == 0)  <$> s1 <*> show "==" s2

class <=. a where
    (<=.) infixl 5 :: a a -> Logical
    
instance <=. Element where
    (<=.) e1 e2 = (<=) <$> e1 <*> show "<=" e2
    
instance <=. Set where
    (<=.) s1 s2 = (\s1 s2 -> length ('List'.difference s1 s2) == 0) <$> s2 <*> show "<=" s2
   
class <. a where
    (<.) infixl 5 :: a a -> Logical
    
instance <. Element where
    (<.) e1 e2 = (<) <$> e1 <*> show "<" e2
    
instance <. Set where
    (<.) s1 s2 = (\s1 s2 -> length ('List'.difference s1 s2) == 0 && length s1 < length s2) <$> s2 <*> show "<=" s2
   
not :: Logical -> Logical
not l = ((==) False) <$> show "!" l
    
(||.) infixl 7 :: Logical Logical -> Logical
(||.) l1 l2 = (||) <$> l1 <*> show "||" l2
    
(&&.) infixl 7 :: Logical Logical -> Logical
(&&.) l1 l2 = (&&) <$> l1 <*> show "&&" l2

//////////////////////////////////////////////////
// Statements                                   //
//////////////////////////////////////////////////

(:.) infixr 1 :: (Views a) (Views b) -> Views b
(:.) a b = a >>| show ";\n" b

:: In = In
:: Do = Do

for :: Ident In Set Do (Views a) -> Views ()
for ident In set Do stmts = {
                                a = \state -> case set.a state of
                                        (Right set`, state2) -> evalFor ident set` stmts state2
                                , s = ["for ", ident, " in "] ++ set.s ++ [" do {\n"] ++ indent stmts.s ++ ["\n}"]
                            }
    where
    evalFor :: Ident [Int] (Views a) -> Sem ()
    evalFor ident [] stmts = \state -> (Right (), state)
    evalFor ident [s:ss] stmts = \state -> let (_, state2) = store ident s state in
                                           let (_, state3) = stmts.a state2 in
                                           evalFor ident ss stmts state3

while :: Logical Do (Views a) -> Views ()
while condition Do stmts = {   a = \state -> evalWhile condition stmts state
                               , s = ["while ("] ++ condition.s ++ [") do {\n"] ++ indent stmts.s ++ ["\n}"]
                           }
    where
    evalWhile :: Logical (Views a) -> Sem ()
    evalWhile condition stmts = \state -> case condition.a state of
                                              (Right bool, state2) -> if bool (
                                                      case stmts.a state2 of
                                                          (Right _, state3) ->  evalWhile condition stmts state3
                                                          (Left err, state3) -> (Left err, state3)
                                                  )
                                                  (Right (), state2)
                                              (Left err, state2) -> (Left err, state2)

:: Then = Then
:: Else = Else

If :: Logical Then (Views a) Else (Views a) -> Views a
If condition Then then Else else = {
                             a = (\state -> case condition.a state of
                                     (Right bool, state2) = if bool then.a else.a state2
                                     (Left err, state2) = (Left err, state2)
                                 )
                             , s = ["if ("] ++ condition.s ++ [") then {\n"] ++ indent then.s ++ ["\n} else {\n"] ++ indent else.s ++ ["\n}"]
                         }

skip :: Views ()
skip = show "skip" (pure ())

//////////////////////////////////////////////////
// Evaluation                                   //
//////////////////////////////////////////////////

/* Hacky type hints
 * Necessary for, e.g.:
 *     variable "r" * variable "n"
 * as * is a class, and we use dynamics, so Clean cannot infer that
 * variable "r" should be an Element. Does Clean implement nicer way
 * to give an inline type hint? For example:
 *     (variable "r") :: Element  * variable "n"
 */ 
integer` :: Element -> Element
integer` e = e

set` :: Set -> Set
set` s = s

logical` :: Logical -> Logical
logical` l = l

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
            ("sum" =. (variable "sum") + (If (variable "v" <=. integer 9) Then (
                integer` (variable "v")
            ) Else (
                integer 0
            ))) :.
        variable "sum"
        
findFirstNPrimes :: Int -> Set
findFirstNPrimes n =
    "primes" =. set [] :.
    "cur" =. integer 2 :.
    // Loop until n primes are found
    while (size (variable "primes") <. integer n) Do (
        "n" =. integer 2 :.
        "hasDivisor" =. logical False :.
        // Try to find an n * multiplier such that n * multiplier = cur, which means
        // cur is divisible by more than 1 and itself.
        while (integer` (variable "n") <. variable "cur") Do (
            "multiplier" =. integer 1 :.
            while (integer` (variable "multiplier") <. variable "cur") Do (
                If ((integer` (variable "n") * variable "multiplier") ==. variable "cur") Then (
                    "hasDivisor" =. logical True :.
                    skip
                ) Else (
                    skip
                ) :.
                "multiplier" =. variable "multiplier" + integer 1
            ) :.
            "n" =. variable "n" + integer 1
        ) :.
        If (variable "hasDivisor") Then (
            // A divisor has been found; not prime
            skip
        ) Else (
            // No divisor has been found; prime
            "primes" =. (set` (variable "primes") +. integer` (variable "cur")) :.
            skip
        ) :.
        "cur" =. variable "cur" + integer 1
    ) :.
    variable "primes"

// From slides
fac2 :: Int -> Element
fac2 n =
    "n" =. integer n :.
    "r" =. integer 1 :.
    If (integer 0 <. variable "n") Then (
        while (integer 1 <. variable "n") Do (
            "r" =. integer` (variable "r") * variable "n" :.
            "n" =. variable "n" - integer 1
        )
    ) Else (
        skip
    ) :.
    "out" =. variable "r"

eval :: (Views a) State -> Either String a
eval va s = fst (va.a s)

// Hacky indentation function
indent :: [String] -> [String]
indent [] = []
indent [s:ss] = indent` ["    " +++ s : ss]
    where
    indent` [] = []
    indent` [s:ss] = [foldr (+++) "" [if (c == "\n") "\n    " c \\ c <- fromString s] : indent` ss]

print :: (Views a) -> String
print va = foldr (+++) "" va.s

Start
    //# prog = expr1 :. expr2
    //# prog = stmt1
    # prog = findFirstNPrimes 15
    //# prog = "a" =. integer 6
    //# prog = fac2 5
    = (eval prog initState, "\n------\n" +++ print prog +++ "\n")


