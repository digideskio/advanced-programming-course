module e1_DeepEmbeddedDSL

/*
 * Jordi Riemens    s4243064
 * Thomas Churchman s4206606
 *
 * Based on:
 * Pieter Koopman, pieter@cs.ru.nl
 * Skeleton for Advanced Programming, week 8, 2017
 */

import StdEnv, StdMaybe, Data.Either, Data.Functor, Control.Applicative, Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set

//////////////////////////////////////////////////
// Language data structures                     //
//////////////////////////////////////////////////

// Bimap to convince Clean's type system that all is well
:: BM a b = { t :: a -> b, f :: b -> a }

bm :: BM a a
bm = {f = id, t = id}

:: Identifier :== String
:: Set :== 'Set'.Set Int

instance + Set where
    (+) s1 s2 = 'Set'.union s1 s2

instance - Set where
    (-) s1 s2 = 'Set'.difference s1 s2

instance * Set where
    (*) s1 s2 = 'Set'.intersection s1 s2

:: Expression a
   = Lit a
   | Size       (BM a Int) (Expression Set)
   | Var Identifier
   | (+.) infixl 6 (Expression a) (Expression a) & + a
   | (-.) infixl 6 (Expression a) (Expression a) & - a
   | (*.) infixl 7 (Expression a) (Expression a) & * a
   | (=.) infixl 2 Identifier (Expression a)
   | E.b: Eq    (BM a Bool) (Expression b) (Expression b) & TC, == b
   | E.b: Lteq  (BM a Bool) (Expression b) (Expression b) & TC, Ord b
   | E.b: Lt    (BM a Bool) (Expression b) (Expression b) & TC, Ord b
   | E.b: Gteq  (BM a Bool) (Expression b) (Expression b) & TC, Ord b
   | E.b: Gt    (BM a Bool) (Expression b) (Expression b) & TC, Ord b
   | Not        (BM a Bool) (Expression Bool)
   | Or         (BM a Bool) (Expression Bool) (Expression Bool)
   | And        (BM a Bool) (Expression Bool) (Expression Bool)
   // Expression-based language (like Rust <3)
   // Though one issue is that variables can get void values now, e.g. '"a" =. Skip',
   // but this is also possible through e.g. '"a" =. Lit ()'
   | E.b: (:.) infixr 1 (Expression b) (Expression a) & TC b
   | If         (Expression Bool) Then (Expression a) Else (Expression a)
   // () because a for loop might not return a value (e.g. its set is empty), and
   // though bm a (Maybe a) could also work, there is no wider support for this in
   // the language we are creating.
   | E.b: For   (BM a ()) Identifier In (Expression Set) Do (Expression b) & TC b
   | E.b: While (BM a ()) (Expression Bool) Do (Expression b) & TC b
   | Skip       (BM a ())

:: Then = Then
:: Else = Else
:: In = In
:: Do = Do

//////////////////////////////////////////////////
// Syntactic sugar                              //
//////////////////////////////////////////////////

true = Lit True
false = Lit False
skip = Skip bm

set :: [Int] -> Expression Set
set l = Lit ('Set'.fromList l)

size = Size bm

(==.) infix 4 :: (Expression a) (Expression a) -> Expression Bool | TC, ==, toString a
(==.) expr1 expr2 = Eq bm expr1 expr2

(<=.) infix 4 :: (Expression a) (Expression a) -> Expression Bool | TC, Ord, toString a
(<=.) expr1 expr2 = Lteq bm expr1 expr2

(<.) infix 4 :: (Expression a) (Expression a) -> Expression Bool | TC, Ord, toString a
(<.) expr1 expr2 = Lt bm expr1 expr2

(>=.) infix 4 :: (Expression a) (Expression a) -> Expression Bool | TC, Ord, toString a
(>=.) expr1 expr2 = Gteq bm expr1 expr2

(>.) infix 4 :: (Expression a) (Expression a) -> Expression Bool | TC, Ord, toString a
(>.) expr1 expr2 = Gt bm expr1 expr2

for = For bm
while = While bm

//////////////////////////////////////////////////
// Semantics                                    //
//////////////////////////////////////////////////

:: State :== 'Map'.Map Identifier Dynamic
:: Sem a = S (State -> (Either String a, State))

initialState :: State
initialState = 'Map'.newMap

unS :: (Sem a) -> State -> (Either String a, State)
unS (S f) = f

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

store :: Identifier a -> Sem a | TC a
store name val = S \s0 -> (Right val, 'Map'.put name (dynamic val) s0)

read :: Identifier -> Sem a | TC a
read name = S \s0 -> case 'Map'.get name s0 of
    Nothing -> (Left ("The following variable could not be found: " +++ name), s0)
    Just val -> (typecheck name val, s0)
        where typecheck :: Identifier Dynamic -> (Either String a) | TC a
              typecheck _ (e :: a^) = Right e
              typecheck name e      = Left ("Used variable is of wrong type: " +++ name)

//////////////////////////////////////////////////
// Evaluation                                   //
//////////////////////////////////////////////////

eval :: (Expression a) -> Sem a | TC a
eval (Lit l) = pure l
eval (Var name) = read name
eval (Size {f} setExpr) = eval setExpr >>= \set -> (pure o f) ('Set'.size set)
eval (+. expr1 expr2) = ev expr1 expr2
    where ev :: (Expression a) (Expression a) -> Sem a | TC,+ a
          ev e1 e2 = eval e1 >>= \r1 -> eval e2 >>= \r2 -> pure (r1 + r2)
eval (-. expr1 expr2) = ev expr1 expr2
    where ev :: (Expression a) (Expression a) -> Sem a | TC,- a
          ev e1 e2 = eval e1 >>= \r1 -> eval e2 >>= \r2 -> pure (r1 - r2)
eval (*. expr1 expr2) = ev expr1 expr2
    where ev :: (Expression a) (Expression a) -> Sem a | TC,* a
          ev e1 e2 = eval e1 >>= \r1 -> eval e2 >>= \r2 -> pure (r1 * r2)
eval (=. identifier expr) = eval expr >>= store identifier
eval (Eq {f} expr1 expr2) = ev f expr1 expr2
    where ev :: (Bool -> a) (Expression b) (Expression b) -> Sem a | TC a & TC,== b
          ev f e1 e2 = eval e1 >>= \r1 -> eval e2 >>= \r2 -> (pure o f) (r1 == r2)
eval (Lteq {f} expr1 expr2) = ev f expr1 expr2
    where ev :: (Bool -> a) (Expression b) (Expression b) -> Sem a | TC a & TC,Ord b
          ev f e1 e2 = eval e1 >>= \r1 -> eval e2 >>= \r2 -> (pure o f) (r1 <= r2)
eval (Lt {f} expr1 expr2) = ev f expr1 expr2
    where ev :: (Bool -> a) (Expression b) (Expression b) -> Sem a | TC a & TC,Ord b
          ev f e1 e2 = eval e1 >>= \r1 -> eval e2 >>= \r2 -> (pure o f) (r1 < r2)
eval (Gteq {f} expr1 expr2) =  ev f expr1 expr2
    where ev :: (Bool -> a) (Expression b) (Expression b) -> Sem a | TC a & TC,Ord b
          ev f e1 e2 = eval e1 >>= \r1 -> eval e2 >>= \r2 -> (pure o f) (r1 >= r2)
eval (Gt {f} expr1 expr2) = ev f expr1 expr2
    where ev :: (Bool -> a) (Expression b) (Expression b) -> Sem a | TC a & TC,Ord b
          ev f e1 e2 = eval e1 >>= \r1 -> eval e2 >>= \r2 -> (pure o f) (r1 > r2)
eval (Not {f} boolExpr) = eval boolExpr >>= pure o f o not
eval (Or {f} boolExpr1 boolExpr2) = ev f boolExpr1 boolExpr2
    where ev :: (Bool -> a) (Expression Bool) (Expression Bool) -> Sem a | TC a
          ev f e1 e2 = eval e1 >>= \r1 -> eval e2 >>= \r2 -> (pure o f) (r1 || r2)
eval (And {f} boolExpr1 boolExpr2) = ev f boolExpr1 boolExpr2
    where ev :: (Bool -> a) (Expression Bool) (Expression Bool) -> Sem a | TC a
          ev f e1 e2 = eval e1 >>= \r1 -> eval e2 >>= \r2 -> (pure o f) (r1 && r2)
eval (:. expr1 expr2) = eval expr1 >>| eval expr2
eval (If condition Then exprThen Else exprElse) =
    eval condition >>= \cond -> if cond (eval exprThen) (eval exprElse)
eval (For {f} identifier In exprSet Do exprBody) = ev f identifier exprSet exprBody
    where ev :: (() -> a) Identifier (Expression ('Set'.Set b)) (Expression c) -> Sem a | TC a & TC b & TC c
          ev f identifier exprSet exprBody = eval exprSet
               >>= \set -> ev` f identifier ('Set'.toList set) exprBody
          ev` :: (() -> a) Identifier [b] (Expression c) -> (Sem a) | TC a & TC b & TC c
          ev` f identifier [] _ = (pure o f) ()
          ev` f identifier [s:ss] exprBody = store identifier s >>| eval exprBody >>| ev` f  identifier ss exprBody
eval (While {f} exprCond Do exprBody) = ev f exprCond exprBody
    where ev :: (() -> a) (Expression Bool) (Expression b) -> (Sem a) | TC b
          ev f exprCond exprBody =
              eval exprCond >>=
              \cond -> if cond (eval exprBody >>| ev f exprCond exprBody) ((pure o f) ())
eval (Skip {f}) = (pure o f) ()

show :: (Expression a) -> String
show _ = undef

/* Hacky type hints
 * Necessary for, e.g.:
 *     variable "r" * variable "n"
 * as * is a class, and we use dynamics, so Clean cannot infer that
 * variable "r" should be an Element. Does Clean implement nicer way
 * to give an inline type hint? For example:
 *     (variable "r") :: Element  * variable "n"
 */ 
integer` :: (Expression Int) -> Expression Int
integer` e = e

set` :: (Expression ('Set'.Set a)) -> (Expression ('Set'.Set a))
set` s = s

logical` :: (Expression Bool) -> Expression Bool
logical` l = l

findFirstNPrimes :: Int -> Expression ('Set'.Set Int)
findFirstNPrimes n =
    "primes" =. set [] :.
    "cur" =. Lit 2 :.
    // Loop until n primes are found
    while (size (Var "primes") <. Lit n) Do (
        "n" =. Lit 2 :.
        "hasDivisor" =. false :.
        // Try to find an n * multiplier such that n * multiplier = cur, which means
        // cur is divisible by more than 1 and itself.
        while (integer` (Var "n") <. Var "cur") Do (
            "multiplier" =. Lit 1 :.
            while (integer` (Var "multiplier") <. Var "cur") Do (
                If ((integer` (Var "n") *. Var "multiplier") ==. Var "cur") Then (
                    "hasDivisor" =. true :.
                    skip
                ) Else (
                    skip
                ) :.
                "multiplier" =. Var "multiplier" +. Lit 1
            ) :.
            "n" =. Var "n" +. Lit 1
        ) :.
        If (Var "hasDivisor") Then (
            // A divisor has been found; not prime
            skip
        ) Else (
            // No divisor has been found; prime
            //"primes" =. (set` (Var "primes") +. integer` (Var "cur")) :.
            skip
        ) :.
        "cur" =. Var "cur" +. Lit 1
    ) :.
    Var "primes"

Start
    #prog1 = Size bm (set [1,5,7,7]) +. Lit 39
    #prog2 = findFirstNPrimes 15
    = (unS (eval prog2)) initialState
