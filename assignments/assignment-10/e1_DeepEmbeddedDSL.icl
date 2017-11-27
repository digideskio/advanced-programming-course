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

:: Set :== 'Set'.Set Int

instance + Set where
    (+) s1 s2 = 'Set'.union s1 s2

instance - Set where
    (-) s1 s2 = 'Set'.difference s1 s2

:: Identifier :== String

:: Expression a
   = Lit a
   | New        (BM a Set) [Int]
   | Size       (BM a Int) (Expression Set)
   | (+.) infixl 6 (Expression a) (Expression a) & + a
   | (-.) infixl 6 (Expression a) (Expression a) & - a
   | (*.) infixl 7 (Expression a) (Expression a) & * a
   | (=.) infixl 2 Identifier (Expression a)
   | TRUE       (BM a Bool)
   | FALSE      (BM a Bool)
   | E.b: Eq    (BM a Bool) (Expression b) (Expression b) & == b
   | E.b: Lteq  (BM a Bool) (Expression b) (Expression b) & Ord b
   | E.b: Lt    (BM a Bool) (Expression b) (Expression b) & Ord b
   | E.b: Gteq  (BM a Bool) (Expression b) (Expression b) & Ord b
   | E.b: Gt    (BM a Bool) (Expression b) (Expression b) & Ord b
   | Not        (BM a Bool) (Expression Bool)
   | Or         (BM a Bool) (Expression Bool) (Expression Bool)
   | And        (BM a Bool) (Expression Bool) (Expression Bool)

//////////////////////////////////////////////////
// Syntactic sugar                              //
//////////////////////////////////////////////////

true = TRUE bm
false = FALSE bm

(==.) infix 4 :: (Expression a) (Expression a) -> Expression Bool | ==, toString a
(==.) expr1 expr2 = Eq bm expr1 expr2

(<=.) infix 4 :: (Expression a) (Expression a) -> Expression Bool | Ord, toString a
(<=.) expr1 expr2 = Lteq bm expr1 expr2

(<.) infix 4 :: (Expression a) (Expression a) -> Expression Bool | Ord, toString a
(<.) expr1 expr2 = Lt bm expr1 expr2

(>=.) infix 4 :: (Expression a) (Expression a) -> Expression Bool | Ord, toString a
(>=.) expr1 expr2 = Gteq bm expr1 expr2

(>.) infix 4 :: (Expression a) (Expression a) -> Expression Bool | Ord, toString a
(>.) expr1 expr2 = Gt bm expr1 expr2

//////////////////////////////////////////////////
// Semantics                                    //
//////////////////////////////////////////////////

:: State :== 'Map'.Map Identifier Dynamic
:: Views a = {a :: Sem a, s :: [String]}
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

eval` :: (Expression a) -> a
eval` (Lit l) = l
eval` (New {f} l) = f ('Set'.fromList l)
eval` (Size {f} setExpr) = f ('Set'.size (eval` setExpr))
eval` (+. expr1 expr2) = eval` expr1 + eval` expr2
eval` (-. expr1 expr2) = eval` expr1 - eval` expr2
eval` (*. expr1 expr2) = eval` expr1 * eval` expr2
eval` (TRUE {f}) = f True
eval` (FALSE {f}) = f False
eval` (Eq {f} expr1 expr2) = f (eval` expr1 == eval` expr2)
eval` (Lteq {f} expr1 expr2) = f (eval` expr1 <= eval` expr2)
eval` (Lt {f} expr1 expr2) = f (eval` expr1 < eval` expr2)
eval` (Gteq {f} expr1 expr2) = f (eval` expr1 >= eval` expr2)
eval` (Gt {f} expr1 expr2) = f (eval` expr1 > eval` expr2)
eval` (Not {f} boolExpr) = f (not (eval` boolExpr))
eval` (Or {f} boolExpr1 boolExpr2) = f (eval` boolExpr1 || eval` boolExpr2)
eval` (And {f} boolExpr1 boolExpr2) = f (eval` boolExpr1 && eval` boolExpr2)

eval :: (Expression a) -> Sem a | TC a
eval (Lit l) = pure l
eval (New {f} l) = pure (f ('Set'.fromList l))
eval (Size {f} setExpr) = eval setExpr >>= \set -> (pure o f) ('Set'.size set)
eval (+. expr1 expr2) =
    undef
    //eval expr1
    //>>= \expr1 -> eval expr2
    //>>= \expr2 -> pure (expr1 + expr2)
eval (-. expr1 expr2) =
    undef
    //eval expr1
    //>>= \expr1 -> eval expr2
    //>>= \expr2 -> pure (expr1 - expr2)
eval (*. expr1 expr2) =
    undef
    //eval expr1
    //>>= \expr1 -> eval expr2
    //>>= \expr2 -> pure (expr1 * expr2)
eval (=. identifier expr) = eval expr >>= store identifier
eval (TRUE {f}) = (pure o f) True
eval (FALSE {f}) = (pure o f) False
eval (Eq {f} expr1 expr2) =
    undef
    //eval expr1
    //>>= \expr1` -> eval expr2
    //>>= \expr2` -> (pure o f) (expr1` == expr2`)
eval (Lteq {f} expr1 expr2) =
    undef
    //eval expr1
    //>>= \expr1` -> eval expr2
    //>>= \expr2 -> (pure o f) (expr1 <= expr2)
eval (Lt {f} expr1 expr2) =
    undef
    //eval expr1
    //>>= \expr1` -> eval expr2
    //>>= \expr2 -> (pure o f) (expr1 < expr2)
eval (Gteq {f} expr1 expr2) = 
    undef
    //eval expr1
    //>>= \expr1` -> eval expr2
    //>>= \expr2 -> (pure o f) (expr1 >= expr2)
eval (Gt {f} expr1 expr2) =
    undef
    //eval expr1
    //>>= \expr1` -> eval expr2
    //>>= \expr2 -> (pure o f) (expr1 > expr2)
eval (Not {f} boolExpr) = eval boolExpr >>= pure o f o not
eval (Or {f} boolExpr1 boolExpr2) =
    eval boolExpr1
    >>= \bool1 -> eval boolExpr2
    >>= \bool2 -> (pure o f) (bool1 || bool2)
eval (And {f} boolExpr1 boolExpr2) =
    eval boolExpr1
    >>= \bool1 -> eval boolExpr2
    >>= \bool2 -> (pure o f) (bool1 && bool2)

show :: (Expression a) -> String
show _ = undef

Start = eval` (Size bm (New bm [1,5,7,7]) +. Lit 39)
