module e1_DeepEmbeddedDSL

/*
 * Jordi Riemens    s4243064
 * Thomas Churchman s4206606
 *
 * Based on:
 * Pieter Koopman, pieter@cs.ru.nl
 * Skeleton for Advanced Programming, week 8, 2017
 */

import StdEnv
import qualified Data.List as List
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

:: Expression a
   = Lit a
   | New   (BM a Set) [Int]
   | Size  (BM a Int) (Expression Set)
   | (+.) infixl 6 (Expression a) (Expression a) & + a
   | (-.) infixl 6 (Expression a) (Expression a) & - a
   | (*.) infixl 7 (Expression a) (Expression a) & * a
   | TRUE  (BM a Bool)
   | FALSE (BM a Bool)
   | Eq    (BM a Bool) (Expression a) (Expression a) & == a
   | Lteq  (BM a Bool) (Expression a) (Expression a) & Ord a
   | Lt    (BM a Bool) (Expression a) (Expression a) & Ord a
   | Gteq  (BM a Bool) (Expression a) (Expression a) & Ord a
   | Gt    (BM a Bool) (Expression a) (Expression a) & Ord a
   | Not   (BM a Bool) (Expression Bool)
   | Or    (BM a Bool) (Expression Bool) (Expression Bool)
   | And   (BM a Bool) (Expression Bool) (Expression Bool)

//////////////////////////////////////////////////
// Semantics                                    //
//////////////////////////////////////////////////

eval :: (Expression a) -> a
eval (Lit l) = l
eval (New {f} l) = f ('Set'.fromList l)
eval (Size {f} setExpr) = f ('Set'.size (eval setExpr))
eval (+. expr1 expr2) = eval expr1 + eval expr2
eval (-. expr1 expr2) = eval expr1 - eval expr2
eval (*. expr1 expr2) = eval expr1 * eval expr2
eval (TRUE {f}) = f True
eval (FALSE {f}) = f False
eval (Eq {f} expr1 expr2) = f (eval expr1 == eval expr2)
eval (Lteq {f} expr1 expr2) = f (eval expr1 <= eval expr2)
eval (Lt {f} expr1 expr2) = f (eval expr1 < eval expr2)
eval (Gteq {f} expr1 expr2) = f (eval expr1 >= eval expr2)
eval (Gt {f} expr1 expr2) = f (eval expr1 > eval expr2)
eval (Not {f} boolExpr) = f (not (eval boolExpr))
eval (Or {f} boolExpr1 boolExpr2) = f (eval boolExpr1 || eval boolExpr2)
eval (And {f} boolExpr1 boolExpr2) = f (eval boolExpr1 && eval boolExpr2)


Start = eval (Size bm (New bm [1,5,7,7]))
