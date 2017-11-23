module e1_DeepEmbeddedDSL

/*
  Advanved Progrmming 2017, Assignment 8
  Pieter Koopman, pieter@cs.ru.nl
*/

from iTasks import class iTask, class toPrompt, class Publishable, instance Publishable Task,
	instance toPrompt String, instance Functor Task, 
	class TApplicative, instance TApplicative Task,
	generic gEq, generic gDefault, generic JSONDecode, generic JSONEncode, generic gText, generic gEditor, 
	:: JSONNode, :: TextFormat, :: Editor, :: TaskValue(..), :: Stability, :: Task, :: Action, 
	:: TaskCont(..), :: ViewOption(..), :: UpdateOption(..),
	-||-, -||, ||-, >>*, always, hasValue, updateInformation, viewInformation, startEngine
import qualified iTasks
import qualified iTasks.WF.Combinators.Overloaded as WF
import Data.Functor, Control.Applicative, Control.Monad
import Data.Tuple, StdClass, StdList, StdMaybe, StdString
import StdGeneric, StdBool
from StdFunc import o
import qualified Data.List as List
import qualified Data.Map as Map

import Data.Either
import qualified Data.Set as Set

:: Expression
  = New      [Int]
  | Elem     Int
  | Variable Ident
  | Size     SetExp
  | (+.) infixl 6 Expression Expression
  | (-.) infixl 6 Expression Expression
  | (*.) infixl 7 Expression Expression
  | (=.) infixl 2 Ident Expression

:: Logical
  = TRUE | FALSE
  | (In) infix 4 Elem SetExp
  | (==.) infix 4 Expression Expression
  | (<=.) infix 4 Expression Expression
  | Not Logical
  | (||.) infixr 2 Logical Logical
  | (&&.) infixr 3 Logical Logical

:: Stmt
  = If Logical Stmt Stmt
  | For Ident SetExp Stmt
  | Expression Expression
  | Logical Logical

:: SetExp :== Expression
:: Elem   :== Expression
:: Ident  :== String


// === Structures

:: Val = Int Int | Set ('Set'.Set Int)
:: State :== 'Map'.Map Ident Val
:: Sem a = S (State -> (Either String a, State))

unS :: (Sem a) -> State -> (Either String a, State)
unS (S f) = f

instance == Val where
    (==) (Int i1) (Int i2) = i1 == i2
    (==) (Set s1) (Set s2) = 'Set'.toList s1 == 'Set'.toList s2
    (==) _ _ = False

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

store :: Ident Val -> Sem Val
store name val = S \s0 -> (Right val, 'Map'.put name val s0)

read :: Ident -> Sem Val
read name = S \s0 -> case 'Map'.get name s0 of
    Just val -> (Right val, s0)
    Nothing  -> (Left ("The following variable could not be found: " +++ name), s0)
        
fail :: String -> Sem a
fail err = S \s0 -> (Left err, s0)


// === Semantics

eval :: Expression -> Sem Val
eval (New set) = (pure o Set o 'Set'.fromList) set
eval (Elem i) = (pure o Int) i
eval (Variable v) = read v
eval (Size sexp) = eval sexp
    >>= \sval -> case sval of
        Int _   -> fail "Cannot take the size of an integer"
        Set set -> (pure o Int o 'Set'.size) set
eval (e1 +. e2) = eval e1 
    >>= \v1    -> eval e2
    >>= \v2    -> case (v1,v2) of
        (Int i1, Int i2) -> (pure o Int) (i1 + i2)
        (Int i1, Set s2) -> (pure o Set) ('Set'.insert i1 s2)
        (Set s1, Int i2) -> (pure o Set) ('Set'.insert i2 s1)
        (Set s1, Set s2) -> (pure o Set) ('Set'.union s1 s2)
eval (e1 -. e2) = eval e1 
    >>= \v1    -> eval e2
    >>= \v2    -> case (v1,v2) of
        (Int i1, Int i2) -> (pure o Int) (i1 - i2)
        (Int i1, Set s2) -> fail "Cannot subtract a set from an integer"
        (Set s1, Int i2) -> (pure o Set) ('Set'.delete i2 s1)
        (Set s1, Set s2) -> (pure o Set) ('Set'.difference s1 s2)
eval (e1 *. e2) = eval e1 
    >>= \v1    -> eval e2
    >>= \v2    -> case (v1,v2) of
        (Int i1, Int i2) -> (pure o Int) (i1 * i2)
        (Int i1, Set s2) -> (pure o Set o 'Set'.mapSet (\v -> i1*v)) s2
        (Set s1, Int i2) -> fail "Cannot multiply a set with an integer"
        (Set s1, Set s2) -> (pure o Set) ('Set'.intersection s1 s2)
eval (v =. e2) = eval e2 >>= store v 

evalLogical :: Logical -> Sem Bool
evalLogical TRUE        = pure True
evalLogical FALSE       = pure False
evalLogical (Not log)   = fmap not (evalLogical log)
evalLogical (e In sexp) = eval e
    >>= \v             -> eval sexp
    >>= \set           -> case (v,set) of
        (Int i, Set s)  -> pure ('Set'.member i s)
        (_,_)           -> fail "Membership tests only work with types <integer> In <set>"
evalLogical (e1 ==. e2) = eval e1
    >>= \v1            -> eval e2
    >>= \v2            -> pure (v1 == v2)
evalLogical (e1 <=. e2) = eval e1
    >>= \v1            -> eval e2
    >>= \v2            -> case (v1,v2) of
        (Int i1, Int i2) -> pure (i1 <= i2)
        (_,_)            -> fail "Can only use <= for pairs of integer expressions"
evalLogical (l1 ||. l2) = evalLogical l1
    >>= \b1            -> evalLogical l2
    >>= \b2            -> pure (b1 || b2)
evalLogical (l1 &&. l2) = evalLogical l1
    >>= \b1            -> evalLogical l2
    >>= \b2            -> pure (b1 && b2)

evalStmt :: Stmt -> Sem ()
evalStmt (If cond then else) = evalLogical cond
    >>= \b                  -> if b (evalStmt then) (evalStmt else)
evalStmt (For v sexp stmt)   = eval sexp
    >>= \sval               -> case sval of
        Set set -> foldr (\a b -> a >>= \_ -> b) (pure ()) (map (\el -> store v (Int el) >>= \_ -> evalStmt stmt) ('Set'.toList set))
        Int _   -> fail "For statements can only be used when assigning sets to the temporary variable"
evalStmt (Expression e)      = eval e
    >>= \_                  -> pure ()
evalStmt (Logical log)       = evalLogical log
    >>= \_                  -> pure ()

executeProgram :: [Stmt] -> (Either String (), State)
executeProgram stmts = unS (foldr (\a b -> a >>= \_ -> b) (pure ()) (map evalStmt stmts)) 'Map'.newMap

sampleProgram = 
    [ Expression ("set" =. Elem 2 *. (New [1,2,3,4] +. (Elem 1 +. Elem 2) -. (Elem 3)) +. (Elem 1)) // set = [1,2,4,8]
    , Expression ("counter" =. Size (Variable "set")) // counter = |size| = 4
    , For "i" (Variable "set") (Expression ("counter" =. Variable "counter" +. Variable "i" -. Elem 1)) // for i in set, counter += i - 1
    ] // at the end, counter should be the sum of set, i.e. 15


// === Simulation

(>>>=)     :== 'iTasks'.tbind
(>>>|) a b :== 'iTasks'.tbind a (\_ -> b)
treturn    :== 'iTasks'.return
ActionOk   :== 'iTasks'.ActionOk
ActionQuit :== 'iTasks'.ActionQuit
ActionNew  :== 'iTasks'.ActionNew


Start = executeProgram sampleProgram
    