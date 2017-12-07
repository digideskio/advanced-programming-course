module e1_TypeClassDSL

/*
 * Jordi Riemens    s4243064
 * Thomas Churchman s4206606
 *
 * Based on:
 * Pieter Koopman, pieter@cs.ru.nl
 * Skeleton for Advanced Programming, week 8+10, 2017
 */
 
import StdEnv, Data.Either, Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

//////////////////////////////////////////////////
// Basic data types                             //
//////////////////////////////////////////////////

class type a | toString, TC a where
    type :: a -> String

:: Set a :== 'Set'.Set a

instance type Int  where type _ = "int"
instance type Bool where type _ = "bool"
instance type Char where type _ = "char"

instance type (Set a) | type a where
    type 'Set'.Tip = "{a}"
    type ('Set'.Bin _ x _ _) = "{" +++ type x +++ "}"

instance == (Set a) | == a where
    (==) a b = 'Set'.toList a == 'Set'.toList b

instance toString (Set a) | toString a where
    toString s = "{" +++ toString` ('Set'.toList s) +++ "}"
        where toString` [] = ""
              toString` [a] = toString a
              toString` [a:as] = toString a +++ ", " +++ toString` as

//////////////////////////////////////////////////
// Language construct types                     //
//////////////////////////////////////////////////

:: Upd = Upd
:: Expr = Expr
:: Stmt = Stmt

class isExpr a :: a -> a

instance isExpr Expr where isExpr a = a
instance isExpr Upd  where isExpr a = a


//////////////////////////////////////////////////
// Operations                                   //
//////////////////////////////////////////////////

instance + (Set a) | Ord, == a where
    (+) s1 s2 = 'Set'.union s1 s2
instance - (Set a) | Ord, == a where
    (-) s1 s2 = 'Set'.difference s1 s2
instance * (Set a) | Ord, == a where
    (*) s1 s2 = 'Set'.intersection s1 s2


//////////////////////////////////////////////////
// Language definition                          //
//////////////////////////////////////////////////

class literals v where
    Lit :: a -> v a Expr         | type a
    New :: [a] -> v (Set a) Expr | type, Ord, == a
    New l :== Lit ('Set'.fromList l)

class variables v where
    (=.) infixr 2 :: (v t Upd) (v t p) -> v t Expr                      | type t & isExpr p
    Var :: ((v t Upd) -> In t (v a p)) -> v a p                         | type t

class arithexprs v where
    Size :: (v (Set a) p) -> v Int Expr                                 | isExpr p
    (+.)  infixl 6 :: (v a p) (v a q) -> v a Expr                       | + a & isExpr p & isExpr q
    (-.)  infixl 6 :: (v a p) (v a q) -> v a Expr                       | - a & isExpr p & isExpr q
    (*.)  infixl 7 :: (v a p) (v a q) -> v a Expr                       | * a & isExpr p & isExpr q
    (++.) infixl 6 :: (v (Set a) p) (v a q) -> v (Set a) Expr           | isExpr p & isExpr q & Ord a
    (.++) infixr 6 :: (v a p) (v (Set a) q) -> v (Set a) Expr           | isExpr p & isExpr q & Ord a
    (--.) infixl 6 :: (v (Set a) p) (v a q) -> v (Set a) Expr           | isExpr p & isExpr q & Ord a
    (.**) infixr 6 :: (v a p) (v (Set a) q) -> v (Set a) Expr           | * a & isExpr p & isExpr q

class booleanexprs v where
    (&&.) infixr 3 :: (v Bool p) (v Bool q) -> v Bool Expr              | isExpr p & isExpr q
    (||.) infixr 2 :: (v Bool p) (v Bool q) -> v Bool Expr              | isExpr p & isExpr q
    ~. :: (v Bool p) -> v Bool Expr                                     | isExpr p

class comparisons v where
    (==.) infix 4 :: (v a p) (v a q) -> v Bool Expr                     | == a  & isExpr p & isExpr q
    (<.)  infix 4 :: (v a p) (v a q) -> v Bool Expr                     | Ord a & isExpr p & isExpr q
    (<=.) infix 4 :: (v a p) (v a q) -> v Bool Expr                     | Ord a & isExpr p & isExpr q
    (>.)  infix 4 :: (v a p) (v a q) -> v Bool Expr                     | Ord a & isExpr p & isExpr q
    (>=.) infix 4 :: (v a p) (v a q) -> v Bool Expr                     | Ord a & isExpr p & isExpr q
    
class statements v where
    Skip :: v () Stmt
    (:.) infixr 1 :: (v a p) (v b q) -> v b Stmt
    If :: (v Bool p) Then (v a q) Else (v b r) -> v () Stmt             | isExpr p
    While :: (v Bool p) Repeat (v a q) -> v () Stmt                     | isExpr p
    For :: ((v a Upd) -> Do (v (Set a) p) (v b q)) -> v () Stmt         | isExpr p

:: Then = Then
:: Else = Else
:: Repeat = Repeat
:: In a b = In infix 0 a b
:: Do a b = Do infix 0 a b


//////////////////////////////////////////////////
// Views: Show                                  //
//////////////////////////////////////////////////

:: Show a p = Show (ShowResult -> ShowResult)
:: ShowResult = { nextid :: Int, indent :: Int, print :: [String] }

unShow :: (Show a p) -> (ShowResult -> ShowResult)
unShow (Show f) = f

initialShow :: ShowResult
initialShow = { nextid = 0, indent = 0, print = [] }

show :: (Show a p) -> [String]
show e = (unShow e initialShow).print

put :: a -> Show b c | toString a
put a = Show \s -> {s & print = [toString a : s.print]}

(+.+) infixl 5 :: (Show a p) (Show b q) -> Show c r
(+.+) (Show f) (Show g) = Show (f o g)

indent :: Show a b
indent = Show \s -> {s & indent = s.indent + 1}

unindent :: Show a b
unindent = Show \s -> {s & indent = s.indent - 1}

nl :: Show a b
nl = Show \s -> {s & print = [toString ['\n' : repeatn (2*s.indent) ' '] : s.print]}

freshVar :: ((Show a p) -> (Show b q)) -> Show b q
freshVar f = freshVar` (f o \i -> put ("v" +++ toString i))
    where freshVar` :: (Int -> (Show a p)) -> Show a p
          freshVar` f = Show \c -> unShow (f c.nextid) {c & nextid = c.nextid + 1}

binOp :: String (Show a p) (Show b q) -> Show c r
binOp op l r = put "(" +.+ l +.+ put (" " +++ op +++ " ") +.+ r +.+ put ")"


instance literals Show where
    Lit a = put a

// N.B. we chose for having no brackets on "a =. 2" statements 
// rather than correctly using brackets in "2 *. (a =. 2)". 
// This can most definitively be solved by actually working with binding power 
instance variables Show where
    (=.) v e = v +.+ put " = " +.+ e 
    Var f = freshVar \v -> let (x In s) = f v in put (type x +++ " ") +.+ v +.+ put " = " +.+ put x +.+ put ";" +.+ nl +.+ s

instance arithexprs Show where
    Size  s   = put "size( " +.+ s +.+ put " )"
    (+.)  a b = binOp "+"  a b
    (-.)  a b = binOp "-"  a b
    (*.)  a b = binOp "*"  a b
    (++.) a b = binOp "++" a b
    (.++) a b = binOp "++" a b
    (--.) a b = binOp "--" a b
    (.**) a b = binOp "**" a b

instance booleanexprs Show where
    (&&.) a b = binOp "&&" a b
    (||.) a b = binOp "||" a b
    ~. a = put "~" +.+ a

instance comparisons Show where
    (==.) a b = binOp "==" a b
    (<.)  a b = binOp "<"  a b
    (<=.) a b = binOp "<=" a b
    (>.)  a b = binOp ">"  a b
    (>=.) a b = binOp ">=" a b

instance statements Show where
    Skip = put "skip"
    (:.) a b = a +.+ put ";" +.+ nl +.+ b
    If c Then t Else e = put "if( " +.+ put " ) {" +.+ indent +.+ nl +.+ t +.+ put ";" +.+ unindent +.+ nl +.+
                         put "} else {" +.+ indent +.+ nl +.+ e +.+ put ";" +.+ unindent +.+ nl +.+ put "}"
    While c Repeat s = put "while( " +.+ put " ) {" +.+ indent +.+ nl +.+ s +.+ put ";" +.+ unindent +.+ nl +.+ put "}"
    For f = freshVar \v -> let (e Do s) = f v in put "for( " +.+ v +.+ put " in " +.+ e +.+ put " ) {" +.+ 
                                          indent +.+ nl +.+ s +.+ put ";" +.+ unindent +.+ nl +.+ put "}"

// Type class to add context restrictions for polymorphic functions on views
class DSL v | literals, variables, arithexprs, booleanexprs, comparisons, statements v

//////////////////////////////////////////////////
// Views: Eval                                  //
//////////////////////////////////////////////////

:: Eval a p = Eval ((ReadWrite a) State -> (Result a, State))
:: Result a :== Either String a
:: ReadWrite a = Read | Write a
:: State = { map :: 'Map'.Map Int Dynamic, vars :: Int }

unEval :: (Eval a p) -> ((ReadWrite a) State -> (Result a, State))
unEval (Eval f) = f

initialState :: State
initialState = { map = 'Map'.newMap, vars = 0 }

eval :: (Eval a p) -> Result a
eval (Eval f) = fst (f Read initialState)

readWriteVar :: Int (ReadWrite a) State -> (Result a, State) | TC a
readWriteVar n Read state =
    case 'Map'.get n state.map of
        (Just (v :: a^)) = (Right v, state)
        (Just d) = (Left ("Used variable is of wrong type: " +++ toString n), state)
        _        = (Left ("Variable " +++ toString n +++ " is undefined"), state)
readWriteVar n (Write a) state = (Right a, { state & map = 'Map'.put n (dynamic a) state.map})

// Monadic definitions
pure :: a -> Eval a p
pure a = Eval \r state -> (Right a, state)

(>>=) infixl 1 :: (Eval a p) (a -> Eval b q) -> Eval b q
(>>=) (Eval e) f =
    Eval \r state -> case e Read state of
        (Left err, state2) = (Left err, state2)
        (Right a, state2) = unEval (f a) r state2
        
(>>|) infixl 1 :: (Eval a p) (Eval b q) -> Eval b q
(>>|) a b = a >>= \_ -> b
        
(<*>) infixl 4 :: (Eval (a -> b) p) (Eval a q) -> Eval b r
(<*>) f a = f >>= \f` -> a >>= \a` -> pure (f` a`)

// Semantics
instance literals Eval where
    Lit a = pure a

instance variables Eval where
    (=.) v a = a >>= \a` -> Eval \r state -> unEval v (Write a`) state
    Var f = Eval \r state ->
                let (x In (Eval rest)) = f (Eval (readWriteVar state.vars))
                in rest Read {state & vars = inc state.vars, map = 'Map'.put state.vars (dynamic x) state.map}

instance arithexprs Eval where
    Size  s   = s >>= \s` -> pure ('Set'.size s`)
    (+.)  a b = a >>= \a` -> b >>= \b` -> pure (a` + b`)
    (-.)  a b = a >>= \a` -> b >>= \b` -> pure (a` - b`)
    (*.)  a b = a >>= \a` -> b >>= \b` -> pure (a` * b`)
    (++.) set v = set >>= \set` -> v   >>= \v`   -> pure ('Set'.insert v` set`)
    (.++) v set = v   >>= \v`   -> set >>= \set` -> pure ('Set'.insert v` set`)
    (--.) set v = set >>= \set` -> v   >>= \v`   -> pure ('Set'.delete v` set`)
    (.**) v set = v   >>= \v`   -> set >>= \set` -> undef //Complains about an internal overloading issue: pure ('Set'.mapSet (multiply v`) set`)
        where multiply :: a a -> a | * a
              multiply a1 a2 = a1 * a2
              
instance booleanexprs Eval where
    (&&.) a b = a >>= \a` -> b >>= \b` -> pure (a` && b`)
    (||.) a b = a >>= \a` -> b >>= \b` -> pure (a` || b`)
    ~. a = a >>= \a` -> pure (not a`)
    
instance comparisons Eval where
    (==.) a b = a >>= \a` -> b >>= \b` -> pure (a` == b`)
    (<.)  a b = a >>= \a` -> b >>= \b` -> pure (a` < b`)
    (<=.) a b = a >>= \a` -> b >>= \b` -> pure (a` <= b`)
    (>.)  a b = a >>= \a` -> b >>= \b` -> pure (a` > b`)
    (>=.) a b = a >>= \a` -> b >>= \b` -> pure (a` >= b`)

instance statements Eval where
    Skip = pure ()
    (:.) a b = a >>| b >>= \b` -> pure b`
    If c Then t Else e = c >>= \condition -> if condition (t >>| pure ()) (e >>| pure ())
    While c Repeat s = c >>= \condition` -> if condition` (s >>| While c Repeat s) (pure ())
    For f = undef

    //For f = freshVar \v -> let (e Do s) = f v in put "for( " +.+ v +.+ put " in " +.+ e +.+ put " ) {" +.+ 
    //                                      indent +.+ nl +.+ s +.+ put ";" +.+ unindent +.+ nl +.+ put "}"

//////////////////////////////////////////////////
// Testing programs                             //
//////////////////////////////////////////////////

fac2 :: Int -> v Int Stmt | DSL v
fac2 num =
    Var \n = num In
    Var \r = 1 In
    If (n >. Lit 0) Then (
        While (n >. Lit 1) Repeat (
            r =. r *. n :.
            n =. n -. Lit 1
        )
    ) Else (
        Skip
    ) :.
    r

findFirstNPrimes :: Int -> v (Set Int) Stmt | DSL v
findFirstNPrimes num =
    Var \primes = emptySet In
    Var \cur = 2 In
    // Loop until n primes are found
    While (Size primes <. Lit num) Repeat (
        Var \n = 2 In
        Var \hasDivisor = False In
        // Try to find an n * multiplier such that n * multiplier = cur, which means
        // cur is divisible by more than 1 and itself.
        While (n <. cur) Repeat (
            Var \multiplier = 1 In
            While (multiplier <. cur) Repeat (
                If (n *. multiplier ==. cur) Then (
                    hasDivisor =. Lit True
                ) Else (
                    Skip
                ) :.
                multiplier =. multiplier +. Lit 1
            ) :.
            n =. n +. Lit 1
        ) :.
        If hasDivisor Then Skip Else (primes =. primes ++. cur) :.
        cur =. cur +. Lit 1
    ) :.
    primes
    where emptySet :: Set Int
          emptySet = 'Set'.fromList []

setTest :: v Int Stmt | DSL v
setTest =
    Var \set = ('Set'.fromList []) In
    set =. New [5,1,1,2,2,3,3,4,4] :.
    Lit 42

testprog = Size (New [1,2] +. New [2,3])
Start 
    #prog1 = eval (fac2 5)
    #prog2 = case eval (findFirstNPrimes 15) of
        Left _ = []
        Right s = 'Set'.toList s
    #prog3 = eval setTest
    = prog3


