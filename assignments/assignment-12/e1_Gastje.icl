module e1_Gastje

/*
 * Jordi Riemens    s4243064
 * Thomas Churchman s4206606
 *
 * Based on:
 * Pieter Koopman, pieter@cs.ru.nl
 * Skeleton for Advanced Programming, week 12, 2017
 */
 
import StdEnv, StdGeneric, GenEq, Data.Eq

test :: p -> [String] | prop p
test p = check 1000 (holds p prop0)

check :: Int [Prop] -> [String]
check n [] = ["Proof\n"]
check 0 l  = ["Passed\n"]
check n [p:x] | p.bool
	= check (n-1) x
	= ["Fail for: ":reverse ["\n":p.info]]

class prop a where holds :: a Prop -> [Prop]

instance prop Bool where holds b p = [{p & bool = b}]

// crashes the iTask compiler if there is no dcl module :(
instance prop (a->b) | prop b & testArg a 
where
	holds f p = diagonal [holds (f a) {p & info = [" ",string{|*|} a:p.info]} \\ a <- gen{|*|}]

class testArg a | gen{|*|}, string{|*|}, gEq{|*|} a 

:: Prop =
	{ bool :: Bool
	, info :: [String]
	}
prop0 = {bool = True, info = []}

generic gen a :: [ a ]
gen{|Int|}  = [0,1,-1,maxint,minint,maxint-1,minint+1:[j\\i<-[2..], j<-[i,~i]]]
gen{|Bool|} = [True,False]
gen{|Char|} = [' '..'~'] ++ ['\t\n\b']
gen{|UNIT|} = [UNIT]
gen{|PAIR|}   f g	= map (\(a,b)=PAIR a b) (diag2 f g)
gen{|EITHER|} f g = merge (map RIGHT g) (map LEFT f)
where
  merge [a:x] ys = [a: merge ys x]
  merge []    ys = ys
gen{|CONS|}   f  = map CONS f
gen{|OBJECT|} f  = map OBJECT f
gen{|RECORD|} f  = map RECORD f
gen{|FIELD|}  f  = map FIELD f

generic string a :: a -> String
string{|Int|} i = toString i
string{|Bool|} b = toString b
string{|Char|} c = toString ['\'',c,'\'']
string{|UNIT|} _ = ""
string{|PAIR|} f g (PAIR x y) = f x + " " + g y
string{|EITHER|} f g (LEFT x) = f x
string{|EITHER|} f g (RIGHT y) = g y
string{|CONS of gcd|} f (CONS x) | gcd.gcd_arity > 0
	= "(" + gcd.gcd_name + " " + f x + ")"
	= gcd.gcd_name
string{|OBJECT|} f (OBJECT x) = f x
string{|RECORD of grd|} f (RECORD x) = "{" + grd.grd_name + "|" + f x + "}"
string{|FIELD of gfd|} f (FIELD x) = gfd.gfd_name + " = " + f x + " "

maxint :: Int
maxint =: IF_INT_64_OR_32 (2^63-1) (2^31-1) //2147483647

minint :: Int
minint =: IF_INT_64_OR_32 (2^63) (2^31) //-2147483648

instance + String where + s t = s +++ t

diagonal :: [[a]] -> [a]
diagonal list = f 1 2 list []
where
	f n m [] [] = []
	f 0 m xs ys = f m (m+1) (rev ys xs) []
	f n m [] ys = f m (m+1) (rev ys []) []
	f n m [[x:r]:xs] ys = [x: f (n-1) m xs [r:ys]]
	f n m [[]:xs] ys = f (n-1) m xs ys
	
	rev []    accu = accu
	rev [x:r] accu = rev r [x:accu]

//////////////////////////////////////////////////
// Exercise 1                                   //
//////////////////////////////////////////////////

// Combine a function to a property with its test cases
(For) infixl :: (a->b) [a] -> (a->b, [a]) | prop b
(For) f cases = (f, cases)

// Implement a property instance for a function to a property
// combined with its test cases
instance prop (a->b, [a]) | prop b & testArg a 
where
	holds (f, cases) p = diagonal [holds (f a) {p & info = [" ",string{|*|} a:p.info]} \\ a <- cases]

//////////////////////////////////////////////////
// Exercise 2                                   //
//////////////////////////////////////////////////

:: FilterProp a b = FilterProp (a->Bool) (a->b) 

// Combine a function to a property with a way to filter arguments
(==>) infixl :: (a->Bool) (a->b) -> FilterProp a b | prop b
(==>) filter` f = FilterProp filter` f

// Implement a property instance for a function to a property
// combined with its argument filter
instance prop (FilterProp a b) | prop b & testArg a 
where
	holds (FilterProp filter` f) p = diagonal [holds (f a) {p & info = [" ",string{|*|} a:p.info]} \\ a <- (filter filter` gen{|*|})]

//////////////////////////////////////////////////
// Examples                                     //
//////////////////////////////////////////////////

pUpper :: Char -> Bool
pUpper c = c /= toUpper c

Start = ["pUpper1: "] ++ test (pUpper For ['a'..'z'])
        ++ ["pUpper2: "] ++ test ((\c. isLower c) ==> pUpper)
