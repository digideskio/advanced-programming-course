/* Jordi Riemens    s4243064
 * Thomas Churchman s4206606
 */
 
module e1_ReviewQuestions

import StdEnv

/*
    1.
    Technically it is fine to write any of the three alternatives;
    they are all correct.
    
    The important thing to note is that the UNIT type has only a
    single constructor, UNIT, taking no arguments. As such, it
    provides no real alternatives.
    
    This means that in
    
    instance == UNIT where
        (==) UNIT UNIT = TRUE
        (==) x    y    = FALSE
        
    the second definition is never used.
    
    In
    
    instance == UNIT where (==) x y = TRUE
    
    x and y will always be equal, by the definition of the UNIT
    type constructors.
    
    For clarity, we would opt for using the original definition
    provided in the slides. This is also consistent with the
    definition for e.g. PAIR, where only a single alternative
    is given, as there is only a single constructor.
    
    2.
    Checking for equality of the constructor names would 
    technically be better; then, values of different types 
    with the same generic representation (but different 
    constructor name), will not actually be treated as 
    equal when defined manually, without the type aliases 
    such as "ListG", "RoseG", etc. Currently,     
        CONS "Something" UNIT == CONS "SomethingElse" UNIT
    would evaluate to True, but it wouldn't if this is enforced.
    
    However, as the generic type mechanism is meant to 
    be used in the background, hidden from the user,
    this should not happen. Instead, what would happen
    when a List and Rose are compared, is that the
    compiler sees that there is no instance for a
    List == Rose equality test, and complain about this
    to the user with a type error. As, when used as 
    envisaged, only elements of the same types are 
    compared in the generic domain, and within such
    types constructor names play no role (because
    EITHER takes care of the "choice" between different
    constructors), comparing constructor names would
    normally not be needed.
    
    3.
    The generic representation of [] is:
    LEFT (CONS "Nil" UNIT)
    
    The generic representation of Leaf is:
    LEFT (CONS "Leaf" UNIT)
    
    As such, the representations are the same up to the
    constructor name given in CONS. However, their type is
	not, as the right-hand sides of the EITHER in types
	BinG and ListG are different. Thus,
        fromBin Leaf == fromList []
    is undefined, and gives a type error.
	Similarly, there is no instance of (==)
    between Bin a and [a], and so
        Leaf == []
    is also undefined and gives a type error.
	
	Extrapolating, however, assume two algebraic data
	types with the same generic layout:
	    :: TypeA = A | B
		:: TypeB = C | D
	which will both be transformed to 
	the generic representation
	EITHER (CONS String UNIT) (CONS String UNIT)
	but with different Strings. In this case,
	    fromTypeA A == fromTypeB C
	is a valid statement and evaluates to TRUE,
	as the compared generic types are the same,
	as are their contents (apart from the constructor
	name strings). However,
	    A == C
	will still give a type error even when the
	instances of (==) TypeA and (==) TypeC,
	using equality on their generic forms, are 
	defined. Thus, the envisaged usage of generics
	to define equality does not assume that 
	generic representations are different either.
	
    Note that, even for Bin and Leaf, if the 
	representations are defined manually, 
	== will in fact return True:
        CONS "Leaf" UNIT == CONS "Nil" UNIT
    evaluates to True. Again, as users are not
	meant to interact with this mechanism,
	this is not a major problem.
*/

Start = undef
