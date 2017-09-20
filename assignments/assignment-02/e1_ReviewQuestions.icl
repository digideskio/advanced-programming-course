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
    Checking for equality of the constructor names could be
    better; then, values of different types with the same 
    generic representation (but different constructor name),
    will not actually be treated as equal, even if defined
    manually, without the type aliases such as "ListG", 
    "RoseG", etc.
    
    Currently, 
    
    CONS "Something" UNIT == CONS "SomethingElse" UNIT
    
    would evaluate to True.
    
    3.
    The generic representation of [] is:
    LEFT (CONS "Nil" UNIT)
    
    The generic representation of Leaf is:
    LEFT (CONS "Leaf" UNIT)
    
    As such, the representations are the same up to the
    constructor name given in CONS. However,
    
    fromBin Leaf == fromList []
    
    is undefined: fromBin Leaf has type BinG a, whereas 
    fromList [] has type ListG b.
    
    There is no definition of == with type
    (BinG a) (ListG b) -> Bool.
    
    Only if the representations are defined manually, will
    the equal operator return True:
    
    CONS "Leaf" UNIT == CONS "Nil" UNIT
    
    evaluates to True.
    
    This can be solved by checking for the constructor
    name in the CONS instance of ==.
*/

Start = undef
