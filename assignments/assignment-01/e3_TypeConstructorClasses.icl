/* Jordi Riemens    s4243064
 * Thomas Churchman s4206606
 */

module e3_TypeConstructorClasses

import StdEnv

:: Bin a = Leaf | Bin (Bin a) a (Bin a)

class Container t where
    Cinsert   :: a (t a) -> t a      | < a
    Ccontains :: a (t a) -> Bool     | <, Eq a
    Cshow     ::   (t a) -> [String] | toString a
    Cnew      :: t a

/*-------------------------------------------------*/
    
Start = undef
