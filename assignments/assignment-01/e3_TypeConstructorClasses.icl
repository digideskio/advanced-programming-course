/* Jordi Riemens    s4243064
 * Thomas Churchman s4206606
 */

module e3_TypeConstructorClasses

import StdEnv

:: Bin a = Leaf | Bin (Bin a) a (Bin a)

Start = undef
