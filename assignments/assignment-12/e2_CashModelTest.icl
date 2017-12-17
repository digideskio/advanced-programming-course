module e2_CashModelTest

/*
 * Jordi Riemens    s4243064
 * Thomas Churchman s4206606
 */

import StdEnv, GenEq, GenBimap, e1_Gastje, e2_CashModel

derive gen Euro, Product, [], (,)
derive string Euro, Product, [], (,)

//////////////////////////////////////////////////
// Integer properties                           //
//////////////////////////////////////////////////

pEuroPlusCorrectnessInteger :: Int Int -> EqualsProp Euro
pEuroPlusCorrectnessInteger v1 v2 = (euro v1 + euro v2) =.= {euro = v1 + v2, cent = 0}
/* pEuroPlusCorrectnessInteger: Fail for: 0 2147483647
   =.= evaluation:
      Left-hand side: {Euro|euro = -1  cent = 0 }
      Right-hand side: {Euro|euro = 2147483647  cent = 0 } */
/* pEuroPlusCorrectnessInteger (constrained): Passed */

pEuroMinusCorrectnessInteger :: Int Int -> EqualsProp Euro
pEuroMinusCorrectnessInteger v1 v2 = (euro v1 - euro v2) =.= {euro = v1 - v2, cent = 0}
/* pEuroMinusCorrectnessInteger: Fail for: 0 2147483647
   =.= evaluation:
      Left-hand side: {Euro|euro = 1  cent = 0 }
      Right-hand side: {Euro|euro = -2147483647  cent = 0 } */
/* pEuroMinusCorrectnessInteger (constrained): Passed */

pEuroNegateCorrectnessInteger :: Int -> EqualsProp Euro
pEuroNegateCorrectnessInteger v1 = (~(euro v1)) =.= {euro = ~v1, cent = 0}
/* pEuroNegateCorrectnessInteger: Passed */
/* pEuroNegateCorrectnessInteger (constrained): Passed */


//////////////////////////////////////////////////
// Tuple properties                             //
//////////////////////////////////////////////////

pEuroPlusCommutativeTuple :: (Int,Int) (Int,Int) -> EqualsProp Euro
pEuroPlusCommutativeTuple v1 v2 = (euro v1 + euro v2) =.= (euro v2 + euro v1)
/* pEuroPlusCommutativeTuple: Passed */
/* pEuroPlusCommutativeTuple (constrained): Passed */

pEuroMinusCommutativeTuple :: (Int,Int) (Int,Int) -> EqualsProp Euro
pEuroMinusCommutativeTuple v1 v2 = (euro v1 - euro v2) =.= ~(euro v2 - euro v1)
/* pEuroMinusCommutativeTuple: Fail for: (_Tuple2 1 0) (_Tuple2 1 -2147483648)
   =.= evaluation:
      Left-hand side: {Euro|euro = -21474836  cent = -48 }
      Right-hand side: {Euro|euro = 21474836  cent = -48 } */
/* pEuroMinusCommutativeTuple (constrained): Passed */

pEuroPlusZeroTuple :: (Int,Int) -> EqualsProp Euro
pEuroPlusZeroTuple v1 = (euro v1 + zero) =.= (euro v1)
/* pEuroPlusZeroTuple: Fail for: (_Tuple2 0 1)
   =.= evaluation:
      Left-hand side: {Euro|euro = 0  cent = 0 }
      Right-hand side: {Euro|euro = 0  cent = 1 } */
/* pEuroPlusZeroTuple (constrained) Fail for: (_Tuple2 0 1)
   =.= evaluation:
      Left-hand side: {Euro|euro = 0  cent = 0 }
      Right-hand side: {Euro|euro = 0  cent = 1 } */

pEuroMinusZeroTuple :: (Int,Int) -> EqualsProp Euro
pEuroMinusZeroTuple v1 = (euro v1 - zero) =.= (euro v1)
/* pEuroMinusZeroTuple: Fail for: (_Tuple2 0 1)
   =.= evaluation:
      Left-hand side: {Euro|euro = 0  cent = 0 }
      Right-hand side: {Euro|euro = 0  cent = 1 } */
/* pEuroMinusZeroTuple (constrained): Fail for: (_Tuple2 0 1)
   =.= evaluation:
      Left-hand side: {Euro|euro = 0  cent = 0 }
      Right-hand side: {Euro|euro = 0  cent = 1 } */

pEuroNegateInverseTuple :: (Int,Int) -> EqualsProp Euro
pEuroNegateInverseTuple v1 = (euro v1 + ~(euro v1)) =.= zero
/* pEuroNegateInverseTuple: Fail for: (_Tuple2 -2147483648 1)
   =.= evaluation:
      Left-hand side: {Euro|euro = 0  cent = 2 }
      Right-hand side: {Euro|euro = 0  cent = 0 } */
/* pEuroNegateInverseTuple (constrained): Passed */


//////////////////////////////////////////////////
// Rem properties                               //
//////////////////////////////////////////////////

pActionRemValueOffWhenPresent :: Product [Product] -> FilterProp (EqualsProp Euro)
pActionRemValueOffWhenPresent p list = (isMember p list) ==> (value (model list (Rem p))) =.= (euro list - euro p)
    where value modelresult = euro (fst modelresult)
/* pActionRemValueOffWhenPresent: Passed */

pActionRemValuePreservedWhenMissing :: Product [Product] -> FilterProp (EqualsProp Euro)
pActionRemValuePreservedWhenMissing p list = (not (isMember p list)) ==> (value (model list (Rem p))) =.= (euro list)
    where value modelresult = euro (fst modelresult)
/* pActionRemValuePreservedWhenMissing: Passed */


//////////////////////////////////////////////////
// Run tests                                    //
//////////////////////////////////////////////////

// These test modifiers are there so we can test whether found issues
// are caused only by over/underflow, or if it is wrong even for non-edge cases
constrained :: (Int -> a) -> (Int -> FilterProp a)
constrained f = \v1 -> v1 > -10000 && v1 < 10000 ==> f v1

constrainedtup :: ((Int,Int) -> a) -> ((Int,Int) -> FilterProp a)
constrainedtup f = \v1 -> fst v1 > -10000 && fst v1 < 10000 && snd v1 > -10000 && snd v1 < 10000 ==> f v1

constrained2 :: (Int Int -> a) -> (Int Int -> FilterProp a)
constrained2 f = \v1 v2 -> v1 > -10000 && v1 < 10000 && v2 > -10000 && v2 < 10000 ==> f v1 v2

constrained2tup :: ((Int,Int) (Int,Int) -> a) -> ((Int,Int) (Int,Int) -> FilterProp a)
constrained2tup f = \v1 v2 -> fst v1 > -10000 && fst v1 < 10000 && fst v2 > -10000 && fst v2 < 10000
                           && snd v1 > -10000 && snd v1 < 10000 && snd v2 > -10000 && snd v2 < 10000 
                          ==> f v1 v2

Start =    ["pEuroPlusCorrectnessInteger: "] ++ test pEuroPlusCorrectnessInteger
        ++ ["pEuroMinusCorrectnessInteger: "] ++ test pEuroMinusCorrectnessInteger
        ++ ["pEuroNegateCorrectnessInteger: "] ++ test pEuroNegateCorrectnessInteger
        ++ ["pEuroPlusCorrectnessInteger (constrained): "] ++ test (constrained2 pEuroMinusCorrectnessInteger)
        ++ ["pEuroMinusCorrectnessInteger (constrained): "] ++ test (constrained2 pEuroMinusCorrectnessInteger) 
        ++ ["pEuroNegateCorrectnessInteger (constrained): "] ++ test (constrained pEuroNegateCorrectnessInteger) 
        ++ ["pEuroPlusCommutativeTuple: "] ++ test pEuroPlusCommutativeTuple
        ++ ["pEuroMinusCommutativeTuple: "] ++ test pEuroMinusCommutativeTuple
        ++ ["pEuroPlusZeroTuple: "] ++ test pEuroPlusZeroTuple
        ++ ["pEuroMinusZeroTuple: "] ++ test pEuroMinusZeroTuple
        ++ ["pEuroNegateInverseTuple: "] ++ test pEuroNegateInverseTuple 
        ++ ["pEuroPlusCommutativeTuple (constrained): "] ++ test (constrained2tup pEuroPlusCommutativeTuple)
        ++ ["pEuroMinusCommutativeTuple (constrained): "] ++ test (constrained2tup pEuroMinusCommutativeTuple)
        ++ ["pEuroPlusZeroTuple (constrained) "] ++ test (constrainedtup pEuroPlusZeroTuple)
        ++ ["pEuroMinusZeroTuple (constrained): "] ++ test (constrainedtup pEuroMinusZeroTuple)
        ++ ["pEuroNegateInverseTuple (constrained): "] ++ test (constrainedtup pEuroNegateInverseTuple) 
        ++ ["pActionRemValueOffWhenPresent: "] ++ test pActionRemValueOffWhenPresent
        ++ ["pActionRemValuePreservedWhenMissing: "] ++ test pActionRemValuePreservedWhenMissing 