module e2_CashModelTest

/*
 * Jordi Riemens    s4243064
 * Thomas Churchman s4206606
 */

import StdEnv, GenEq, e1_Gastje, e2_CashModel

derive gen Euro, Product, []
derive string Euro, Product, []

//////////////////////////////////////////////////
// Integer properties                           //
//////////////////////////////////////////////////

pEuroPlusCorrectnessInteger :: Int Int -> EqualsProp Euro
pEuroPlusCorrectnessInteger v1 v2 = (euro v1 + euro v2) =.= {euro = v1 + v2, cent = 0}

pEuroMinusCorrectnessInteger :: Int Int -> EqualsProp Euro
pEuroMinusCorrectnessInteger v1 v2 = (euro v1 - euro v2) =.= {euro = v1 - v2, cent = 0}

pEuroNegateCorrectnessInteger :: Int -> EqualsProp Euro
pEuroNegateCorrectnessInteger v1 = (~(euro v1)) =.= {euro = ~v1, cent = 0}

//////////////////////////////////////////////////
// Tuple properties                             //
//////////////////////////////////////////////////

pEuroPlusCommutativeTuple :: (Int,Int) (Int,Int) -> EqualsProp Euro
pEuroPlusCommutativeTuple v1 v2 = (euro v1 + euro v2) =.= (euro v2 + euro v1)

pEuroMinusCommutativeTuple :: (Int,Int) (Int,Int) -> EqualsProp Euro
pEuroMinusCommutativeTuple v1 v2 = (euro v1 - euro v2) =.= ~(euro v2 - euro v1)

pEuroPlusZeroTuple :: (Int,Int) -> EqualsProp Euro
pEuroPlusZeroTuple v1 = (euro v1 + zero) =.= (euro v1)

pEuroMinusZeroTuple :: (Int,Int) -> EqualsProp Euro
pEuroMinusZeroTuple v1 = (euro v1 - zero) =.= (euro v1)

pEuroNegateInverseTuple :: (Int,Int) -> EqualsProp Euro
pEuroNegateInverseTuple v1 = (euro v1 + ~(euro v1)) =.= zero

//////////////////////////////////////////////////
// Rem properties                               //
//////////////////////////////////////////////////

pActionRemValueOffWhenPresent :: Product [Product] -> FilterProp (EqualsProp Euro)
pActionRemValueOffWhenPresent p list = (isMember p list) ==> (value (model list (Rem p))) =.= (euro list - euro p)
    where value modelresult = euro (fst modelresult)

pActionRemValuePreservedWhenMissing :: Product [Product] -> FilterProp (EqualsProp Euro)
pActionRemValuePreservedWhenMissing p list = (not (isMember p list)) ==> (value (model list (Rem p))) =.= (euro list)
    where value modelresult = euro (fst modelresult)

//////////////////////////////////////////////////
// Run tests                                    //
//////////////////////////////////////////////////

Start =    //["pEuroPlusCorrectnessInteger: "] ++ test (\v1 v2 -> abs v1 < 10000 && abs v2 < 10000 ==> pEuroPlusCorrectnessInteger v1 v2)
           ["pEuroPlusCorrectnessInteger: "] ++ test pEuroPlusCorrectnessInteger
        ++ ["pEuroMinusCorrectnessInteger: "] ++ test pEuroMinusCorrectnessInteger
        ++ ["pEuroNegateCorrectnessInteger: "] ++ test pEuroNegateCorrectnessInteger /*
        ++ ["pEuroPlusCommutativeTuple: "] ++ test pEuroPlusCommutativeTuple
        ++ ["pEuroMinusCommutativeTuple: "] ++ test pEuroMinusCommutativeTuple
        ++ ["pEuroPlusZeroTuple: "] ++ test pEuroPlusZeroTuple
        ++ ["pEuroMinusZeroTuple: "] ++ test pEuroMinusZeroTuple
        ++ ["pEuroNegateInverseTuple: "] ++ test pEuroNegateInverseTuple */
        ++ ["pActionRemValueOffWhenPresent: "] ++ test pActionRemValueOffWhenPresent
        ++ ["pActionRemValuePreservedWhenMissing: "] ++ test pActionRemValuePreservedWhenMissing 