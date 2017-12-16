module e2_CashModelTest

import StdEnv, GenEq, e1_Gastje, e2_CashModel

derive string Euro
derive string Product

//////////////////////////////////////////////////
// Additive properties                          //
//////////////////////////////////////////////////

pEuroPlusAdditive :: Int Int -> EqualsProp Euro
pEuroPlusAdditive v1 v2 = (euro v1 + euro v2) =.= {euro = v1 + v2, cent = 0}

pEuroMinusAdditive :: Int Int -> EqualsProp Euro
pEuroMinusAdditive v1 v2 = (euro v1 - euro v2) =.= {euro = v1 - v2, cent = 0}

//////////////////////////////////////////////////
// Run tests                                    //
//////////////////////////////////////////////////

Start =    //["pEuroPlusAdditive: "] ++ test (\v1 v2 -> abs v1 < 10000 && abs v2 < 10000 ==> pEuroPlusAdditive v1 v2)
           ["pEuroPlusAdditive: "] ++ test pEuroPlusAdditive
        ++ ["pEuroMinusAdditive: "] ++ test pEuroMinusAdditive