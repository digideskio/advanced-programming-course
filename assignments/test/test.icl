module test

import StdEnv

// (o) f g :== \ x -> f (g x)
     
:: Y a = Y ((Y a) -> a)
Y` f = (\g -> g (Y g)) (\x -> (f o ((\(Y g) -> g) x)) x)
//Y` f = f (Y` f)
F f n = if (n == 0) 1 (n * (f (n-1)))
//Start = Y` F 3
//Start = (\(Y g) -> g) (Y (\x -> (F o ((\(Y g) -> g) x)) x))
//Start = \x -> (F o ((\(Y g) -> g) x)) x
//Start = (\(Y g) -> g) (Y (\x -> ((\(Y g) -> g) x) x))
Start = (\x -> ((\(Y g) -> g) x) x)