implementation module Monad

/*
 * Jordi Riemens    s4243064
 * Thomas Churchman s4206606
 */

(*>) infixl 4 :: (f a) (f b) -> f b | Applicative f
(*>) fa fb = (\x y. y) <$> fa <*> fb
