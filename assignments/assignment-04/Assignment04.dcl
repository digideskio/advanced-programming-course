definition module Assignment04

import StdEnv

class Functor f where
    fmap :: (a -> b) (f a) -> f b

class Applicative f | Functor f where
    pure            :: a -> f a
    (<*>) infixl 4  :: (f (a -> b)) (f a) -> f b

class fail m | Applicative m where
    fail :: m a
    guard :: Bool -> m a | fail m
    guard b :== if b (pure undef) fail

class Monad m | Applicative m where
    bind :: (m a) (a -> m b) -> m b

class OrMonad m where
    (<|>) infixl 0 :: (m a) (m a) -> m a

(<$>) infixl 4 :: (a -> b) (f a) -> f b   | Functor f
(*>)  infixl 4 :: (f a) (f b) -> f b      | Applicative f
(>>=) infixl 1 :: (m a) (a -> m b) -> m b | Monad m
(>>|) infixl 1 :: (m a) (m b) -> m b      | Monad m
rtrn           :: a -> m a                | Monad m
