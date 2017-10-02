implementation module Assignment04

(<$>) infixl 4 :: (a -> b) (f a) -> (f b) | Functor f
(<$>) f fa = fmap f fa

(*>) infixl 4 :: (f a) (f b) -> f b | Applicative f
(*>) fa fb = (\x y. y) <$> fa <*> fb

(>>=) infixl 1 :: (m a) (a -> m b) -> m b | Monad m
(>>=) ma a2mb = bind ma a2mb

(>>|) infixl 1 :: (m a) (m b) -> m b | Monad m
(>>|) ma mb = ma >>= \_ -> mb

rtrn :: a -> m a | Monad m
rtrn a = pure a
