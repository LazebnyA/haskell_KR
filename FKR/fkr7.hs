import Prelude hiding (Functor, Applicative, Monad)

class Functor f where
  fmap :: (a -> b) -> f a -> f b

-- List [a] is a Functor.
instance Functor [] where
  fmap = map


class (Functor f) => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

-- List [a] is an Applicative Functor.
instance Applicative [] where
  pure x    = [x]
  fs <*> xs = [f x | f <- fs, x <- xs]


class (Applicative m) => Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b

-- List [a] is a monad!
instance Monad [] where
  return x = [x]
  -- concatMap ::  (a -> [b]) -> [a] -> [b]
  (>>=) xs f = concatMap f xs
  -- or (>>=) xs f = foldl (++) [] (map f x)


main = do
  let myList = ["apple", "banana", "orange"]
      modifiedList = myList Main.>>= (\x -> [x ++ "s"])
  print modifiedList
  -- it works!