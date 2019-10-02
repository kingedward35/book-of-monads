module Chapter3 where

import Prelude hiding (Applicative(..))

-- Chapter 3.1 Lift2, Lift3, ..., Ap
plus :: Maybe Int -> Maybe Int -> Maybe Int
plus x y = do
  a <- x
  b <- y
  return $ a + b

plusMaybeToInt :: Maybe Int -> Int
plusMaybeToInt Nothing = 0
plusMaybeToInt (Just n) = n

lift2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
lift2 f x y = do
  a <- x
  b <- y
  return $ f a b

ap :: Monad m => m (b -> c) -> m b -> m c
ap mbc mb = do
  bc <- mbc
  b <- mb
  return $ bc b

-- An Example of using fmap and ap: (+) <$> (Just 12) <*> (Just 77)
---------------------------------------
-- Chapter 3.2 Applicatives
-- class Functor f =>
--       Applicative f
--   where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b
-- testFmap :: Monad m => (b -> c) -> m b -> m c
-- testFmap f fa = return f <*> fa
class Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

instance Applicative ((,) a) where
  pure x = (mempty, x)
  (u, f) <*> (v, x) = (u <> v, f x)
  -- pure x = (x, _)
  -- (<*>) (a, _) (a', _) = (a a', _)

newtype ZipList a =
  ZipList
    { getZipList :: [a]
    }
  deriving (Show)

instance Applicative ZipList where
  pure x = ZipList (repeat x) -- infinite list of x values
  ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)

instance Functor ZipList where
  fmap _ (ZipList []) = ZipList []
  fmap f (ZipList (x:xs)) = ZipList (f x : fmap f xs)

randomZipList :: ZipList Integer
randomZipList = ZipList [1 .. 10]

---------------------------------------------
-- Chapter 3.3 Applicative Style
-- do x <- [1 .. 3]       this can be written in the following fashion
--    y <- [4 .. 6]       (+) <$> [1..3] <*> [4..6]
--    return $ x + y
----------------------------------------------
-- Chapter 3.3.1 Applicative Goodies
(<$) :: Functor f => a -> f b -> f a
(<$) = undefined

(<*) :: Applicative f => f a -> f b -> f a
(<*) = undefined

(*>) :: Applicative f => f a -> f b -> f b
(*>) = undefined

-- map toUpper <$> validateName name <*> validateAge age     -- this code is invalid
-- Option 1
-- do validateAge age
--    map toUpper <$> validateName name
-- Option 2
-- do validateAge age
--    m <- validateName name
--    return $ map toUpper m
-- Some other ways of potential validation
-- map toUpper <$> validateName name <* validateAge age
-- map toUpper <$ validateAge age <*> validateName name
-- (\n -> Person n 20) <$> validateName name
-- flip Person 20 <$> validateName name
-- Person <$> validateName name <*> pure 20
--------------------------------------------
-- Chapter 3.4 Definition Using Tuples
class Functor f =>
      Monoidal f
  where
  unit :: f ()
  (**) :: f a -> f b -> f (a, b)

convertTriples :: (a, b, c) -> (a, (b, c))
convertTriples (a, b, c) = (a, (b, c))

convertQuadruples :: (a, b, c, d) -> (a, (b, (c, d)))
convertQuadruples (a, b, c, d) = (a, (b, (c, d)))

-- pure :: a -> f a
-- pure x = fmap (\_ -> x) ()
-- (<*>) :: f (a -> b) -> f a -> f b
-- f <*> x = fmap (\(g, y) -> g y) (f ** x)
-- Excercise 3.5
unit' :: Applicative f => f ()
unit' = pure ()

(**=) :: Applicative f => f a -> f b -> f (a, b)
-- (**=) = undefined
fa **= fb = (,) <$> fa <*> fb
