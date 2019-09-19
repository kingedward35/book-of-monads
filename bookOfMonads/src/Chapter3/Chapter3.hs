module Chapter3 where

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
testFmap :: Monad m => (b -> c) -> m b -> m c
testFmap f fa = return f <*> fa

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
