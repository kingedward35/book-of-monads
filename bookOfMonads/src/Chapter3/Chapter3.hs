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
