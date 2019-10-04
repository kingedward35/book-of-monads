module Chapter4 where

-- Chapter 4.1
-- map (\name -> print ("Hello, " <> name)) ["Alejandro", "Elena"] -- Code will not compile because it is the wrong type
mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' _ [] = return []
mapM' f (x:xs) = do
  r <- f x
  rs <- mapM' f xs
  return (r : rs)

mapMApp :: Monad m => (a -> m b) -> [a] -> m [b]
mapMApp _ [] = return []
mapMApp f (x:xs) = (:) <$> f x <*> mapMApp f xs

-- forM ["Alejandro", "Elena"] (\ name -> print ("Hello, " <> name))
sequence' :: Monad m => [m a] -> m [a]
sequence' [] = return []
sequence' (x:xs) = do
  r <- x
  rs <- sequence' xs
  return (r : rs)

sequenceApp :: Monad m => [m a] -> m [a]
sequenceApp [] = return []
sequenceApp (x:xs) = (:) <$> x <*> sequenceApp xs

mapM'' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM'' f = sequence' . map f

zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM _ [] _ = return []
zipWithM _ _ [] = return []
zipWithM f as bs = sequence' $ zipWith f as bs

replicateM :: Monad m => Int -> m a -> m [a]
replicateM 0 _ = return []
replicateM n ma = sequence' $ replicate n ma
