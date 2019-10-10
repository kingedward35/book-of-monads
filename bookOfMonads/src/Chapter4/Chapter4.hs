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

-- Exercise 4.1
zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f as bs = sequence' $ zipWith f as bs

replicateM :: Monad m => Int -> m a -> m [a]
replicateM n ma = sequence' $ replicate n ma

-- Chapter 4.1.1 Actions Without Value
void :: Functor m => m a -> m ()
void = fmap (\_ -> ()) -- can also use fmap const ()

-- mapM_ :: Monad m => (a -> m b) -> m a -> m ()
-- mapM_ f = void . fmap f
-- mapM_ f = sequence_ . map f
-- sequence_ = void . sequence'
when :: Monad m => Bool -> m () -> m ()
when cond action =
  if cond
    then action
    else return ()

when' :: Monad m => Bool -> m () -> m ()
when' True action = action
when' False _ = return ()

unless :: Monad m => Bool -> m () -> m ()
unless cond = when $ not cond

-- ifM :: Monad m => Bool -> m a -> ma -> m a
ifM :: Monad m => m Bool -> m b -> m b -> m b
ifM cond th el = do
  c <- cond
  if c
    then th
    else el
-- ifM' =
--   liftM3
--     (\c t e ->
--        if c
--          then t
--          else e)
