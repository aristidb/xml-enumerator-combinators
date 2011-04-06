module Text.XML.Enumerator.Combinators.General
where
  
import Control.Monad (liftM)

-- | Like 'choose', but also returns the list of elements that were /not/ chosen.
chooseSplit :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m (Maybe (b, [a]))
chooseSplit f xs = go xs []
    where
      go [] _ = return Nothing
      go (i:is) is' = do
        x <- f i
        case x of
          Nothing -> go is (i : is')
          Just a -> return $ Just (a, is' ++ is)

-- | Permute all parsers until none return 'Just'.
permute :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m (Maybe [b])          
permute _ [] = return (Just [])
permute f is = do
    x <- chooseSplit f is
    case x of
      Nothing -> return Nothing
      Just (a, is') -> fmap (a:) `liftM` permute f is'

-- | Permute all parsers until none return 'Just', but always test some fallback parsers.
permuteFallback :: (Monad m) => m (Maybe [b]) -> (a -> m (Maybe b)) -> [a] -> m (Maybe [b])
permuteFallback _  _ [] = return (Just [])
permuteFallback fb f is = do
    x <- chooseSplit f is
    case x of
      Nothing -> do y <- fb
                    case y of
                      Nothing -> return Nothing
                      Just as -> fmap (as ++) `liftM` permuteFallback fb f is
      Just (a, is') -> fmap (a:) `liftM` permuteFallback fb f is'
