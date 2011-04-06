module Text.XML.Enumerator.Combinators.Tags
where

import           Control.Applicative       ((<$>))
import           Control.Arrow             (second)
import           Control.Monad             (guard, join)
import           Data.XML.Types
import           Data.Enumerator           (Iteratee)
import qualified Data.Map                  as Map
import qualified Text.XML.Enumerator.Parse as P

-- | Statefully and efficiently parse a list of tags.
-- 
-- The first parameter is a function that, given state and an element name, returns
-- either 'Nothing', to indicate that the element is invalid, or a pair of attribute
-- and element content parsers in 'Just'. 
-- 
-- The second parameter is a function that, given the current state, returns a
-- "fallback" parser to be executed when no valid element has been found.
-- 
-- The third parameter is the initial state.
-- 
-- This function updates the state as it goes along, but it also accumulates a list of
-- elements as they occur.
tags :: (Monad m)
        => (a -> Name -> Maybe (P.AttrParser b, b -> Iteratee Event m (Maybe (a, Maybe c))))
        -> (a -> Iteratee Event m (Maybe (a, Maybe c)))
        -> a
        -> Iteratee Event m (a, [c])
tags f fb s' = go s'
    where go s = do
            t <- fmap join (P.tag (f s) (\(attr, sub) -> sub <$> attr) id) `P.orE` fb s
            case t of
              Nothing -> return (s, [])
              Just (s2, Nothing) -> go s2
              Just (s2, Just a) -> second (a:) `fmap` go s2

-- | Parse a permutation of tags.
-- 
-- The first parameter is a function to preprocess Names for equality testing, because
-- sometimes XML documents contain inconsistent naming. This allows the user to deal
-- with it.
-- 
-- The second parameter is a map of tags to attribute and element content parsers.
-- 
-- The third parameter is a fallback parser. The outer Maybe indicates whether it succeeds,
-- and the inner Maybe whether an element should be added to the output list.
-- 
-- This function accumulates a list of elements for each step that produces one.
tagsPermute :: (Monad m, Ord k)
               => (Name -> k)
               -> Map.Map k (P.AttrParser a, a -> Iteratee Event m (Maybe b))
               -> Iteratee Event m (Maybe (Maybe b))
               -> Iteratee Event m (Maybe [b])
tagsPermute f m fb = do
      (rest, result) <- tags go (\s -> fmap (\a -> (s, a)) <$> fb) m
      return (guard (Map.null rest) >> Just result)
    where go s name = case Map.lookup k s of
                        Nothing          -> Nothing
                        Just (attr, sub) -> Just (attr, fmap adaptSub . sub)
              where k = f name
                    adaptSub Nothing = Nothing
                    adaptSub a       = Just (Map.delete k s, a)

-- | Specifies how often an element may repeat.
data Repetition
    = Repeat { 
        repetitionNeedsMore :: Bool
      , repetitionAllowsMore :: Bool
      , repetitionConsume :: Repetition
      }

-- | Element may never occur.
repeatNever :: Repetition
repeatNever = Repeat False False repeatNever

-- | Element may occur exactly once.
repeatOnce :: Repetition
repeatOnce = Repeat True True repeatNever

-- | Element may occur up to once.
repeatOptional :: Repetition
repeatOptional = Repeat False True repeatNever

-- | Element may occur any number of times.
repeatMany :: Repetition
repeatMany = Repeat False True repeatMany

-- | Element may occur at least once.
repeatSome :: Repetition
repeatSome = Repeat True True repeatMany

-- | Parse a permutation of tags, with some repeating elements.
-- 
-- The first parameter is a function to preprocess Names for equality testing, because
-- sometimes XML documents contain inconsistent naming. This allows the user to deal
-- with it.
-- 
-- The second parameter is a map of tags to attribute and element content parsers.
-- It also specifies how often elements may repeat.
-- 
-- The third parameter is a fallback parser. The outer Maybe indicates whether it succeeds,
-- and the inner Maybe whether an element should be added to the output list.
-- 
-- This function accumulates a list of elements for each step that produces one.
tagsPermuteRepetition :: (Monad m, Ord k)
                         => (Name -> k)
                         -> Map.Map k (Repetition, P.AttrParser b, b -> Iteratee Event m (Maybe t))
                         -> Iteratee Event m (Maybe (Maybe (k, t)))
                         -> Iteratee Event m (Maybe [(k, t)])
tagsPermuteRepetition f m' fb = do
      let m = Map.filter (\(r, _, _) -> repetitionAllowsMore r) m'
      (rest, result) <- tags go (\s -> fmap (\a -> (s, a)) <$> fb) m
      return (guard (finished rest) >> Just result)
    where
      finished = Map.null . Map.filter (\(r, _, _) -> repetitionNeedsMore r)
      go s name = do
                    let k = f name
                    (rep, attr, sub) <- Map.lookup k s
                    let adaptSub Nothing  = Nothing
                        adaptSub (Just v) = let s' = case repetitionConsume rep of
                                                       rep' | repetitionAllowsMore rep' -> Map.insert k (rep', attr, sub) s
                                                            | otherwise                 -> Map.delete k s
                                            in Just (s', Just (k, v))
                    Just (attr, fmap adaptSub . sub)
