{-# LANGUAGE ScopedTypeVariables #-}

module RelAl (
  
    project
  , groupBy
  , groupProject
  , aggregate
  , join
  , product

) where


import Prelude        hiding (lookup, product)
import Data.List      (nub) -- um ... not currently using 'nub'
import Data.Map       (lookup, insert, toList, fromList, Map)



project :: (a -> b) -> [a] -> [b]
project = map


-- filter :: (a -> Bool) -> [a] -> [a]
-- filter is just filter


join :: (a -> b -> Bool) -> [a] -> [b] -> [(a, b)]
join f ls rs = filter (uncurry f) (product ls rs)
  
  
product :: [a] -> [b] -> [(a, b)]
product ls rs = do
  l <- ls
  r <- rs
  return (l, r)


groupBy :: (Ord b) => (a -> b) -> [a] -> [(b, [a])]
groupBy f rel = toList grouped
  where
    grouped = foldl f' (fromList []) rel

    f' mp next = updateMe (f next) next mp
    
    -- check whether the key's already in the map:
    -- if it is, stick 'next' on the existing list
    -- if not, create a new, single-element list for that key
    updateMe :: Ord k => k -> v -> Map k [v] -> Map k [v]
    updateMe k v mp = case lookup k mp of    
                        (Just oldval) -> insert k (v:oldval) mp;  
                        _ -> insert k [v] mp;     
                        

-- still missing:   (a -> c) -> [(b, a)] -> [(b, c)]  -- that's projection across a pair
--   but this can be handled by project:  \f rel -> project (fmap f) rel
groupProject :: forall a b c. (a -> c) -> [(b, [a])] -> [(b, [c])]
groupProject f rel = fmap g rel
  where
    g :: (b, [a]) -> (b, [c])
    g = fmap (fmap f)
    
    
aggregate :: forall a b c. ([a] -> c) -> [(b, [a])] -> [(b, c)]
aggregate f rel = fmap g rel
  where
    g :: (b, [a]) -> (b, c)
    g = fmap f
                        


{-
so what independent operations do we have so far?

- transforming
- grouping
- aggregation

so we combine these pair-wise, and get:

1. transforming a grouped relation
  - what is transformed -- just the rows; not the grouping value
  - if you need to transform everything together, that's just a normal 'project'
2. aggregating a grouped relation
3. transforming and aggregating a grouped relation (can this be done by 1 and 2?)
4. aggregating and transforming a grouped relation (same ? as 3)

  
-}
