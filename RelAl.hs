module RelAl (
  
    project
  , groupBy
  , groupByTake1
  , groupByTake1AndTransform
  , join
  , product

) where


import Prelude        hiding (lookup, product)
import Data.List      (nub) -- um ... not using 'nub'
import Data.Map       (lookup, insert, toList, fromList, Map)



project :: (a -> b) -> [a] -> [b]
project = map


-- filter :: (a -> Bool) -> [a] -> [a]
-- filter is just filter


join :: [a] -> [b] -> (a -> b -> Bool) -> [(a, b)]
join ls rs f = filter (uncurry f) (product ls rs)
  
  
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
    updateMe :: Ord k => k -> v -> Map k [v] -> Map k [v]
    updateMe k v mp = case lookup k mp of    -- check whether the key's already in the map:
                        (Just oldval) -> insert k (v:oldval) mp;  -- if it is, stick 'next' on the existing list
                        _ -> insert k [v] mp;     -- if not, create a new, single-element list for that key
                        
                        
-- lots of related functions that would be useful:
--  :: [a] -> (a -> b) -> ([a] -> a) -> [a] -- groups the rows, then pulls out one row from each group
--  :: [a] -> (a -> b) -> ([a]

-- groups a relation and takes one row from each group
--   should the grouping value also be returned?
-- takes: 
--   1. a projection function
--   2. a selection function -- to get one row out of each group
--   3. the relation
-- this may have a bug:  if there are multiple rows in the same group
--   with the same value for the selection function, I think a random
--   one gets picked
groupByTake1 :: (Ord b) => (a -> b) -> ([a] -> a) -> [a] -> [(b, a)]
groupByTake1 f selector rel = project (\(m, rs) -> (m, selector rs)) grouped
  where grouped = groupBy f rel
  
  
-- take:
--  1. a projection function for grouping
--  2. a selection function
--  3. a projection function for transforming the selected rows
--  4. the relation
groupByTake1AndTransform :: Ord b => (a -> b) -> ([a] -> a) -> (a -> c) -> [a] -> [(b, c)]
groupByTake1AndTransform fg sel trans rel = project (\(m, r) -> (m, trans r)) longThing
  where longThing = groupByTake1 fg sel rel
  
-- aggregate :: [a] -> (a -> b) 
-- or:  this applies an aggregate function to a grouped relation
-- :: [(a, [b])] -> ([b] -> b) -> [(a, b)]
-- or:  this transforms each row and selects the -est row from each group
-- :: [(a, [b])] -> (b -> c) -> ([c] -> c) -> [(a, c)]
-- or:  ???


{-
so what independent operations do we have so far?

- transforming
- grouping
- aggregation

so we combine these pair-wise, and get:

1. transforming a grouped relation
  - what is transformed? the grouping value?  the rows?  everything all together?
2. aggregating a grouped relation
3. transforming and aggregating a grouped relation (can this be done by 1 and 2?)
4. aggregating and transforming a grouped relation (same ? as 3)

  
-}
