{-# LANGUAGE ScopedTypeVariables #-}

module RelAl (
  
    project
  , groupBy
  , groupProject
  , aggregate
  , join
  , product
  , pivot
  , extend
  , union
  , intersect
  , difference
  , divide
  , divide2

) where


import Prelude                hiding (lookup, product)
import Data.List              (nub, union, intersect)
import Data.Map               (lookup, insert, toList, fromList, Map)



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
    

-- extend all the rows in a relation
--   by adding extra field(s)
extend :: (a -> b) -> [a] -> [(a, b)]
extend f rel = zip rel (project f rel)


-- this seems to be a mostly unnecessary function
-- but it's interesting
pivot :: (Ord b, Ord c) => (a -> b) -> (a -> c) -> [a] -> [((b, c), [a])]
pivot f1 f2 rel = groupBy (app2 f1 f2) rel


-- alternate, non-monadic implementation:  app2 f g x = (f x, g x)
-- this is an unnecessary but fun function
app2 :: (a -> b) -> (a -> c) -> a -> (b, c)
app2 f g = 
  f >>= \x ->
  g >>= \y ->
  return (x, y)
  
  
-- divide :: [a] -> [b] -> [c] -- or [a] since we can't fuck with type variables


-- those elements in the 1st, *and not* in the 2nd
--   note how I could use Data.List.(\\) if I knew the
--   input lists didn't have duplicates
difference :: Eq a => [a] -> [a] -> [a]
difference r1 r2 = filter (\x -> not $ elem x r2) r1


-- this is basically a step-by-step implementation of the wikipedia algorithm
-- I put it in reverse order though
-- I think it's basically trying to find counter-examples
-- then subtracting those from the input
divide :: forall a b. (Eq a, Eq b) => [(a, b)] -> [b] -> [a]
divide dividend divisor = quotient
  where
    quotient :: [a]
    quotient = u `difference` x
    x :: [a]
    x = nub $ map fst w
    w :: [(a, b)]
    w = v `difference` dividend
    v :: [(a, b)]
    v = u `product` divisor
    u :: [a]
    u = nub $ map fst dividend
    
    
divide2 :: forall a b c. (Eq a, Eq b, Ord c) => (a -> b -> Bool) -> (a -> c) -> [a] -> [b] -> [c]
divide2 p f dividend divisor = umm
  where 
    inner :: [(a, b)]
    inner = join p (nub dividend) (nub divisor)
    sthg :: [(c, [(c, b)])] -- type??
    sthg = groupBy fst $ map (\(x, y) -> (f x, y)) inner
    umm :: [c]
    umm = map fst $ filter (\(_, xs) -> length xs == length divisor) sthg


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

we don't really need a special 'pivot' function, 
   because honestly it's just a special case of 'groupBy'
   the way people typically use it is as a formatting thing
   I have no need of a formatting operator
-}