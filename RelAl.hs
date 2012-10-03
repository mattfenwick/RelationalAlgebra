{-# LANGUAGE ScopedTypeVariables #-}

module RelAl (
  
    project
  , groupBy
  , groupProject
  , aggregate
  , join
  , leftOuterJoin
  , fullOuterJoin
  , selfJoin
  , semiJoin
  , antiJoin
  , product
  , pivot
  , extend
  , union
  , intersect
  , difference
  , divide

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
    
    
leftOuterJoin :: forall a b. (a -> b -> Bool) -> b -> [a] -> [b] -> [(a, b)]
leftOuterJoin p null rl rr = concatMap f rl
  where 
    f :: a -> [(a, b)]
    f a = map ((,) a) $ addNull $ filter (p a) rr
      where
        addNull :: [b] -> [b]
        addNull [] = [null]
        addNull bs = bs
-- go through all the a's 
--   match each a with all b's
--   if no matches, match it with the default
--   otherwise keep all matches


fullOuterJoin :: (Eq a, Eq b) => (a -> b -> Bool) -> a -> b -> [a] -> [b] -> [(a, b)]
fullOuterJoin p anull bnull as bs = union left right
  where 
    left = leftOuterJoin p bnull as bs
    right = map swap $ leftOuterJoin (flip p) anull bs as
    swap (a, b) = (b, a) -- why isn't this in Data.Tuple?  do I have an old library version?
    
    
selfJoin :: (a -> a -> Bool) -> [a] -> [(a, a)]
selfJoin p rel = join p rel rel


semiJoin :: Eq a => (a -> b -> Bool) -> [a] -> [b] -> [a]
semiJoin p r1 r2 = nub $ map fst $ join p r1 r2


antiJoin :: Eq a => (a -> b -> Bool) -> [a] -> [b] -> [a]
antiJoin p r1 r2 = r1 `difference` semiJoin p r1 r2


-- more ideas:
--  - transitive closure :: (a -> a -> Bool) -> [a] -> [(a, a)] -- not sure about the function sig
--  - rank :: [a] -> [(a, Integer)] -- should it sort first?
--  - window functions
--     - size of window
--     - sort order WRT window position


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