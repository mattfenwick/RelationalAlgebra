{-# LANGUAGE ScopedTypeVariables #-}

module Data.Relation.Core (
  
    project
  , rfilter
  , rproduct
  , union
  , difference
  , intersect

  , divide
  , divideBy

  , innerJoin
  , leftJoin
  , outerJoin

  , groupBy
  , groupLift
  , ungroup
  , aggregate
  
  , orderBy

) where


import Prelude             hiding (lookup)
import Data.Map            (lookup, insert, Map, toList, fromList)
import Control.Monad       (liftM2)
import Data.List           (nub, sortBy)



-- ------------------------------------------------------------
-- basic operations on relations
-- ------------------------------------------------------------

project :: Eq b => (a -> b) -> [a] -> [b]
project f = nub . map f


rfilter :: (a -> Bool) -> [a] -> [a]
rfilter = filter
  
  
rproduct :: [a] -> [b] -> [(a, b)]
rproduct = liftM2 (,)


intersect :: Eq a => [a] -> [a] -> [a]
intersect xs ys = filter (\x -> x `elem` ys) xs


union :: Eq a => [a] -> [a] -> [a]
union xs ys = nub (xs ++ ys)


difference :: Eq a => [a] -> [a] -> [a]
difference r1 r2 = filter (\x -> not $ elem x r2) r1



-- ------------------------------------------------------------
-- non-primitive relational operations
-- ------------------------------------------------------------

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
    x = project fst w
    w :: [(a, b)]
    w = v `difference` dividend
    v :: [(a, b)]
    v = u `rproduct` divisor
    u :: [a]
    u = project fst dividend


divideBy :: forall a b c. (Eq a, Eq b, Eq c) => (a -> b) -> (a -> c) -> [a] -> [c] -> [b]
divideBy f f' dividend divisor = divide dividend' divisor
  where
    dividend' :: [(b, c)]
    dividend' = project (app2 f f') dividend
    app2 :: (a -> b) -> (a -> c) -> a -> (b, c)
    app2 = liftM2 (,)



-- ------------------------------------------------------------
-- basic joins
-- ------------------------------------------------------------

innerJoin :: (a -> b -> Bool) -> [a] -> [b] -> [(a, b)]
innerJoin f ls rs = rfilter (uncurry f) (rproduct ls rs)
    
    
-- a left outer join
-- for each a: 
--   match with each b
--   if no matches, match a with the default
--   otherwise keep all matches
leftJoin :: forall a b. (a -> b -> Bool) -> b -> [a] -> [b] -> [(a, b)]
leftJoin p null rl rr = concatMap f rl
  where 
    f :: a -> [(a, b)]
    f a = map ((,) a) $ addNull $ filter (p a) rr
      where
        addNull :: [b] -> [b]
        addNull [] = [null]
        addNull bs = bs


outerJoin :: (Eq a, Eq b) => (a -> b -> Bool) -> a -> b -> [a] -> [b] -> [(a, b)]
outerJoin p anull bnull as bs = union left right
  where 
    left = leftJoin p bnull as bs
    right = project swap $ leftJoin (flip p) anull bs as
    swap (a, b) = (b, a) -- why isn't this in Data.Tuple?  do I have an old library version?
    


-- ------------------------------------------------------------
-- grouping and aggregation
-- ------------------------------------------------------------

groupBy :: (Ord b) => (a -> b) -> [a] -> [(b, [a])]
groupBy f rel = toList grouped
  where
    grouped = foldl f' (fromList []) rel

    f' mp next = addRow (f next) next mp
    
    -- check whether the key's already in the map:
    -- if it is, stick 'next' on the existing list
    -- if not, create a new, single-element list for that key
    addRow :: Ord k => k -> v -> Map k [v] -> Map k [v]
    addRow k v mp = case lookup k mp of    
                        (Just oldval) -> insert k (v:oldval) mp;  
                        _ -> insert k [v] mp;    


groupLift :: ([a] -> c) -> [(b, [a])] -> [(b, c)]
groupLift f = map (fmap f) 


-- hmm, this isn't the opposite of grouping,
--   because it retains the group value ... is that inconsistent?
-- also  rel >>= \(x,y) -> y >>= \z -> return (x, z)
ungroup :: [(b, [a])] -> [(b, a)]
ungroup rel = do
  (x, y) <- rel
  z <- y
  return (x, z)


-- this isn't just application -- example:
--    f_aggregate $ project f_proj relation
-- if the projection creates duplicates, they'll be lost
-- that's why we need to use 'map' instead of project
aggregate :: (a -> b) -> ([b] -> c) -> [a] -> c
aggregate proj f = f . map proj



-- ------------------------------------------------------------
-- sorting
-- ------------------------------------------------------------

orderBy :: (a -> a -> Ordering) -> [a] -> [a]
orderBy = sortBy
