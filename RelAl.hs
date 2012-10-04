{-# LANGUAGE ScopedTypeVariables #-}

module RelAl (
  
    project
  , rfilter
  , rproduct
  , union
  , difference
  , intersect

  , groupBy
  , groupProject
  , groupSetProject

  , join
  , leftOuterJoin
  , fullOuterJoin
  , selfJoin
  , semiJoin
  , antiJoin

  , pivot
  , extend
  , divide
  , divideBy

  , rank

) where


import Prelude             hiding (lookup)
import Data.Map            (lookup, insert, Map)
import Control.Monad       (liftM2)
import Data.List           (nub)



project :: Eq b => (a -> b) -> [a] -> [b]
project f = nub . map f


rfilter :: (a -> Bool) -> [a] -> [a]
rfilter = filter
  
  
rproduct :: [a] -> [b] -> [(a, b)]
rproduct = liftM2 (,)


intersect :: Eq a => [a] -> [a] -> [a]
intersect x y = nub (x ++ y)


join :: (a -> b -> Bool) -> [a] -> [b] -> [(a, b)]
join f ls rs = rfilter (uncurry f) (rproduct ls rs)


-- what's the correct type sig?
-- groupBy :: (Ord a, Ord b) => (a -> b) -> [a] -> [(b, [a])
groupBy f rel = toList grouped
  where
    grouped = foldl f' (toMap []) rel

    f' mp next = updateMe (f next) next mp
    
    -- check whether the key's already in the map:
    -- if it is, stick 'next' on the existing list
    -- if not, create a new, single-element list for that key
    updateMe :: Ord k => k -> v -> Map k [v] -> Map k [v]
    updateMe k v mp = case lookup k mp of    
                        (Just oldval) -> insert k (v:oldval) mp;  
                        _ -> insert k [v] mp;     
                        
                        
groupProject :: (Eq b, Eq c) => (a -> c) -> [(b, a)] -> [(b, c)]
groupProject f = project (fmap f)
                        

groupSetProject :: (Eq b, Eq c) => (a -> c) -> [(b, [a])] -> [(b, [c])]
groupSetProject f = groupProject (project f)
    

-- extend all the rows in a relation
--   by adding extra field(s)
extend :: (Eq b, Eq c) => (a -> b) -> [a] -> [(a, b)]
extend f rel = project (\a -> (a, f a)) rel


-- alternate, non-monadic implementation:  app2 f g x = (f x, g x)
-- this is an unnecessary but fun function
app2 :: (a -> b) -> (a -> c) -> a -> (b, c)
app2 = liftM2 (,)
-- holy crap, it's exactly the same as 'rproduct'!!!!


-- this seems to be a mostly unnecessary function
-- but it's interesting
pivot :: (Ord a, Ord b, Ord c) => (a -> b) -> (a -> c) -> [a] -> [((b, c), [a])]
pivot f1 f2 rel = groupBy (app2 f1 f2) rel


-- this is basically a step-by-step implementation of the wikipedia algorithm
-- I put it in reverse order though
-- I think it's basically trying to find counter-examples
-- then subtracting those from the input
divide :: forall a b. [(a, b)] -> [b] -> [a]
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


divideBy :: forall a b c. (a -> b) -> (a -> c) -> [a] -> [c] -> [b]
divideBy f f' dividend divisor = divide dividend' divisor
  where
    dividend' :: [(b, c)]
    dividend' = project (app2 f f') dividend
    
    
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


fullOuterJoin :: (a -> b -> Bool) -> a -> b -> [a] -> [b] -> [(a, b)]
fullOuterJoin p anull bnull as bs = union left right
  where 
    left = leftOuterJoin p bnull as bs
    right = project swap $ leftOuterJoin (flip p) anull bs as
    swap (a, b) = (b, a) -- why isn't this in Data.Tuple?  do I have an old library version?
    
    
selfJoin :: (a -> a -> Bool) -> [a] -> [(a, a)]
selfJoin p rel = join p rel rel


semiJoin :: (a -> b -> Bool) -> [a] -> [b] -> [a]
semiJoin p r1 r2 = project fst $ join p r1 r2


antiJoin :: (Ord a, Ord b) => (a -> b -> Bool) -> [a] -> [b] -> [a]
antiJoin p r1 r2 = r1 `difference` semiJoin p r1 r2


rank :: ([a] -> [a]) -> [a] -> [(Integer, a)]
rank fsort = zip [1 .. ] . fsort


-- more ideas:
--  - transitive closure :: (a -> a -> Bool) -> [a] -> [(a, a)] -- not sure about the function sig
--  - window functions
--     - size of window
--     - sort order WRT window position
