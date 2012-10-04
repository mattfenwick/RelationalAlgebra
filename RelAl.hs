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
import qualified Data.Map
import Data.Set            (toList, fromList, Set, union, intersection, difference)



toMap :: Ord k => [(k, a)] -> Map k a
toMap = Data.Map.fromList

fromMap :: Map k a -> [(k, a)]
fromMap = Data.Map.toList

setApply :: (Ord b) => ([a] -> [b]) -> Set a -> Set b
setApply f = fromList . f . toList



project :: Ord b => (a -> b) -> Set a -> Set b
project f = setApply (map f)


rfilter :: Ord a => (a -> Bool) -> Set a -> Set a
rfilter f = setApply (filter f)
  
  
rproduct :: (Ord a, Ord b) => Set a -> Set b -> Set (a, b)
rproduct ls rs = fromList $ do
  l <- toList ls
  r <- toList rs
  return (l, r)


intersect :: Ord a => Set a -> Set a -> Set a
intersect = intersection


join :: (Ord a, Ord b) => (a -> b -> Bool) -> Set a -> Set b -> Set (a, b)
join f ls rs = rfilter (uncurry f) (rproduct ls rs)


groupBy :: (Ord a, Ord b) => (a -> b) -> Set a -> Set (b, Set a)
groupBy f rel = fromList $ fromMap $ fmap fromList grouped
  where
    grouped = foldl f' (toMap []) $ toList rel

    f' mp next = updateMe (f next) next mp
    
    -- check whether the key's already in the map:
    -- if it is, stick 'next' on the existing list
    -- if not, create a new, single-element list for that key
    updateMe :: Ord k => k -> v -> Map k [v] -> Map k [v]
    updateMe k v mp = case lookup k mp of    
                        (Just oldval) -> insert k (v:oldval) mp;  
                        _ -> insert k [v] mp;     
                        
                        
groupProject :: (Ord b, Ord c) => (a -> c) -> Set (b, a) -> Set (b, c)
groupProject f = project (fmap f)
                        

groupSetProject :: (Ord b, Ord c) => (a -> c) -> Set (b, Set a) -> Set (b, Set c)
groupSetProject f = groupProject (project f)
    

-- extend all the rows in a relation
--   by adding extra field(s)
extend :: (Ord a, Ord b) => (a -> b) -> Set a -> Set (a, b)
extend f rel = project (\a -> (a, f a)) rel


-- alternate, non-monadic implementation:  app2 f g x = (f x, g x)
-- this is an unnecessary but fun function
app2 :: (a -> b) -> (a -> c) -> a -> (b, c)
app2 f g = 
  f >>= \x ->
  g >>= \y ->
  return (x, y)


-- this seems to be a mostly unnecessary function
-- but it's interesting
pivot :: (Ord a, Ord b, Ord c) => (a -> b) -> (a -> c) -> Set a -> Set ((b, c), Set a)
pivot f1 f2 rel = groupBy (app2 f1 f2) rel


-- this is basically a step-by-step implementation of the wikipedia algorithm
-- I put it in reverse order though
-- I think it's basically trying to find counter-examples
-- then subtracting those from the input
divide :: forall a b. (Ord a, Ord b) => Set (a, b) -> Set b -> Set a
divide dividend divisor = quotient
  where
    quotient :: Set a
    quotient = u `difference` x
    x :: Set a
    x = project fst w
    w :: Set (a, b)
    w = v `difference` dividend
    v :: Set (a, b)
    v = u `rproduct` divisor
    u :: Set a
    u = project fst dividend


divideBy :: forall a b c. (Ord b, Ord c) => (a -> b) -> (a -> c) -> Set a -> Set c -> Set b
divideBy f f' dividend divisor = divide dividend' divisor
  where
    dividend' :: Set (b, c)
    dividend' = project (app2 f f') dividend
    
    
leftOuterJoin :: forall a b. (Ord a, Ord b) => (a -> b -> Bool) -> b -> Set a -> Set b -> Set (a, b)
leftOuterJoin p null rl rr = setApply (concatMap f) rl
  where 
    f :: a -> [(a, b)]
    f a = map ((,) a) $ addNull $ filter (p a) $ toList rr
      where
        addNull :: [b] -> [b]
        addNull [] = [null]
        addNull bs = bs
-- go through all the a's 
--   match each a with all b's
--   if no matches, match it with the default
--   otherwise keep all matches


fullOuterJoin :: (Ord a, Ord b) => (a -> b -> Bool) -> a -> b -> Set a -> Set b -> Set (a, b)
fullOuterJoin p anull bnull as bs = union left right
  where 
    left = leftOuterJoin p bnull as bs
    right = project swap $ leftOuterJoin (flip p) anull bs as
    swap (a, b) = (b, a) -- why isn't this in Data.Tuple?  do I have an old library version?
    
    
selfJoin :: Ord a => (a -> a -> Bool) -> Set a -> Set (a, a)
selfJoin p rel = join p rel rel


semiJoin :: (Ord a, Ord b) => (a -> b -> Bool) -> Set a -> Set b -> Set a
semiJoin p r1 r2 = project fst $ join p r1 r2


antiJoin :: (Ord a, Ord b) => (a -> b -> Bool) -> Set a -> Set b -> Set a
antiJoin p r1 r2 = r1 `difference` semiJoin p r1 r2


rank :: Ord a => ([a] -> [a]) -> Set a -> Set (Integer, a)
rank fsort = fromList . zip [1 .. ] . fsort . toList


-- more ideas:
--  - transitive closure :: (a -> a -> Bool) -> Set a -> Set (a, a) -- not sure about the function sig
--  - window functions
--     - size of window
--     - sort order WRT window position
