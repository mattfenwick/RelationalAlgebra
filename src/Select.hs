module Select (

    extendAgg

  , select
  , selectMany
  
  , window1
  , getTopN
  
  , pivot

) where


import RelAl
import Data.List (elemIndex, sortBy, genericTake)
import Control.Monad (liftM2)



extendAgg :: (Eq a, Eq b) => ([a] -> b) -> [a] -> [(a, b)]
extendAgg fa rel = extend (const $ fa rel) rel


-- this represents selecting a single row by extreme value
--   or something
select :: (a -> a -> a) -> a -> [a] -> a
select = foldl


select' :: (a -> a -> a) -> [a] -> Maybe a
select' _ [] = Nothing
select' f (x:xs) = Just $ foldl f x xs


-- note how 'selectMany id' specialize this to
--    :: Eq a => (a -> a -> b) -> [a] -> [a]
selectMany :: Eq b => (a -> b) -> (b -> b -> b) -> [a] -> [a]
selectMany _ _ [] = []
selectMany proj chs (r:rs) = rfilter (\r' -> proj r' == bVal) (r:rs)
  where
    bVal = select chs (proj r) (map proj rs) 


-- a window function
window1 :: (a -> a -> Ordering) -> (a -> b -> b) -> b -> [a] -> [(a, b)]
window1 cmp fa base rel = zip sorted aggVals
  where
--    aggVals :: [b]
    aggVals = something fa base sorted
    sorted = sortBy cmp rel
    
    
-- this is really similar to scanl and scanr, except that 
--   the size of the output is the same as that of the input
something :: (a -> b -> b) -> b -> [a] -> [b]
something _ _ [] = []
something g base (l:ls) = reverse $ snd $ foldl f (start, [start]) ls
  where
    start = g l base
    f (b, bs) nxt = (g nxt b, (g nxt b):bs)
    
    
-- a limitation of this approach:  if there's 'ties', one of
--   them will get picked ... but which one?  I guess this could
--   be resolved by making sure that the ordering is on a key,
--   but that's not gonna happen
-- how about:  Ord b => Integer -> (a -> b) -> [a] -> [a]
--        getTopN num f rel = genericTake num . sortBy (on compare f)
-- how does this deal with client code that needs to, say, 
--   reverse the sort order?
getTopN :: (Integral t) => t -> (a -> a -> Ordering) -> [a] -> [a]
getTopN num f = genericTake num . sortBy f


-- this seems to be a mostly unnecessary function
-- but it's interesting, and shows that pivoting
-- is not a real RA operation, but rather, a 
-- formatting one
pivot :: (Ord a, Ord b, Ord c) => (a -> b) -> (a -> c) -> [a] -> [((b, c), [a])]
pivot f1 f2 rel = groupBy (app2 f1 f2) rel


-- alternate, non-monadic implementation:  app2 f g x = (f x, g x)
-- this is an unnecessary but fun function
app2 :: (a -> b) -> (a -> c) -> a -> (b, c)
app2 = liftM2 (,)
-- holy crap, it's exactly the same as 'rproduct'!!!!

