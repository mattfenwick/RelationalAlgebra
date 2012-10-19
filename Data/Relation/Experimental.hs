module Data.Relation.Experimental (

    extend
  , extendAgg
  , rank
  
  , selfJoin
  , semiJoin
  , antiJoin
  , groupJoin

  , select
  , selectMany
  
  , window1
  , windowSeq
  , getTopN

) where


import Data.Relation.Core
import Data.List (genericTake)




-- ------------------------------------------------------------
-- row extensions
-- ------------------------------------------------------------
                        
-- extend all the rows in a relation
--   by adding extra field(s)
extend :: (Eq a, Eq b) => (a -> b) -> [a] -> [(a, b)]
extend f rel = project (\a -> (a, f a)) rel


extendAgg :: (Eq a, Eq b) => ([a] -> b) -> [a] -> [(a, b)]
extendAgg fa rel = extend (const $ fa rel) rel


rank :: [a] -> [(Integer, a)]
rank = zip [1 .. ]



-- ------------------------------------------------------------
-- some more joins    
-- ------------------------------------------------------------
    
selfJoin :: (a -> a -> Bool) -> [a] -> [(a, a)]
selfJoin p rel = innerJoin p rel rel


semiJoin :: Eq a => (a -> b -> Bool) -> [a] -> [b] -> [a]
semiJoin p r1 r2 = project fst $ innerJoin p r1 r2


antiJoin :: (Ord a, Ord b) => (a -> b -> Bool) -> [a] -> [b] -> [a]
antiJoin p r1 r2 = r1 `difference` semiJoin p r1 r2
        
        
groupJoin :: (a -> b -> Bool) -> [a] -> [b] -> [(a, [b])]
groupJoin p ls rs = do
    l <- ls
    let rights = filter (p l) rs
    return (l, rights) -- I guess 'do'-notation was a pretty lame choice here
{- alternatively:
-- has to be no duplicates in left-hand table
groupJoin :: Eq a => (a -> b -> Bool) -> [a] -> [b] -> [(a, [b])]
groupJoin p ls rs = groupBy fst $ innerJoin p ls rs
-- or --
groupJoin p ls rs = ls >>= \l -> let rights = filter (p l) rs
                                 in return (l, rights)
-}



-- ------------------------------------------------------------
-- picking rows by extreme value
-- ------------------------------------------------------------

-- a single row
select :: (a -> a -> a) -> a -> [a] -> a
select = foldl


-- same as 'select', but with Nothing as default value
select' :: (a -> a -> a) -> [a] -> Maybe a
select' _ [] = Nothing
select' f (x:xs) = Just $ foldl f x xs


selectMany :: Eq b => (a -> b) -> (b -> b -> b) -> [a] -> [a]
selectMany _ _ [] = []
selectMany proj chs (r:rs) = rfilter (\r' -> proj r' == bVal) (r:rs)
  where
    bVal = select chs (proj r) (map proj rs) 



-- ------------------------------------------------------------
-- other stuff
-- ------------------------------------------------------------

-- a window function
window1 :: (a -> a -> Ordering) -> (a -> b -> b) -> b -> [a] -> [(a, b)]
window1 cmp fa base rel = zip sorted aggVals
  where
--    aggVals :: [b]
    aggVals = something fa base sorted
    sorted = orderBy cmp rel
    -- this is really similar to scanl and scanr, except that 
    --   the size of the output is the same as that of the input
    something :: (a -> b -> b) -> b -> [a] -> [b]
    something _ _ [] = []
    something g base (l:ls) = reverse $ snd $ foldl f (start, [start]) ls
      where
        start = g l base
        f (b, bs) nxt = (g nxt b, (g nxt b):bs)
        
        
-- bundle each 'row' with a function applied
-- to the list of rows 1) before and 2) after it
windowSeq :: ([a] -> [a] -> b) -> [a] -> [(a, b)]
windowSeq f rs = help [] rs
  where
    help _ [] = []
    help be (a:as) = (a, f be as):(help (a:be) as)
    
    
-- how about:  Ord b => Integer -> (a -> b) -> [a] -> [a]
--        getTopN num f rel = genericTake num . orderBy (on compare f)
getTopN :: (Integral t) => t -> (a -> a -> Ordering) -> [a] -> [a]
getTopN num f = genericTake num . orderBy f
