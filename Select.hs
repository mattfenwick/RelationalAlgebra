module Select (

    extendAgg

  , select
  , selectMany

) where


import RelAl
import Data.List (elemIndex)



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


