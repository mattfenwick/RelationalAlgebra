module Select (

) where


import RelAl
import Data.List (elemIndex)



extendAgg :: (Eq a, Eq b) => ([a] -> b) -> [a] -> [(a, b)]
extendAgg fa rel = extend (const $ fa rel) rel


-- this represents selecting all the rows that 
--   have some aggregate value
select :: Eq b => ([b] -> b) -> (a -> b) -> [a] -> [a]
select chs proj rel = rfilter (\r -> proj r == aggB) rel
  where
    aggB = chs $ project proj rel


select' :: (Eq a, Eq b) => ([b] -> b) -> (a -> b) -> [a] -> [a]
select' chs proj rel = project fst $ filter (\(a, b) -> proj a == b) $ extendAgg (chs . map proj) rel 


-- this represents selecting a single row by extreme value
--   or something
select1 :: (a -> a -> a) -> a -> [a] -> a
select1 = foldl


select1' :: (a -> a -> a) -> [a] -> Maybe a
select1' _ [] = Nothing
select1' f (x:xs) = Just $ foldl f x xs


