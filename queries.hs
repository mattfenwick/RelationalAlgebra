{-# LANGUAGE ScopedTypeVariables #-} -- just for um3


import RelAl
import Select
import Data.List (maximumBy, nub, genericLength)
import Data.Function (on)


-- check out this example (just to show how cool 'groupLift' can be):
--   groupLift (getTopN 2 (on (flip compare) snd)) $ groupBy fst [(x,y) | x <- [1..4], y <- "acxv"]



data Product = Product { 
                   id' :: Integer, 
                   name :: String, 
                   store :: String, 
                   price :: Double
            } deriving (Ord, Eq, Show)
            

pr :: Show a => [a] -> IO ()
pr = mapM_ print


ps = [Product  1   "abc"  "xyz"  32.23,
      Product  2   "def"  "www"  2.23,
      Product  3   "abc"  "www"  3.23,
      Product  4   "def"  "xyz"  32.3,
      Product  5   "ghi"  "xyz"  32.2,
      Product  6   "abc"  "www"  2.3,
      Product  7   "abc"  "www"  2.2,
      Product  8   "ghi"  "xyz"  3.2,
      Product  9   "ghi"  "www"  3.3,
      Product  10  "def"  "xyz"  3,
      Product  11  "abc"  "xyz"  2,
      Product  12  "jkl"  "www"  19.7,
      Product  13  "mno"  "xyz"  9.85,
      Product  14  "abc"  "zzz"  10.22]


-- select * from t;
-- select x, y from t;
-- select x, y, x + y from t;
-- select t.*, x + y from t;

s1 = ps
s2 = project (\r -> (id' r, price r)) ps
s3 = project (\r -> (id' r, price r, name r ++ store r)) ps
s4 = extend (\r -> price r - (fromIntegral $ id' r)) ps


-- select t.*, (select max(x) from t) from t;
-- select max(x) from t;

sel1 = extendAgg (sum . map price) ps
sel2 = select max 0 $ project price ps
sel2' = maximum $ project price ps


-- select count(*) from t;
-- select sum(x) from t;
-- select avg(x + y) from t;
agg1 = length ps
agg1' = aggregate id length ps
agg2 = aggregate price sum ps
agg3 = aggregate (\r -> price r + (fromIntegral $ id' r)) avg ps
  where avg xs = sum xs / genericLength xs


-- select * from t where x < 3;
-- select * from t where y - x = 4;
fil1 = rfilter (\r -> price r < 3) ps
fil2 = rfilter (\r -> id' r == 7) ps


-- select a, max(x) from t group by a;
-- select a, b, count(*), sum(x) from t group by a, b;
gr1 = groupLift (select max 0) $ groupLift (project price) $ groupBy name ps
gr2 = groupLift (aggregate price (\p -> (length p, sum p))) $ groupBy (\r -> (name r, store r)) ps


-- select x, y from t order by a desc limit 5;
-- select x + y from t order by b asc limit 5, 10; -- this starts with the 5th and ends with the 10th, right?  not sure
topN1 = project (\r -> (id' r, price r)) $ getTopN 5 (on (flip compare) price) ps
topN_5_10 = drop 5 $ project (\r -> (id' r, price r)) $ getTopN 10 (on compare price) ps


-- group top
grpTop1 = groupLift (getTopN 2 (on (flip compare) price)) $ groupBy name ps

{-


g2 = aggregate maximum $ groupProject price $ groupBy store ps 


group_aug = join pred ps g2
  where
    pred l r = store l == fst r
-- using :: Eq b => (a -> b) -> (c -> b) -> a -> c -> Bool
-- using f g a b = f a == g b
-- so:
--   pred = using store fst


maximumOn :: Ord b => (a -> b) -> [a] -> a
maximumOn f xs = maximumBy myF xs
  where
    myF l r = compare (f l) (f r)


group_est = aggregate (maximumOn price) $ groupBy store ps


pivot_2 = aggregate fa $ groupProject price $ groupBy fg ps
  where
    fa y = (sum y, length y)
    fg x = (name x, store x)
    
    
quotient = myDividend `divide` myDivisor
  where 
    myDividend = project (\x -> (name x, store x)) ps
    myDivisor = fromList ["www", "xyz"]




tabs = [(1, 3, 5, "foo"),
        (1, 4, 7, "bar"),
        (1, 2, 9, "baz"),
        (2, 1, 1, "dog"),
        (2, 5, 2, "cat"),
        (2, 5, 3, "horse"),
        (2, 3, 8, "pig")]
ts = fromList tabs
        
one   (a, _, _, _) = a
two   (_, b, _, _) = b
three (_, _, c, _) = c
four  (_, _, _, d) = d


um3 :: forall a b. (Ord a, Ord b) => ([b] -> b) -> (a -> b) -> Set a -> Set a
um3 ch pr rel = rfilter f rel
  where
    f :: a -> Bool
    f x = pr x == top
    top :: b
    top = ch $ toList $ project pr rel
    
    
-- SO question:   http://stackoverflow.com/questions/12726549/select-one-value-from-a-group-based-on-order-from-other-columns
soQ = project (fmap (four . head . toList)) $ project (fmap (um3 maximum three)) $ project (fmap (um3 maximum two)) $ groupBy one ts

-}