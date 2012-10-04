{-# LANGUAGE ScopedTypeVariables #-} -- just for um3


import RelAl
import Data.List (maximumBy, nub)


data Product = Product { 
                   id' :: Integer, 
                   name :: String, 
                   store :: String, 
                   price :: Double
            } deriving (Ord, Eq, Show)


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



g1 = maximum $ project price ps


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


pr :: Show a => Set a -> IO ()
pr = mapM_ print . toList



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

