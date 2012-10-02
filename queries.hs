import RelAl
import Data.List (maximumBy)


data Product = Product { 
                   id' :: Integer, 
                   name :: String, 
                   store :: String, 
                   price :: Double
            } deriving (Ord, Eq, Show)


ps = [Product 1 "abc" "xyz" 32.23,
          Product 2 "def" "www" 2.23,
          Product 3 "abc" "www" 3.23,
          Product 4 "def" "xyz" 32.3,
          Product 5 "ghi" "xyz" 32.2,
          Product 6 "abc" "www" 2.3,
          Product 7 "abc" "www" 2.2,
          Product 8 "ghi" "xyz" 3.2,
          Product 9 "ghi" "www" 3.3,
          Product 10 "def" "xyz" 3,
          Product 11 "abc" "xyz" 2]



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
