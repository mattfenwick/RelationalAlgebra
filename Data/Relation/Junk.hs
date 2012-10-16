module Data.Relation.Junk (
  
    pivot
  
) where

import Data.Relation.Core   (groupBy)
import Control.Monad        (liftM2)

-- -----------------------
-- this module has combinators that are expected to be useless
--   as such, it will not be stable




-- shows that pivoting is not a real RA 
-- operation, but rather, a formatting one
pivot :: (Ord a, Ord b, Ord c) => (a -> b) -> (a -> c) -> [a] -> [((b, c), [a])]
pivot f1 f2 rel = groupBy (app2 f1 f2) rel


app2 :: (a -> b) -> (a -> c) -> a -> (b, c)
app2 = liftM2 (,)