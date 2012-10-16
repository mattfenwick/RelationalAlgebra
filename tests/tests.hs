import Test.HUnit      -- why doesn't this work? (assertEqual, Test, TestList) -- , TestCase)
import Data.Relation.Core          
import Data.Relation.Experimental
import Control.Monad   (liftM2)


newTest :: (Eq a, Show a) => String -> a -> a -> Test
newTest a b c = TestCase $ assertEqual a b c


empty = []

-- project:
--   - empty set -> empty set
--   - output has no dupes -> set has same size
--   - output has dupes -> smaller output
--   
testProject = TestList [tp1, tp2, tp3]
  where 
    tp1 = newTest "empty set to empty set" empty (project (+1) empty)
    tp2 = newTest "same size" (newSet [1 .. 5]) (project (flip (-) 3) $ newSet [4 .. 8])
    tp3 = newTest "different size" (newSet [1 .. 3]) (project (flip div 2) $ newSet [2 .. 7])
    

-- empty -> empty
-- same -> same if filter catches nothing
-- smaller if filter catches some
testRFilter = TestList [tf1, tf2, tf3]
  where
    tf1 = newTest "empty to empty" empty (rfilter (> 2) empty)
    tf2 = newTest "same to same" (newSet [1 .. 5]) (rfilter (< 8) $ newSet [1..5])
    tf3 = newTest "smaller" (newSet [1..3]) (rfilter (< 4) $ newSet [1 .. 10])
    
    
-- empty, empty -> empty
-- 1 X m -> m rows
-- m x n -> m * n rows
testRProduct = TestList [tp1, tp2, tp3]
  where
    tp1 = newTest "empty, empty to empty" e3 (rproduct e1 e2)
      where   -- not sure why this works, but otherwise it complains about ambiguous type variables
        e1 :: Set Int
        e1 = empty
        e2 :: Set Char
        e2 = empty
        e3 :: Set (Int, Char)
        e3 = empty
    tp2 = newTest "1 x n -> n rows" (newSet [('a', 'x'), ('a', 'y'), ('a', 'z')]) (rproduct (newSet "a") (newSet "xyz"))
    tp3 = newTest "m x n -> m * n rows" (newSet $ liftM2 (,) [1 .. 5] "abc") (rproduct (newSet [1..5]) (newSet "abc"))
    
    
-- I don't really feel like testing intersection b/c it's provided by Data.Set


-- empty, empty -> empty
-- m x n -> empty if p misses all pairs
-- m x n -> (< m * n) if p hits some
-- m x n -> m * n if p hits all
testJoin = TestList [tp1, tp2, tp3, tp4]
  where
    tp1 = newTest "empty, empty -> empty" e3 (join (const2 True) e1 e2)
      where
        e1 :: Set Int
        e1 = empty
        e2 :: Set Char
        e2 = empty
        e3 :: Set (Int, Char)
        e3 = empty
    tp2 = newTest "m x n -> empty" empty (join (const2 False) s1 s2)
    tp3 = newTest "m x n -> (< m * n)" s3 (join (\x y -> x + y > 11) s1 s2)
    tp4 = newTest "m x n -> m * n" s4 (join (const2 True) s1 s2)
    const2 :: a -> b -> c -> a
    const2 a _ _ = a
    s1 = newSet [1 .. 5]
    s2 = newSet [3 .. 9]
    s3 = newSet [(3, 9), (4, 8), (4, 9), (5, 7), (5, 8), (5, 9)]
    s4 = newSet $ liftM2 (,) [1..5] [3..9]


-- union of all groups = input
-- number of groups
testGroupBy = TestList [tp1, tp2]
  where
    tp1 = newTest "union of results = input" input unioned
    input = newSet [1 .. 15]
    grouped = groupBy (flip div 4) input
    unioned = foldl Data.Set.union empty $ map snd $ toList grouped
    tp2 = newTest "number of groups" 4 (length $ toList grouped)
    
    
testGroupProject = undefined -- b/c groupBy's type may need to be changed
-- to something like (a -> b) -> Set a -> Map b (Set a) ... but then it's no longer a relation



testMe = runTestTT $ TestList tests
  where tests = [testProject, testRFilter, testRProduct, testJoin,
                 testGroupBy]

