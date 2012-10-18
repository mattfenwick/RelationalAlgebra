import Data.Relation.Core
import Data.Relation.Experimental
import Data.Function  (on)
import Data.List      (isPrefixOf)



type Date = (Int, Int, Int)
     
data Trans = Trans {
      id        :: Integer,
      date      :: Date,
      amount    :: Double,
      account   :: String,
      isBank    :: Bool
    } deriving (Eq, Show, Ord)
    
trans :: [Trans]
trans = [
    Trans 2   (2008, 10, 26)   12.00  "Checking"     True,
    Trans 6   (2008, 12, 21)    7.59  "Checking"     True,
    Trans 10  (2009, 4, 19)     0.05  "Checking"     True,
    Trans 7   (2009, 5, 18)    11.99  "Checking"     True,
    Trans 1   (2009, 11, 11)   32.33  "Checking"     True,
    Trans 3   (2006, 8, 4)    108.77  "Savings"      True,
    Trans 9   (2008, 11, 13)   22.01  "Savings"      True,
    Trans 8   (2009, 4, 1)      9.99  "Savings"      True,
    Trans 5   (2010, 4, 29)     7.49  "Savings"      True,
    Trans 4   (2012, 3, 21)     0.55  "Credit card"  True,
    Trans 11  (2010, 5, 9)     29.99  "Credit card"  True,
    Trans 12  (2011, 4, 31)    16.34  "Credit card"  True]
    
eoms :: [(String, Date, Double)]
eoms = [("Checking", (2008, 10, 27), 314.15), 
        ("Savings",  (2007, 3,  9),  782.09)]
    
    
-- last step: add initial balance to balance change, of course :)
answer :: [(Trans, Double)]
answer = project (\((t, b), (_, _, b')) -> (t, b + b')) withEOM
  where

    -- where do I get the EOM balance from?  why not a join?
    withEOM :: [((Trans, Double), (String, Date, Double))]
    withEOM = innerJoin (\(t, _) (act, _, _) -> act == account t) ungrouped eoms

    -- ungroup them ... I guess
    ungrouped :: [(Trans, Double)]
    ungrouped = map snd $ ungroup totaled

    totaled :: [(String, [(Trans, Double)])]
    totaled = groupLift windowMe grouped

    -- apply a window function to find the running totals
    windowMe :: [Trans] -> [(Trans, Double)]
    windowMe ts = window1 (on compare date) (\n b -> b + (amount n)) 0 ts

    -- group the transactions by account
    grouped :: [(String, [Trans])]
    grouped = groupBy account filtered

    -- filter the transactions
    --   1. by date -- have to be on or after the date
    --      of first EOM balance, of the same account
    --   2. have to be bank confirmed
    filtered :: [Trans]
    filtered = rfilter isBank $ semiJoin pred trans eoms

    pred t (act, de, _) = (account t == act) && (date t >= de)
    
    
    
    
    
-- ---------------------------------------------------------------
-- now for the billing project example

type I = Integer

type BDate = (I, I, I, I, I, I) -- year, month, day, hour, minute, second

billing :: [([String], Integer)]
billing = groupLift sum grouped
  where
    grouped :: [([String], [Integer])]
    grouped = groupLift (map snd) windowed
    
    windowed :: [([String], [([String], Integer)])]
    windowed = map (\((s, _), r) -> (s, r)) wind1
    
    wind1 :: [(([String], Integer), [([String], Integer)])]
    wind1 = window2 q durations
    
    q :: ([String], b) -> ([String], c) -> Bool
    q (s, _) (s', _) = isPrefixOf s s'

    durations :: [([String], Integer)]
    durations = map (\(s, (start, stop)) -> (s, stop - start)) paired

    paired :: [([String], (Integer, Integer))]
    paired = filter (\(_, (a, b)) -> b >= a) cleaned1

    cleaned1 :: [([String], (Integer, Integer))]
    cleaned1 = map my2Weird $ filter doesnt_match_bad_path $ filter myWeirdF grped1
    
    doesnt_match_bad_path = const True 

    grped1 :: [([String], [(Integer, Bool)])]
    grped1 = groupLift (map (\(_, x, y) -> (x, y))) $ groupBy (\(x,_,_) -> x) events

    events :: [([String], Integer, Bool)] -- Bool means 'isStart'
    events = [
        (["500", "no"], 567, True),
        (["500", "no"], 1024, False),
        (["500","no", "mai", "dis", "donc"], 3223, True),
        (["500","no", "mai", "dis", "donc"], 3237, False)
      ]
    

window2 :: (a -> a -> Bool) -> [a] -> [(a, [a])]
window2 f xs = do
  l <- xs
  let rs = filter (f l) xs
  return (l, rs) -- I guess 'do'-notation was a pretty lame choice here
    
    
myWeirdF :: (a, [(Integer, Bool)]) -> Bool
myWeirdF (_, (_,True):(_,False):zs) = True
myWeirdF (_, (_,False):(_,True):zs) = True
myWeirdF (_, _) = False


my2Weird :: (a, [(Integer, Bool)]) -> (a, (Integer, Integer))
my2Weird (s, ((x, False):(y, True):[])) = (s, (y, x))
my2Weird (s, ((x, True):(y, False):[])) = (s, (x, y))
my2Weird _ = error "what?  this never should have happened"
