
 - transitive closure :: (a -> a -> Bool) -> [a] -> [(a, a)] -- not 
   sure about the function sig

 - groupJoin :: (b -> a -> Bool) -> [b] -> [a] -> [(b, [a])]
   that is, join and group at the same time, grouping by the left-hand rows
   can be implemented with (groupBy fst . join) if we allow Eq constraint
   on left-hand table and forbid duplicates (also on left-hand table)

 - window functions
   - size of window
   - sort order WRT window position
   - types of windowing:
     - by sort order, plus a window size
     - by (a -> a -> Bool) -- whether each element is in each 
       other element's window (this could be easily implemented as
       a groupJoin -- see above)

 - look through mysql for more ideas
   - group by ... with rollup

 - organize as a cabal project
