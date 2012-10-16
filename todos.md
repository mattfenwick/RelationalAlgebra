
 - transitive closure :: (a -> a -> Bool) -> [a] -> [(a, a)] -- not 
   sure about the function sig

 - groupJoin :: (a -> b -> Bool) -> [b] -> [a] -> [(a, [b])]
   that is, join and group at the same time, grouping by the left-hand rows

 - window functions
   - size of window
   - sort order WRT window position

 - look through mysql for more ideas
   - group by ... with rollup

 - organize as a cabal project
