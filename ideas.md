
 - transitive closure :: (a -> a -> Bool) -> [a] -> [(a, a)] -- not 
   sure about the function sig

 - groupJoin :: (a -> b -> Bool) -> [b] -> [a] -> [(a, [b])]
   that is, join and group at the same time, grouping by the left-hand rows

 - window functions
   - size of window
   - sort order WRT window position

 - I guess anti- and semi-joins that need to look at the whole of 
   another table should really just be implemented as filter operations

 - group by ... with rollup