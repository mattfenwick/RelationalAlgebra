
 - transitive closure :: (a -> a -> Bool) -> [a] -> [(a, a)] -- not 
   sure about the function sig

 - groupJoin :: (a -> b -> Bool) -> [b] -> [a] -> [(a, [b])]
   that is, join and group at the same time, grouping by the left-hand rows

 - window functions
   - size of window
   - sort order WRT window position

 - take a look at anti- and semi-joins -- right types?
   - figure out how to do 'where not exists' and friends

 - look through mysql for more ideas
   - group by ... with rollup

 - organize as a cabal project

 - maybe move anti, semi, self joins and extend out of the core b/c they're not that great
   - which means I need to create another module for them to live in ... 'Select' isn't a great name