
 - transitive closure :: (a -> a -> Bool) -> [a] -> [(a, a)] -- not 
   sure about the function sig

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
