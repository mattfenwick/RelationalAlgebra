{-# LANGUAGE ScopedTypeVariables #-} -- just for um3


import Data.Relation.Core
import Data.Relation.Experimental
import Data.List      (maximumBy, nub, genericLength)
import Data.Function  (on)
import Dump           (Person(..), Pet(..), persons, pets)
import Control.Monad  (liftM2, liftM3)


-- match the MySQL queries

sel1 = persons
sel2 = project f persons
  where f = liftM2 (,) person_age person_weight
sel3 = project h persons
  where g = liftM3 (,,) person_age person_weight (\x -> person_age x + person_weight x)
        h = liftM3 (,,) person_age person_weight (uncurry (+) . liftM2 (,) person_age person_weight)
sel4 = extend (uncurry (+) . liftM2 (,) person_age person_weight) persons


agg1 = extendAgg (select max 0 . project person_age) persons
agg2 = select max 0 . project person_weight $ persons
agg3 = length persons
agg4 = aggregate person_age sum persons
agg5 = aggregate (\p -> fromIntegral (person_age p + person_weight p)) avg persons
  where avg = uncurry (/) . liftM2 (,) sum genericLength
  
  
fil1 = rfilter (\p -> person_age p < 30) persons
fil1' = rfilter ((< 30) . person_age) persons
fil2 = rfilter ((flip elem [110, 160]) . person_weight) persons


grp1 = groupLift (aggregate pet_age (select max 0)) $ groupBy pet_species pets
grp1' = groupLift (select max 0) $ groupLift (project pet_age) $ groupBy pet_species pets
grp2 = groupLift fg $ groupBy (liftM2 (,) pet_owner_id pet_species) pets
  where fg = aggregate pet_age (liftM2 (,) length sum)
grp3 = aggregate person_age (select max 0) persons
grp4 = groupLift (extendAgg (select max 0 . map pet_age)) $ groupBy pet_species pets
grp5 = innerJoin (\p g -> (pet_species p == fst g) && (pet_age p == snd g)) pets grp1


-- this has reverse sorting.  note that there are a lot of ways to get that:
--   'flip' the 'on', flip the 'compare', flip the projection, 
--   reverse the result ... what's the best?
ord1 = take 5 $ project (liftM2 (,) person_name person_age) $ orderBy (on compare (negate . person_weight)) persons
ord2 = project (uncurry (+) . liftM2 (,) person_age person_weight) $ take 4 $ drop 2 $ orderBy (on compare person_name) persons



sub1 = extendAgg length pets

sub2 = rfilter filt pets
  where filt p = pet_species p == (select max "" $ project pet_species pets)

sub3 = project snd $ ungroup $ rfilter (\(a, b) -> length b == 2) $ groupBy pet_age pets

sub4 = rfilter ff pets
  where ff p = any ((pet_age p) >) $ project pet_age pets

sub5 = rfilter ff pets
  where ff p = elem (pet_age p) $ project person_age persons

sub5_ = semiJoin (\p p2 -> pet_age p == person_age p2) pets persons

sub6 = rfilter fg pets
  where fg p = not $ elem (pet_age p) [1..5]

sub7 = rfilter fh pets
  where fh p = any ((pet_age p) /=) $ project person_age persons

sub8 = antiJoin (\p q -> pet_age p == q) pets [15]
sub8_ = rfilter (\p -> pet_age p /= 15) pets
sub8' = rfilter ((/= 15) . pet_age) pets

sub9 = rfilter ff persons
  where ff p2 = all (\p -> person_age p2 > p) $ project pet_age pets
sub9' = antiJoin (\p2 p -> person_age p2 <= pet_age p) persons pets

sub10 = error "may be pointless???"

sub11 = rfilter fg persons
  where fg p2 = person_age p2 * 2 >= (select max 0 $ project person_age persons)
  
sub12 = rfilter ff persons
  where 
    ff p2 = elem (tup p2) $ project tup filtered
    tup = liftM2 (,) person_id person_age
    filtered = rfilter (\p' -> person_id p' <= 3) persons
    
sub13 = rfilter fg pets
  where fg p = (pet_owner_id p, pet_age p) == (Just 4, 12)
  
shelp p2 p = (Just $ person_id p2) == pet_owner_id p
  
sub14 = semiJoin shelp persons pets

sub15 = antiJoin shelp persons pets

sub16 = avg $ map snd $ groupLift genericLength $ groupBy person_name persons
  where avg xs = (sum xs) / (fromIntegral $ length xs)



-- prime b/c 'pivot' already in use
pivot' = groupLift length $ pivot pet_owner_id pet_species pets



quotient = divideBy pet_owner_id pet_species pets ["cat", "frog"]
quotient' = divide dividend ["cat", "frog"]
  where dividend = project (liftM2 (,) pet_owner_id pet_species) pets


-- -----------------------------------------------------------------
-- some older junk


pr :: Show a => [a] -> IO ()
pr = mapM_ print


maximumOn :: Ord b => (a -> b) -> [a] -> a
maximumOn f xs = maximumBy myF xs
  where
    myF l r = compare (f l) (f r)


ts = [(1, 3, 5, "foo"),
      (1, 4, 7, "bar"),
      (1, 2, 9, "baz"),
      (2, 1, 1, "dog"),
      (2, 5, 2, "cat"),
      (2, 5, 3, "horse"),
      (2, 3, 8, "pig")]
     
one   (a, _, _, _) = a
two   (_, b, _, _) = b
three (_, _, c, _) = c
four  (_, _, _, d) = d


um3 :: forall a b. (Ord a, Ord b) => ([b] -> b) -> (a -> b) -> [a] -> [a]
um3 ch pr rel = rfilter f rel
  where
    f :: a -> Bool
    f x = pr x == top
    top :: b
    top = ch $ project pr rel
    
    
-- SO question:   http://stackoverflow.com/questions/12726549/select-one-value-from-a-group-based-on-order-from-other-columns
soQ = project (fmap (four . head)) $ project (fmap (um3 maximum three)) $ project (fmap (um3 maximum two)) $ groupBy one ts
