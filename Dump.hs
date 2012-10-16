module Dump (

    Person(..)
  , Pet(..)
  
  , persons
  , pets

) where


data Person = Person {
        person_id      :: Integer,
        person_name    :: String,
        person_age     :: Integer,
        person_weight  :: Integer
    } deriving (Eq, Show, Ord)
    
    
data Pet = Pet {
        pet_id        :: Integer,
        pet_owner_id  :: Maybe Integer,
        pet_name      :: String,
        pet_species   :: String,
        pet_age       :: Integer
    } deriving (Eq, Show, Ord)


persons :: [Person]
persons = [
          Person  1  "Matt"   27  160,
          Person  2  "Kelly"  39  110,
          Person  3  "Abe"    15  97,
          Person  4  "Vera"   21  135,
          Person  5  "Rob"    52  158,
          Person  6  "Matt"   79  143,
          Person  7  "Vera"   31  129]

pets = [
      Pet  1   (Just 1)   "King Kong"  "dog"   17,
      Pet  2   (Just 3)   "Bozo"       "cat"   10,
      Pet  3   (Just 4)   "Hector"     "frog"   2,
      Pet  4   (Just 3)   "Ike"        "cat"    8,
      Pet  5   Nothing    "Achilles"   "cat"   15,
      Pet  6   Nothing    "Ike"        "frog"   1,
      Pet  7   (Just 4)   "Ike"        "cat"   12,
      Pet  8   (Just 4)   "Yoda"       "dog"   12,
      Pet  9   (Just 4)   "Clifford"   "frog"   2]