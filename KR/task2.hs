{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}


import Data.List ( intercalate )
import Data.Char {- for test cases with characters in main block -}

{-
Dictionaries are one of the most common data structure.
They are associative collections Dict k v
indexed by key of type k with values of type v.

Their inner structure can be different, but they have a common interface:

insert       :: k -> v -> Dict k v -> Dict k v
maybeGet     :: k -> Dict k v -> Maybe v
getOrDefault :: k -> Dict k v -> v -> v
contains     :: k -> Dict k v -> Bool
delete       :: k -> Dict k v -> Dict k v
elems        :: Dict k v -> [v]
keys         :: Dict k v -> [k]
size         :: Dict k v -> Int
empty        :: Dict k v 
-}


-- 1. Populate a following typeclass
--    d denotes the actual type of the dictionary (its type constructor)   

class IDict d k v where
    insert       :: k -> v -> d k v -> d k v
    maybeGet     :: k -> d k v -> Maybe v
    getOrDefault :: k -> d k v -> v -> v
    contains     :: k -> d k v -> Bool
    delete       :: k -> d k v -> d k v
    elems        :: d k v -> [v]
    keys         :: d k v -> [k]
    size         :: d k v -> Int
    empty        :: d k v 



-- 2. Propose a naive implementation of the typeclass above 
data Dict k v = Dict [(k, v)]

instance (Eq k, Eq v) => IDict Dict k v where
    
  insert :: (Eq k, Eq v) => k -> v -> Dict k v -> Dict k v
  insert key value (Dict dictionary) = Dict (dictionary ++ [(key, value)])
  
  maybeGet :: (Eq k, Eq v) => k -> Dict k v -> Maybe v
  maybeGet key (Dict []) = Nothing
  maybeGet key (Dict ((k', v'):xs))   {-Instead of writing our own function maybeGet, we could use built-in one called "lookup". -}
    | key == k' = Just v'
    | otherwise = maybeGet key (Dict xs)

  getOrDefault :: (Eq k, Eq v) => k -> Dict k v -> v -> v
  getOrDefault key dictionary defaultValue =
    case maybeGet key dictionary of
      Nothing         -> defaultValue
      Just valueToGet -> valueToGet

  contains :: (Eq k, Eq v) => k -> Dict k v -> Bool
  contains key dictionary =
    case maybeGet key dictionary of
      Nothing -> False
      Just _  -> True
  
  delete :: (Eq k, Eq v) => k -> Dict k v -> Dict k v
  delete key (Dict dictionary) = Dict (filter (\pair -> (fst pair) /= key) dictionary)

  elems :: (Eq k, Eq v) => Dict k v -> [v]
  elems (Dict dictionary) = map snd dictionary
  
  keys :: (Eq k, Eq v) => Dict k v -> [k]
  keys (Dict dictionary) = map fst dictionary

  size :: (Eq k, Eq v) => Dict k v -> Int
  size (Dict dictionary) = length dictionary

  empty :: (Eq k, Eq v) => Dict k v
  empty = Dict []


  
  
  
  


-- 3. Implement the Show typeclass to represent your dictionary 
--    in the form {key : value} 
instance (Show k, Show v) => Show (Dict k v) where
  show :: Dict k v -> String
  show (Dict []) = "{}"
  show (Dict ((k, v):xs)) = "{ " ++ intercalate ", " (map (\pair -> show (fst pair) ++ " : " ++ show (snd pair)) xs) ++ " }"


-- 4. Implement the Functor typeclass
--    that allows mapping over dictionary
--    Uncomment this block, if you do this
instance Functor (Dict k) where
  fmap :: (v -> p) -> Dict k v -> Dict k p
  fmap f (Dict dict) = Dict (fmap (\(k, v) -> (k, f v)) dict)


-- please, make sure your code runs without errors
-- comment out unsolved tasks

main = do
  print $ fromPairs kvPairs
  print $ fmap toUpper (fromPairs kvPairs)        {- we're mapping over dictionary applying function toUpper to each value, as a result we've got a new Dict -}
  where
    kvPairs   = [(1,'h'), (2,'e'), (3, 'l'), (4,'l'), (5, 'o')]
    fromPairs :: (Eq k, Eq v) => [(k, v)] -> Dict k v
    fromPairs = foldl insert' empty 
    insert' dict (k, v) = insert k v dict