{-# LANGUAGE OverloadedStrings #-}

module DyNet.Dict (
    Dict,
    createDict,
    contains,
    fromIndex,
    fromString
) where

import qualified Data.Text as T
import qualified Data.HashTable.IO as H
import Data.Hashable ( Hashable )
import Data.Tuple ( swap )
import Data.Maybe ( isJust, fromJust )

type Table a b = H.BasicHashTable a b

{-|
 Simple dictionary that is based on hash table.
 It allows bidirectional lookup of key -> value and value -> key
 and /unk/ value that is useful in NLP applications.
-}
data Dict k = Dict { wordToId :: Table k Int
                   , idToWord :: Table Int k
                   , unk :: Maybe k }

{-|
@'createDict' (list :: [k]) unk@ creates 'Dict k' which is
a dictionary type that maps all elements in 'list' to an unique integer value starting from 0.

When 'unk' is @'Some' v@, then it is used in 'fromIndex' and 'fromString' when
the corresponding key is not in the dictionary.
-}
createDict :: (Hashable k, Eq k) => [k] -> Maybe k -> IO (Dict k)
createDict list unk =
    Dict <$> (H.fromList list') <*> (H.fromList $ map swap list') <*> (return unk)
    where list' = zip (unk' ++ list) [0..]
          unk' = maybe [] return unk

{-| @contains dict k@ tests if the key @k@ is contained in the @dict@. -}
contains :: (Hashable k, Eq k) => Dict k -> k -> IO Bool
contains (Dict dict _ _) word = H.lookup dict word >>= (return . isJust)

{-|
@fromIndex dict v@ looks up the key @k@ that corresponds to @v@.

When @v@ is out of range, it returns @unk@ value if the @dict@ is initialized with @'Some' unk@,
or raises an error if it is @None@.
-}
fromIndex :: Dict k -> Int -> IO k
fromIndex (Dict _ dict unk) k = do 
    res <- H.lookup dict k
    case (res, unk) of
        (Just w, _)       -> return w
        (Nothing, Just w) -> return w
        _   -> error $ "Key out of range: " ++ show k

{-|
@fromString dict k@ looks up the value @v@ that corresponds to @k@.

When @k@ is not contained in @dict@, it returns @v@ for @unk@ if it is initialized with @'Some' unk@,
or raises an error if it is @None@.
-}
fromString :: (Hashable k, Eq k, Show k) => Dict k -> k -> IO Int
fromString (Dict dict _ unk) k = do 
    res <- H.lookup dict k
    case (res, unk) of
        (Just i, _)        -> return i
        (Nothing, Just k') -> fromJust <$> H.lookup dict k'
        _   -> error $ "Unknown key encountered: " ++ show k
