{-# LANGUAGE OverloadedStrings #-}

module DyNet.Dict (
    Dict,
    createDict,
    contains,
    fromId,
    fromWord
) where

import qualified Data.Text as T
import qualified Data.HashTable.IO as H
import Data.Tuple ( swap )
import Data.Maybe ( isJust )
import Prelude hiding ( Word )

type Word = T.Text
type Count = Int
type ID = Int
type Table a b = H.BasicHashTable a b

data Dict = Dict { wordToId :: Table Word ID
                 , idToWord :: Table ID   Word
                 , unk :: Maybe Word }

createDict :: [Word] -> Maybe Word -> IO Dict
createDict list unk =
    Dict <$> (H.fromList list') <*> (H.fromList $ map swap list') <*> (return unk)
    where list' = zip list [1..]

contains :: Dict -> Word -> IO Bool
contains (Dict dict _ _) word = H.lookup dict word >>= (return . isJust)

fromId :: Dict -> ID -> IO Word
fromId (Dict _ dict unk) k = do 
    res <- H.lookup dict k
    case (res, unk) of
        (Just w, _)       -> return w
        (Nothing, Just w) -> return w
        _   -> error $ "Key out of range: " ++ show k

fromWord :: Dict -> Word -> IO ID
fromWord (Dict dict _ unk) k = do 
    res <- H.lookup dict k
    case (res, unk) of
        (Just i, _)       -> return i
        (Nothing, Just _) -> return 0
        _   -> error $ "Unknown key encountered: " ++ (show . T.unpack) k
