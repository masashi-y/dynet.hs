
module Utils (
    wordCount,
    makeBatch,
    accuracy
) where

import Control.Arrow ( (&&&) )
import Data.List ( sort, group, reverse, nub )
import Control.Monad ( join )

wordCount :: (Eq a, Ord a) => [a] -> [(a, Int)]
wordCount = map (head &&& length) . group . sort


makeBatch :: Int -> [a] -> [[a]]
makeBatch _    [] = []
makeBatch size xs = let (x, xs') = splitAt size xs in x:makeBatch size xs'


accuracy :: Eq a => [[a]] -> [[a]] -> Float
accuracy pred gold = realToFrac correct / realToFrac (length pred')
    where correct = length $ filter (\(p, g) -> p == g) $ zip pred' gold'
          pred' = join pred
          gold' = join gold


