module Trie (module Trie) where

import Data.Map as Map
import Data.Maybe
import Prelude hiding (lookup)
import Data.List (sort, intercalate)

data Trie = Trie {
  children :: Map Char Trie,
  end :: Bool
}

instance Show Trie where
  show t = intercalate "\n" $ getAll t

-- Constructors
empty :: Trie
empty = Trie { children = Map.empty, end = False }

fromList :: [String] -> Trie
fromList = Prelude.foldr Trie.insert Trie.empty

contains :: String -> Trie -> Bool
contains [] t = end t
contains (c:cs) t =
  let mchild = Map.lookup c $ children t
    in case mchild of
      Just a  -> contains cs a
      Nothing -> False

lookup :: String -> Trie -> Maybe Trie
lookup "" t = if end t then Just t else Nothing
lookup (c:cs) t =
  let mchild = Map.lookup c $ children t
      in case mchild of
        Just a -> Trie.lookup cs a
        Nothing -> Nothing

insert :: String -> Trie -> Trie
insert [] t = Trie { children = children t, end = True }
insert (c:cs) t =
  let mchild = Map.lookup c $ children t
      child = Trie.insert cs $ fromMaybe Trie.empty mchild
      kids = Map.insert c child $ children t
        in Trie {children = kids, end = end t}

getAll :: Trie -> [String]
getAll t = sort $ getAll' "" t

getAll' :: String -> Trie -> [String]
getAll' prefix t =
   let cs = children t
       kids = Map.foldrWithKey (\k v xs -> xs ++ getAll' (prefix ++ [k]) v) [] cs
       in if end t then prefix:kids else kids

size :: Trie -> Int
size t =
  let cs = children t
      kids = Map.foldr (\v s -> Trie.size v + s) 0 cs
      in if end t then kids + 1 else kids

instance Eq Trie where
  (==) t1 t2 = s1 == s2
    where s1 = getAll t1
          s2 = getAll t2
  (/=) t1 t2 = not (t1 == t2)

removeChar :: Char -> Trie -> Trie
removeChar c t = Trie {children = kids', end = end t}
  where kids' = Map.mapMaybeWithKey (\k v -> if c == k then Nothing else Just $ removeChar c v) (children t)

findWorstSpot :: Char -> Trie -> (Int, Int)
findWorstSpot c t =
  let indices = findLetterPosCounts c t
      in maxIndex indices

maxIndex :: [Int] -> (Int, Int)
maxIndex [] = (0, 0)
maxIndex (x:xs) = maxIndex' 0 0 x (x:xs)

maxIndex' :: Int -> Int -> Int -> [Int] -> (Int, Int)
maxIndex' i _ m [] = (i, m)
maxIndex' i d m (x:xs) = if x > m then maxIndex' d (d+1) x xs else maxIndex' i (d+1) m xs

unite :: [[Int]] -> [Int]
unite = Prelude.foldr unite' []

unite' :: [Int] -> [Int] -> [Int]
unite' [] [] = []
unite' xs [] = xs
unite' [] ys = ys
unite' (x:xs) (y:ys) = (x+y):unite' xs ys

findLetterPosCounts :: Char -> Trie -> [Int]
findLetterPosCounts c t =
  let child = Map.lookup c (children t)
      count = Trie.size $ fromMaybe Trie.empty child
      childSizes = unite $ elems $ Map.map (findLetterPosCounts c) (children t)
      in count:childSizes

-- abc, abd, bca - a
-- [2, 0, 1]
-- cbc, aac, bca - a
-- [1, 1, 1]

assignChar :: Char -> Int -> Trie -> Trie
assignChar c 0 t =
  let children' = Map.filterWithKey (\k _ -> k == c) (children t)
      in Trie {children = children', end = end t}
assignChar c i t =
  let children' = Map.map (assignChar c (i-1)) (children t)
      in Trie {children = children', end = end t}
