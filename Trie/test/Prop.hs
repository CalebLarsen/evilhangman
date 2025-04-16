{-# OPTIONS_GHC -Wno-orphans #-}
module Prop (module Prop) where
import qualified Trie as T
import Test.QuickCheck
import Data.List (nub)

instance Arbitrary T.Trie where
  arbitrary = fmap T.fromList arbitrary

prop_EmptyNoContains :: String -> Bool
prop_EmptyNoContains s = not $ T.contains s T.empty

prop_EmptySizeZero :: Bool
prop_EmptySizeZero = T.size t == 0
  where t = T.empty

prop_InsertContains :: String -> T.Trie -> Bool
prop_InsertContains s t = T.contains s t'
  where t' = T.insert s t

prefixes :: String -> [String]
prefixes s = reverse $ prefixes' $ reverse s

prefixes' :: String -> [String]
prefixes' "" = [""]
prefixes' (x:xs) =
  let subpre = prefixes' xs
    in case subpre of
      [] -> []
      (s:ss) -> (x:s):s:ss

prop_PrefixesLen :: String -> Bool
prop_PrefixesLen s = length pre == length s + 1
  where pre = prefixes s

prop_NoInsertPrefixes :: String -> T.Trie -> Bool
prop_NoInsertPrefixes s t = truth
  where xs = prefixes s
        t' = T.insert s t
        truth = all (\x -> (T.contains x t == T.contains x t') || (s == x)) xs

prop_UniqInserts :: [String] -> Bool
prop_UniqInserts xs = T.size t == length (nub xs)
  where t = T.fromList xs

prop_Idempotent :: String -> T.Trie -> Bool
prop_Idempotent s t = T.size t' == T.size t''
  where t' = T.insert s t
        t'' = T.insert s t'

prop_Deleted :: Char -> T.Trie -> Bool
prop_Deleted c t = not $ any (elem c) s'
  where t' = T.removeChar c t
        s' = T.getAll t'

runTest :: IO ()
runTest = do
  putStrLn "Prop Tests"
  putStrLn "=========="
  quickCheck prop_EmptyNoContains
  quickCheck prop_EmptySizeZero
  quickCheck prop_InsertContains
  quickCheck prop_PrefixesLen
  quickCheck prop_NoInsertPrefixes
  quickCheck prop_Idempotent
  quickCheck prop_UniqInserts
  quickCheck prop_Deleted
