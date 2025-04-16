{-# OPTIONS_GHC -Wno-orphans #-}
import Prop
import Unit

main :: IO ()
main = do
  Unit.runTest
  Prop.runTest
