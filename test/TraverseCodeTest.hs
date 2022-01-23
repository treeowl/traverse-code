-- {-# options -ddump-ds #-}
{-# language TemplateHaskell #-}
module Main (module Main) where

import Language.Haskell.TH.TraverseCode
import qualified Data.Functor.Product as FP
import Data.Functor.Sum
import qualified Data.Sequence as S
import Generics.Linear
import Data.Primitive.SmallArray
import GHC.Exts (fromList)
import Test.Tasty.HUnit
import Test.Tasty

arry0 :: SmallArray a
arry0 = $$(sequenceCode (fromList []))

arry1 :: a -> SmallArray a
arry1 a = $$(sequenceCode (fromList [ [||a||]]))

arry2 :: a -> a -> SmallArray a
arry2 a b = $$(sequenceCode (fromList [[||a||], [||b||]]))

hah :: Maybe a -> MP1 m Maybe a
hah Nothing = $$(sequenceCode (MP1 Nothing))
hah (Just x) = $$(sequenceCode (MP1 (Just [||x||])))

ho :: a -> a -> MP1 m [] a
ho x y = $$(sequenceCode (MP1 [ [||x||], [||y||] ]))

boom :: a -> (Int, a)
boom a = $$(sequenceCode (12, [|| a ||]))

zorp :: Bool -> a -> Either Int a
zorp False a = $$(sequenceCode (Left 4))
zorp True a = $$(sequenceCode (Right [||a||]))

goo :: a -> a -> Maybe a -> FP.Product [] Maybe a
goo x1 x2 Nothing = $$(sequenceCode (FP.Pair [ [||x1||], [||x2||] ] Nothing))
goo x1 x2 (Just y) = $$(sequenceCode (FP.Pair [ [||x1||], [||x2||] ] (Just [||y||])))

emptySeq :: S.Seq a
emptySeq = $$(sequenceCode (S.fromList []))

single :: a -> S.Seq a
single a = $$(sequenceCode (S.fromList [ [||a||] ]))

double :: a -> a -> S.Seq a
double a b = $$(sequenceCode (S.fromList [ [||a||], [||b||] ]))

triple :: a -> a -> a -> S.Seq a
triple a b c = $$(sequenceCode (S.fromList [ [||a||], [||b||], [||c||] ]))

seq10 :: a -> a -> a -> a -> a ->
         a -> a -> a -> a -> a -> S.Seq a
seq10 a b c d e  f g h i j = $$(traverseCode id (S.fromList
   [ [||a||], [||b||], [||c||], [||d||], [||e||], [||f||], [||g||], [||h||],[||i||],[||j||] ]))

seq11 :: a -> a -> a -> a -> a ->
         a -> a -> a -> a -> a -> a -> S.Seq a
seq11 a b c d e  f g h i j k = $$(traverseCode id $ S.reverse (S.fromList
   [ [||a||], [||b||], [||c||], [||d||], [||e||], [||f||], [||g||], [||h||],[||i||],[||j||],[||k||] ]))

main :: IO ()
main = defaultMain $ testGroup "properties" [
  testCase "ary" $ do
    (emptySmallArray :: SmallArray Int) @=? arry0
    fromList [10 :: Int] @=? arry1 10
    fromList [20, 30] @=? arry2 20 30
  ]
