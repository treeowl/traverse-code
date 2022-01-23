{-# language TemplateHaskell #-}
module Main (main) where

import Language.Haskell.TH.TraverseCode
import qualified Data.Functor.Product as FP
import Data.Functor.Sum
import qualified Data.Sequence as S

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
main = putStrLn "Test suite not yet implemented."
