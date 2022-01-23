{-# language TemplateHaskellQuotes #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language ScopedTypeVariables #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language EmptyCase #-}
{-# language DefaultSignatures #-}
{-# language BangPatterns #-}

module Language.Haskell.TH.TraverseCode
  ( TraverseCode (..)
  , sequenceCode
  , genericTraverseCode
  , genericSequenceCode
  ) where
import Generics.Linear
import Language.Haskell.TH.Syntax (Code, Lift (..), Exp (..), Quote, Name)
import qualified Language.Haskell.TH.Syntax as TH
import Language.Haskell.TH.Lib (conE)

-- for instances
import qualified Data.Functor.Product as FProd
import qualified Data.Functor.Sum as FSum
import Data.Functor.Identity
import qualified Data.Sequence.Internal as Seq
import Data.Coerce
import Control.Applicative
import qualified Data.Semigroup as S
import qualified Data.Monoid as M
import qualified Data.Complex as Complex
import qualified Data.Ord as Ord
import qualified Data.Functor.Compose as Compose
import qualified Data.Tree as Tree
import qualified Data.List.NonEmpty as NonEmpty
import GHC.Tuple (Solo)
import Data.Proxy (Proxy)
import qualified GHC.Generics as GHCGenerics
import qualified Data.Array as Ar
import qualified Data.Primitive.Array as PAr
import qualified Data.Primitive.SmallArray as PSmAr
import Data.Foldable (toList)

-- | Containers supporting \"traversal\" in 'Code'.
class TraverseCode t where
  -- | Given a container and a function to fill it with splices,
  -- produce a splice that will generate a container of their results.
  traverseCode :: Quote m => (a -> Code m b) -> t a -> Code m (t b)

  default traverseCode :: (Quote m, GTraverseCode (Rep1 t), Generic1 t) => (a -> Code m b) -> t a -> Code m (t b)
  traverseCode = genericTraverseCode

-- | Given a container of splices, produce a splice that will generate a
-- container of their results.
sequenceCode :: (TraverseCode t, Quote m) => t (Code m a) -> Code m (t a)
sequenceCode = traverseCode id

-- | Like 'sequenceCode', but using the @"Generics.Linear".'Generic1'@ instance.
genericSequenceCode :: (Quote m, GTraverseCode (Rep1 t), Generic1 t) => t (Code m a) -> Code m (t a)
genericSequenceCode = TH.unsafeCodeCoerce . gtraverseCode id . from1

-- | Like 'traverseCode', but using the @"Generics.Linear".'Generic1'@ instance.
genericTraverseCode :: (Quote m, GTraverseCode (Rep1 t), Generic1 t) => (a -> Code m b) -> t a -> Code m (t b)
genericTraverseCode f = TH.unsafeCodeCoerce . gtraverseCode f . from1

class GTraverseCode f where
  gtraverseCode :: Quote m => (a -> Code m b) -> f a -> m Exp

instance (Datatype c, GTraverseCodeCon f) => GTraverseCode (D1 c f) where
  gtraverseCode f m@(M1 x) = gtraverseCodeCon pkg modl f x
    where
      pkg = packageName m
      modl = moduleName m

class GTraverseCodeCon f where
  gtraverseCodeCon :: Quote m => String -> String -> (a -> Code m b) -> f a -> m Exp

-- This instance seems totally useless, but it's obviously valid.
instance GTraverseCodeCon V1 where
  gtraverseCodeCon _pkg _modl _f x = case x of

instance (GTraverseCodeCon f, GTraverseCodeCon g) => GTraverseCodeCon (f :+: g) where
  gtraverseCodeCon pkg modl f (L1 x) = gtraverseCodeCon pkg modl f x
  gtraverseCodeCon pkg modl f (R1 y) = gtraverseCodeCon pkg modl f y

instance (Constructor c, GTraverseCodeFields f) => GTraverseCodeCon (C1 c f) where
  gtraverseCodeCon pkg modl f m@(M1 x) = gtraverseCodeFields (conE conN) f x
    where
      conBase = conName m
      conN :: Name
      conN = TH.mkNameG_d pkg modl conBase

class GTraverseCodeFields f where
  gtraverseCodeFields :: Quote m => m Exp -> (a -> Code m b) -> f a -> m Exp

instance GTraverseCodeFields f => GTraverseCodeFields (S1 c f) where
  gtraverseCodeFields c f (M1 x) = gtraverseCodeFields c f x

instance (GTraverseCodeFields f, GTraverseCodeFields g) => GTraverseCodeFields (f :*: g) where
  gtraverseCodeFields c f (x :*: y) =
    gtraverseCodeFields (gtraverseCodeFields c f x) f y

instance Lift p => GTraverseCodeFields (K1 i p) where
  gtraverseCodeFields c _f (K1 x) = [| $c x |]

instance GTraverseCodeFields Par1 where
  gtraverseCodeFields cc f (Par1 ca) = [| $cc $(TH.unTypeCode (f ca)) |]

instance GTraverseCodeFields U1 where
  gtraverseCodeFields cc _f U1 = cc

instance (GTraverseCodeFields f, TraverseCode g) => GTraverseCodeFields (f :.: g) where
  gtraverseCodeFields cc f (Comp1 x) =
    gtraverseCodeFields cc (traverseCode f) x

instance GTraverseCodeFields (URec Char) where
  gtraverseCodeFields c _f (UChar ch) = [| $c (UChar ch) |]

instance GTraverseCodeFields (URec Float) where
  gtraverseCodeFields c _f (UFloat ch) = [| $c (UFloat ch) |]

instance GTraverseCodeFields (URec Double) where
  gtraverseCodeFields c _f (UDouble ch) = [| $c (UDouble ch) |]

instance GTraverseCodeFields (URec Int) where
  gtraverseCodeFields c _f (UInt ch) = [| $c (UInt ch) |]

instance GTraverseCodeFields (URec Word) where
  gtraverseCodeFields c _f (UWord ch) = [| $c (UWord ch) |]

instance GTraverseCodeFields f => GTraverseCodeFields (MP1 m f) where
  gtraverseCodeFields c f (MP1 x) = gtraverseCodeFields c f x

-- Would an instance for URec (Ptr ()) make any sense?

-- TraverseCode instances

instance TraverseCode Maybe
instance TraverseCode Identity
instance TraverseCode []
instance TH.Lift a => TraverseCode (Either a)

instance TH.Lift a => TraverseCode ((,) a)
instance (TH.Lift a, TH.Lift b) => TraverseCode ((,,) a b)
instance (TH.Lift a, TH.Lift b, TH.Lift c) => TraverseCode ((,,,) a b c)
instance (TH.Lift a, TH.Lift b, TH.Lift c, TH.Lift d) => TraverseCode ((,,,,) a b c d)
instance (TH.Lift a, TH.Lift b, TH.Lift c, TH.Lift d, TH.Lift e) => TraverseCode ((,,,,,) a b c d e)

instance (TraverseCode f, TraverseCode g) => TraverseCode (FProd.Product f g)
instance (TraverseCode f, TraverseCode g) => TraverseCode (FSum.Sum f g)
instance Lift a => TraverseCode (Const a)

instance TraverseCode V1
instance TraverseCode U1
instance (TraverseCode f, TraverseCode g) => TraverseCode (f :*: g)
instance (TraverseCode f, TraverseCode g) => TraverseCode (f :+: g)
instance TraverseCode f => TraverseCode (M1 i c f)
instance TraverseCode Par1
instance Lift a => TraverseCode (K1 i a)
instance (TraverseCode f, TraverseCode g) => TraverseCode (f :.: g)
instance (TraverseCode f, TraverseCode g) => TraverseCode (Compose.Compose f g)
instance TraverseCode f => TraverseCode (MP1 m f)
instance TraverseCode f => TraverseCode (GHCGenerics.Rec1 f)
instance (TraverseCode f, TraverseCode g) => TraverseCode (f GHCGenerics.:.: g)

-- The Elem instance isn't needed for the Seq instance
instance TraverseCode Seq.Elem
instance TraverseCode Seq.Digit
instance TraverseCode Seq.Node
instance TraverseCode Seq.FingerTree
instance TraverseCode Seq.Seq where
  -- This wonky way of doing it makes for a more compact
  -- splice.
  traverseCode f s = [|| coerceFT $$(traverseCode f ft') ||]
    where
      ft' = coerceSeq (Seq.zipWith (flip const) (Seq.replicate (Seq.length s) ()) s)

coerceFT :: Seq.FingerTree a -> Seq.Seq a
coerceFT = coerce
coerceSeq :: Seq.Seq a -> Seq.FingerTree a
coerceSeq = coerce

instance TraverseCode Seq.ViewL
instance TraverseCode Seq.ViewR

instance TraverseCode ZipList
instance TraverseCode Complex.Complex
instance TraverseCode S.First
instance TraverseCode M.First
instance TraverseCode S.Last
instance TraverseCode M.Last
instance TraverseCode S.Min
instance TraverseCode S.Max
instance TraverseCode Ord.Down
instance TraverseCode S.WrappedMonoid
instance TraverseCode S.Dual
instance TraverseCode S.Product
instance TraverseCode S.Sum
instance TraverseCode Solo
instance TraverseCode Tree.Tree
instance TraverseCode NonEmpty.NonEmpty
-- instance TraverseCode TH.TyVarBndr  (Lift Name isn't in base)
instance TraverseCode m => TraverseCode (WrappedMonad m)
instance TraverseCode (p a) => TraverseCode (WrappedArrow p a)
instance TH.Lift a => TraverseCode (S.Arg a)
instance TraverseCode Proxy
instance TraverseCode f => TraverseCode (M.Ap f)
instance TraverseCode f => TraverseCode (M.Alt f)
instance TraverseCode (URec Char)
instance TraverseCode (URec Double)
instance TraverseCode (URec Float)
instance TraverseCode (URec Int)
instance TraverseCode (URec Word)

instance (Ar.Ix i, TH.Lift i) => TraverseCode (Ar.Array i) where
  traverseCode f xs = [|| Ar.listArray bnds $$csc ||]
    where
      csc = traverseCode f (Ar.elems xs)
      bnds = Ar.bounds xs

instance TraverseCode PAr.Array where
  traverseCode f ary = case lst of
      [] -> [|| PAr.emptyArray ||]
      [x] -> [|| pure $$(f x) ||]
      x : xs -> [|| unsafeArrayFromListN len $$(f x) $$(traverseCode f xs) ||]
    where
      len = length ary
      lst = toList ary

instance TraverseCode PSmAr.SmallArray where
  traverseCode f ary = case lst of
      [] -> [|| PSmAr.emptySmallArray ||]
      [x] -> [|| pure $$(f x) ||]
      x : xs -> [|| unsafeSmallArrayFromListN len $$(f x) $$(traverseCode f xs) ||]
    where
      len = length ary
      lst = toList ary

-- | Strictly create an array from a nonempty list (represented as
-- a first element and a list of the rest) of a known length. If the length
-- of the list does not match the given length, this makes demons fly
-- out of your nose.
unsafeArrayFromListN :: Int -> a -> [a] -> PAr.Array a
unsafeArrayFromListN n y ys =
  PAr.createArray n y $ \ma ->
    let go !_ix [] = pure ()
        go !ix (x : xs) = do
            PAr.writeArray ma ix x
            go (ix+1) xs
    in go 1 ys

-- | Strictly create an array from a nonempty list (represented as
-- a first element and a list of the rest) of a known length. If the length
-- of the list does not match the given length, this makes demons fly
-- out of your nose.
unsafeSmallArrayFromListN :: Int -> a -> [a] -> PSmAr.SmallArray a
unsafeSmallArrayFromListN n y ys =
  PSmAr.createSmallArray n y $ \ma ->
    let go !_ix [] = pure ()
        go !ix (x : xs) = do
            PSmAr.writeSmallArray ma ix x
            go (ix+1) xs
    in go 1 ys
