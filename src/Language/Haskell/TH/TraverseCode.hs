{-# options_ghc -Wall #-}
{-# language TemplateHaskellQuotes #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language ScopedTypeVariables #-}
{-# language KindSignatures #-}
{-# language DataKinds #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}
{-# language EmptyCase #-}
{-# language DefaultSignatures #-}

module Language.Haskell.TH.TraverseCode
  ( TraverseCode (..)
  , sequenceCode
  , genericTraverseCode
  , genericSequenceCode
  ) where
import Generics.Linear
import Language.Haskell.TH.Syntax (Code, Lift (..), Exp (..), Quote, Name)
import qualified Language.Haskell.TH.Syntax as TH
--import qualified Language.Haskell.TH.Lib as TH
import Language.Haskell.TH.Lib (conE)
import Data.Kind (Type)

-- for instances
import qualified Data.Functor.Product as FProd
import qualified Data.Functor.Sum as FSum
import Data.Functor.Identity
import qualified Data.Sequence.Internal as Seq
import Data.Coerce
import Control.Applicative

class TraverseCode t where
  traverseCode :: Quote m => (a -> Code m b) -> t a -> Code m (t b)

  default traverseCode :: (Quote m, GTraverseCode (Rep1 t), Generic1 t) => (a -> Code m b) -> t a -> Code m (t b)
  traverseCode = genericTraverseCode

sequenceCode :: (TraverseCode t, Quote m) => t (Code m a) -> Code m (t a)
sequenceCode = traverseCode id

genericSequenceCode :: (Quote m, GTraverseCode (Rep1 t), Generic1 t) => t (Code m a) -> Code m (t a)
genericSequenceCode = TH.unsafeCodeCoerce . gtraverseCode id . from1

genericTraverseCode :: (Quote m, GTraverseCode (Rep1 t), Generic1 t) => (a -> Code m b) -> t a -> Code m (t b)
genericTraverseCode f = TH.unsafeCodeCoerce . gtraverseCode f . from1

class GTraverseCode f where
  gtraverseCode :: Quote m => (a -> Code m b) -> f a -> m Exp

data Goop (d :: Meta) (f :: Type -> Type) a = Goop

instance (Datatype c, GTraverseCodeCon f) => GTraverseCode (D1 c f) where
  gtraverseCode f (M1 x) = gtraverseCodeCon pkg modl f x
    where
      pkg = packageName (Goop @c @f)
      modl = moduleName (Goop @c @f)

class GTraverseCodeCon f where
  gtraverseCodeCon :: Quote m => String -> String -> (a -> Code m b) -> f a -> m Exp

-- This instance seems totally useless, but it's obviously valid.
instance GTraverseCodeCon V1 where
  gtraverseCodeCon _pkg _modl _f x = case x of

instance (GTraverseCodeCon f, GTraverseCodeCon g) => GTraverseCodeCon (f :+: g) where
  gtraverseCodeCon pkg modl f (L1 x) = gtraverseCodeCon pkg modl f x
  gtraverseCodeCon pkg modl f (R1 y) = gtraverseCodeCon pkg modl f y

instance (Constructor c, GTraverseCodeFields f) => GTraverseCodeCon (C1 c f) where
  gtraverseCodeCon pkg modl f (M1 x) = gtraverseCodeFields (conE conN) f x
    where
      conBase = conName (Goop @c @f)
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

-- TraverseCode instances

instance TraverseCode Maybe
instance TraverseCode Identity
instance TraverseCode []
instance TH.Lift a => TraverseCode (Either a)
instance TH.Lift a => TraverseCode ((,) a)
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
