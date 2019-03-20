{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Data.Massiv.Core.IndexSpec (SzNE(..), SzIx(..), DimIx(..), spec) where

import Control.Monad
import Data.Functor.Identity
import Data.Massiv.Core.Index
import Data.Proxy
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function

-- | Size that will result in a non-empty array
newtype SzNE ix = SzNE (Sz ix) deriving Show

-- | Dimension that is always within bounds of an index
newtype DimIx ix = DimIx Dim deriving Show

-- | Non-empty size together with an index that is within bounds of that index.
data SzIx ix = SzIx (Sz ix) ix deriving Show

instance (Index ix, Arbitrary ix) => Arbitrary (Sz ix) where
  arbitrary = do
    sz <- Sz . liftIndex abs <$> arbitrary
    if totalElem sz > 200000
      then arbitrary
      else return sz

instance (Index ix, Arbitrary ix) => Arbitrary (SzNE ix) where
  arbitrary = SzNE . Sz . liftIndex (+1) . unSz <$> arbitrary

instance (Index ix, Arbitrary ix) => Arbitrary (Stride ix) where
  arbitrary = do
    Positive (Small x) <- arbitrary
    Stride . liftIndex ((+1) . (`mod` min 6 x)) <$> arbitrary

instance (Index ix, Arbitrary ix) => Arbitrary (SzIx ix) where
  arbitrary = do
    SzNE sz <- arbitrary
    -- Make sure index is within bounds:
    SzIx sz . flip (liftIndex2 mod) (unSz sz) <$> arbitrary


instance Arbitrary e => Arbitrary (Border e) where
  arbitrary =
    oneof
      [ Fill <$> arbitrary
      , return Wrap
      , return Edge
      , return Reflect
      , return Continue
      ]


instance Index ix => Arbitrary (DimIx ix) where
  arbitrary = do
    n <- arbitrary
    return $ DimIx (1 + (Dim n `mod` dimensions (Proxy :: Proxy ix)))

arbitraryIntIx :: Gen Int
arbitraryIntIx = sized (\s -> resize (floor $ (sqrt :: Double -> Double) $ fromIntegral s) arbitrary)
  -- Generators are quadratic in QuickCheck.


instance Arbitrary Ix2 where
  arbitrary = (:.) <$> arbitraryIntIx <*> arbitraryIntIx

instance Arbitrary Ix3 where
  arbitrary = (:>) <$> arbitraryIntIx <*> ((:.) <$> arbitraryIntIx <*> arbitraryIntIx)

instance Arbitrary Ix4 where
  arbitrary = (:>) <$> arbitraryIntIx <*> arbitrary

instance Arbitrary Ix5 where
  arbitrary = (:>) <$> arbitraryIntIx <*> arbitrary

instance CoArbitrary Ix2 where
  coarbitrary (i :. j) = coarbitrary i . coarbitrary j

instance CoArbitrary Ix3 where
  coarbitrary (i :> ix) = coarbitrary i . coarbitrary ix

instance CoArbitrary Ix4 where
  coarbitrary (i :> ix) = coarbitrary i . coarbitrary ix

instance CoArbitrary Ix5 where
  coarbitrary (i :> ix) = coarbitrary i . coarbitrary ix

instance Function Ix2 where
  function = functionMap fromIx2 toIx2

instance Function Ix3 where
  function = functionMap fromIx3 toIx3

instance Function Ix4 where
  function = functionMap fromIx4 toIx4

instance Function Ix5 where
  function = functionMap fromIx5 toIx5


prop_IsSafeIx :: Index ix => proxy ix -> SzIx ix -> Bool
prop_IsSafeIx _ (SzIx sz ix) = isSafeIndex sz ix

prop_RepairSafeIx :: Index ix => proxy ix -> SzIx ix -> Bool
prop_RepairSafeIx _ (SzIx sz ix) =
  ix == repairIndex sz ix (error "Impossible") (error "Impossible")

prop_UnconsCons :: Index ix => proxy ix -> ix -> Bool
prop_UnconsCons _ ix = ix == uncurry consDim (unconsDim ix)

prop_UnsnocSnoc :: Index ix => proxy ix -> ix -> Bool
prop_UnsnocSnoc _ ix = ix == uncurry snocDim (unsnocDim ix)

prop_ToFromLinearIndex :: Index ix => proxy ix -> SzIx ix -> Property
prop_ToFromLinearIndex _ (SzIx sz ix) =
  isSafeIndex sz ix ==> ix == fromLinearIndex sz (toLinearIndex sz ix)

prop_FromToLinearIndex :: Index ix => proxy ix -> SzNE ix -> NonNegative Int -> Property
prop_FromToLinearIndex _ (SzNE sz) (NonNegative i) =
  totalElem sz >= i ==> i == toLinearIndex sz (fromLinearIndex sz i)

prop_CountElements :: Index ix => proxy ix -> Int -> Sz ix -> Property
prop_CountElements _ thresh sz =
  totalElem sz < thresh ==> totalElem sz ==
  iter zeroIndex (unSz sz) (pureIndex 1) (<) 0 (const (+ 1))

prop_IterMonotonic :: Index ix => proxy ix -> Int -> Sz ix -> Property
prop_IterMonotonic _ thresh sz =
  totalElem sz < thresh ==> fst $
  iter (liftIndex succ zeroIndex) (unSz sz) (pureIndex 1) (<) (True, zeroIndex) mono
  where
    mono curIx (prevMono, prevIx) =
      let isMono = prevMono && prevIx < curIx
       in isMono `seq` (isMono, curIx)


prop_IterMonotonic' :: Index ix => proxy ix -> Int -> Sz ix -> Property
prop_IterMonotonic' _ thresh sz =
  totalElem sz <
  thresh ==>
  if isM
    then isM
    else error (show a)
  where
    (isM, a, _) =
      iter (liftIndex succ zeroIndex) (unSz sz) (pureIndex 1) (<) (True, [], zeroIndex) mono
    mono curIx (prevMono, acc, prevIx) =
      let nAcc = (prevIx, curIx, prevIx < curIx) : acc
          isMono = prevMono && prevIx < curIx
       in isMono `seq` (isMono, nAcc, curIx)


prop_IterMonotonicBackwards' :: Index ix => proxy ix -> Int -> Sz ix -> Property
prop_IterMonotonicBackwards' _ thresh sz@(Sz szix) =
  totalElem sz <
  thresh ==>
  if isM
    then isM
    else error (show a)
  where
    (isM, a, _) = iter (liftIndex pred szix) zeroIndex (pureIndex (-1)) (>=) (True, [], szix) mono
    mono curIx (prevMono, acc, prevIx) =
      let isMono = prevMono && prevIx > curIx
          nAcc = (prevIx, curIx, prevIx > curIx) : acc
       in isMono `seq` (isMono, nAcc, curIx)

prop_IterMonotonicM :: Index ix => proxy ix -> Int -> Sz ix -> Property
prop_IterMonotonicM _ thresh sz =
  totalElem sz < thresh ==> fst $
  runIdentity $ iterM (liftIndex succ zeroIndex) (unSz sz) (pureIndex 1) (<) (True, zeroIndex) mono
  where
    mono curIx (prevMono, prevIx) =
      let isMono = prevMono && prevIx < curIx
       in return $ isMono `seq` (isMono, curIx)


prop_IterMonotonicBackwards :: Index ix => proxy ix -> Int -> Sz ix -> Property
prop_IterMonotonicBackwards _ thresh sz@(Sz szix) =
  totalElem sz < thresh ==> fst $
  iter (liftIndex pred szix) zeroIndex (pureIndex (-1)) (>=) (True, szix) mono
  where
    mono curIx (prevMono, prevIx) =
      let isMono = prevMono && prevIx > curIx
       in isMono `seq` (isMono, curIx)

prop_IterMonotonicBackwardsM :: Index ix => proxy ix -> Int -> Sz ix -> Property
prop_IterMonotonicBackwardsM _ thresh sz@(Sz szix) =
  totalElem sz < thresh ==> fst $
  runIdentity $ iterM (liftIndex pred szix) zeroIndex (pureIndex (-1)) (>=) (True, szix) mono
  where
    mono curIx (prevMono, prevIx) =
      let isMono = prevMono && prevIx > curIx
       in return $ isMono `seq` (isMono, curIx)

prop_LiftLift2 :: Index ix => proxy ix -> ix -> Int -> Bool
prop_LiftLift2 _ ix delta = liftIndex2 (+) ix (liftIndex (+delta) zeroIndex) ==
                            liftIndex (+delta) ix


prop_BorderRepairSafe :: Index ix => proxy ix -> Border ix -> SzNE ix -> ix -> Property
prop_BorderRepairSafe _ border@(Fill defIx) (SzNE sz) ix =
  not (isSafeIndex sz ix) ==> handleBorderIndex border sz id ix == defIx
prop_BorderRepairSafe _ border (SzNE sz) ix =
  not (isSafeIndex sz ix) ==> isSafeIndex sz (handleBorderIndex border sz id ix)


prop_GetDropInsert :: Index ix => proxy ix -> NonNegative Int -> ix -> Property
prop_GetDropInsert _ (NonNegative d) ix =
  expected === do
    i <- getDimM ix dim
    ixL <- dropDimM ix dim
    insertDimM ixL dim i
  where expected = if d >= 1 && dim <= dimensions (Just ix) then Just ix else Nothing
        dim = Dim d

prop_PullOutInsert :: Index ix => proxy ix -> NonNegative Int -> ix -> Property
prop_PullOutInsert _ (NonNegative d) ix =
  expected === do
    (i, ixL) <- pullOutDimM ix dim
    insertDimM ixL dim i
  where expected = if d >= 1 && dim <= dimensions (Just ix) then Just ix else Nothing
        dim = Dim d

prop_UnconsGetDrop :: (Index (Lower ix), Index ix) => proxy ix -> ix -> Property
prop_UnconsGetDrop _ ix =
  Just (unconsDim ix) === do
    i <- getDimM ix (dimensions (Just ix))
    ixL <- dropDimM ix (dimensions (Just ix))
    return (i, ixL)

prop_UnsnocGetDrop :: (Index (Lower ix), Index ix) => proxy ix -> ix -> Property
prop_UnsnocGetDrop _ ix =
  Just (unsnocDim ix) === do
    i <- getDimM ix 1
    ixL <- dropDimM ix 1
    return (ixL, i)

prop_SetAll :: Index ix => proxy ix -> ix -> Int -> Bool
prop_SetAll _ ix i =
  foldM (\cix d -> setDimM cix d i) ix ([1 .. dimensions (Just ix)] :: [Dim]) ==
  Just (pureIndex i)


prop_SetGet :: Index ix => proxy ix -> ix -> DimIx ix -> Int -> Bool
prop_SetGet _ ix (DimIx dim) n = Just n == (setDimM ix dim n >>= (`getDimM` dim))


prop_BorderIx1 :: Positive Int -> Border Char -> Fun Ix1 Char -> SzNE Ix1 -> Ix1 -> Bool
prop_BorderIx1 (Positive period) border getVal (SzNE sz) ix =
  if isSafeIndex sz ix
    then apply getVal ix == val
    else case border of
           Fill defVal -> defVal == val
           Wrap ->
             val ==
             handleBorderIndex
               border
               sz
               (apply getVal)
               (liftIndex2 (+) (liftIndex (* period) (unSz sz)) ix)
           Edge ->
             if ix < 0
               then val == apply getVal (liftIndex (max 0) ix)
               else val ==
                    apply getVal (liftIndex2 min (liftIndex (subtract 1) (unSz sz)) ix)
           Reflect ->
             val ==
             handleBorderIndex
               border
               sz
               (apply getVal)
               (liftIndex2 (+) (liftIndex (* (2 * signum ix * period)) (unSz sz)) ix)
           Continue ->
             val ==
             handleBorderIndex
               Reflect
               sz
               (apply getVal)
               (if ix < 0
                  then ix - 1
                  else ix + 1)
  where
    val = handleBorderIndex border sz (apply getVal) ix

specDimN :: (Index ix, Arbitrary ix) => proxy ix -> Spec
specDimN proxy = do
  describe "Safety" $ do
    it "isSafeIndex" $ property $ prop_IsSafeIx proxy
    it "RepairSafeIx" $ property $ prop_RepairSafeIx proxy
  describe "Lifting" $
    it "Lift/Lift2" $ property $ prop_LiftLift2 proxy
  describe "Linear" $ do
    it "ToFromLinearIndex" $ property $ prop_ToFromLinearIndex proxy
    it "FromToLinearIndex" $ property $ prop_FromToLinearIndex proxy
  describe "Iterator" $ do
    it "CountElements" $ property $ prop_CountElements proxy 2000000
    it "Monotonic" $ property $ prop_IterMonotonic proxy 2000000
    it "MonotonicBackwards" $ property $ prop_IterMonotonicBackwards proxy 2000000
    it "MonotonicM" $ property $ prop_IterMonotonicM proxy 2000000
    it "MonotonicBackwardsM" $ property $ prop_IterMonotonicBackwardsM proxy 2000000
  describe "Border" $
    it "BorderRepairSafe" $ property $ prop_BorderRepairSafe proxy
  describe "SetGetDrop" $ do
    it "SetAll" $ property $ prop_SetAll proxy
    it "SetGet" $ property $ prop_SetGet proxy
    it "GetDropInsert" $ property $ prop_GetDropInsert proxy
    it "PullOutInsert" $ property $ prop_PullOutInsert proxy

specDim2AndUp
  :: (Index ix, Index (Lower ix), Arbitrary ix)
  => proxy ix -> Spec
specDim2AndUp proxy =
  describe "Higher/Lower" $ do
    it "UnconsCons" $ property $ prop_UnconsCons proxy
    it "UnsnocSnoc" $ property $ prop_UnsnocSnoc proxy
    it "UnconsGetDrop" $ property $ prop_UnconsGetDrop proxy
    it "UnsnocGetDrop" $ property $ prop_UnsnocGetDrop proxy


spec :: Spec
spec = do
  describe "Tuple based indices" $ do
    describe "Ix1T" $ do
      specDimN (Nothing :: Maybe Ix1T)
      it "prop_BorderIx1" $ property prop_BorderIx1
    describe "Ix2T" $ do
      specDimN (Nothing :: Maybe Ix2T)
      specDim2AndUp (Nothing :: Maybe Ix2T)
    describe "Ix3T" $ do
      specDimN (Nothing :: Maybe Ix3T)
      specDim2AndUp (Nothing :: Maybe Ix3T)
    describe "Ix4T" $ do
      specDimN (Nothing :: Maybe Ix4T)
      specDim2AndUp (Nothing :: Maybe Ix4T)
    describe "Ix5T" $ do
      specDimN (Nothing :: Maybe Ix5T)
      specDim2AndUp (Nothing :: Maybe Ix5T)
  describe "Specialized indices" $ do
    describe "Ix2" $ do
      -- These can be used to quickly debug monotonicity
      it "Monotonic'" $
        property $ prop_IterMonotonic' (Nothing :: Maybe Ix2) 20000
      it "MonotonicBackwards'" $
        property $ prop_IterMonotonicBackwards' (Nothing :: Maybe Ix2)20000
      specDimN (Nothing :: Maybe Ix2)
      specDim2AndUp (Nothing :: Maybe Ix2)
    describe "Ix3" $ do
      specDimN (Nothing :: Maybe Ix3)
      specDim2AndUp (Nothing :: Maybe Ix3)
    describe "Ix4" $ do
      specDimN (Nothing :: Maybe Ix4)
      specDim2AndUp (Nothing :: Maybe Ix4)
    describe "Ix5" $ do
      specDimN (Nothing :: Maybe Ix5)
      specDim2AndUp (Nothing :: Maybe Ix5)
