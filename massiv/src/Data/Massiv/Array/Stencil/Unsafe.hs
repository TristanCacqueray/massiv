{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- Module      : Data.Massiv.Array.Stencil.Unsafe
-- Copyright   : (c) Alexey Kuleshevich 2018-2019
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <lehins@yandex.ru>
-- Stability   : experimental
-- Portability : non-portable
--
module Data.Massiv.Array.Stencil.Unsafe
  ( -- * Stencil
    makeUnsafeStencil
  , unsafeMapStencil
  -- ** Deprecated
  , mapStencilUnsafe
  , forStencilUnsafe
  ) where

import Data.Massiv.Array.Delayed.Windowed (Array(..), DW, Window(..),
                                           insertWindow)
import Data.Massiv.Array.Stencil.Internal
import Data.Massiv.Core.Common
import GHC.Exts (inline)


-- | Just as `mapStencilUnsafe` this is an unsafe version of the stencil
-- mapping. Arguments are in slightly different order and the indexing function returns
-- `Nothing` for elements outside the border.
--
-- @since 0.1.7
forStencilUnsafe ::
     (Source r ix e, Manifest r ix e)
  => Array r ix e
  -> Sz ix -- ^ Size of the stencil
  -> ix -- ^ Center of the stencil
  -> ((ix -> Maybe e) -> a)
  -- ^ Stencil function that receives a "get" function as it's argument that can
  -- retrieve values of cells in the source array with respect to the center of
  -- the stencil. Stencil function must return a value that will be assigned to
  -- the cell in the result array. Offset supplied to the "get" function
  -- cannot go outside the boundaries of the stencil.
  -> Array DW ix a
forStencilUnsafe !arr !sSz !sCenter relStencil =
  insertWindow (DArray (getComp arr) sz (stencil (index arr))) window
  where
    !window =
      Window
        { windowStart = sCenter
        , windowSize = windowSz
        , windowIndex = stencil (Just . unsafeIndex arr)
        , windowUnrollIx2 = unSz . fst <$> pullOutSzM windowSz 2
        }
    !sz = size arr
    !windowSz = Sz (liftIndex2 (-) (unSz sz) (liftIndex (subtract 1) (unSz sSz)))
    stencil getVal !ix = inline relStencil $ \ !ixD -> getVal (liftIndex2 (+) ix ixD)
    {-# INLINE stencil #-}
{-# INLINE forStencilUnsafe #-}
{-# DEPRECATED forStencilUnsafe "In favor of `unsafeMapStencil`" #-}


mapStencilUnsafe ::
     Manifest r ix e
  => Border e
  -> Sz ix
  -> ix
  -> ((ix -> e) -> a)
  -> Array r ix e
  -> Array DW ix a
mapStencilUnsafe b sz ix f = unsafeMapStencil b sz ix (const f)
{-# INLINE mapStencilUnsafe #-}
{-# DEPRECATED mapStencilUnsafe "In favor of `unsafeMapStencil`" #-}

-- | This is an unsafe version of `Data.Massiv.Array.Stencil.mapStencil`, that does no
-- take `Stencil` as argument, as such it does no stencil validation. There is no
-- performance difference between the two, but the unsafe version has an advantage of not
-- requiring to deal with `Value` wrapper and has access to the actual index with the
-- array.
--
-- @since 0.5.0
unsafeMapStencil ::
     Manifest r ix e
  => Border e
  -> Sz ix
  -> ix
  -> (ix -> (ix -> e) -> a)
  -> Array r ix e
  -> Array DW ix a
unsafeMapStencil b sSz sCenter stencilF !arr = insertWindow warr window
  where
    !warr = DArray (getComp arr) sz (stencil (borderIndex b arr))
    !window =
      Window
        { windowStart = sCenter
        , windowSize = windowSz
        , windowIndex = stencil (unsafeIndex arr)
        , windowUnrollIx2 = unSz . fst <$> pullOutSzM sSz 2
        }
    !sz = size arr
    !windowSz = Sz (liftIndex2 (-) (unSz sz) (liftIndex (subtract 1) (unSz sSz)))
    stencil getVal !ix = inline (stencilF ix) $ \ !ixD -> getVal (liftIndex2 (+) ix ixD)
    {-# INLINE stencil #-}
{-# INLINE unsafeMapStencil #-}


-- | Similar to `Data.Massiv.Array.Stencil.makeStencil`, but there are no guarantees that the
-- stencil will not read out of bounds memory. This stencil is also a bit more powerful in sense it
-- gets an extra peice of information, namely the exact index for the element it is constructing.
--
-- @since 0.3.0
makeUnsafeStencil
  :: Index ix
  => Sz ix -- ^ Size of the stencil
  -> ix -- ^ Center of the stencil
  -> (ix -> (ix -> e) -> a)
  -- ^ Stencil function.
  -> Stencil ix e a
makeUnsafeStencil !sSz !sCenter relStencil = Stencil sSz sCenter stencil
  where
    stencil getVal !ix =
      Value $ inline $ relStencil ix (unValue . getVal . liftIndex2 (+) ix)
    {-# INLINE stencil #-}
{-# INLINE makeUnsafeStencil #-}
