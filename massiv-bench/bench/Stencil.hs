{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Main where

import Bench
import Bench.Massiv as A
import Criterion.Main
import Prelude as P





main :: IO ()
main = do
  let t2 = (1600, 1200) :: (Int, Int)
  defaultMain
    [ bgroup
        "Stencil"
        [ bgroup
            "Average Seq"
            [ env
                (return (computeAs U (arrDLightIx2 Seq (tupleToIx2 t2))))
                (bench "Array Ix2" .
                 whnf (computeAs U . A.mapStencil (Fill 0) average3x3Filter))
            , env
                (return (computeAs U (arrDLightIx2 Seq (tupleToIx2 t2))))
                (bench "Unsafe Array Ix2" .
                 whnf (computeAs U . average3x3FilterUnsafe))
            , env
                (return (computeAs U (arrDLightIx2 Seq (tupleToIx2 t2))))
                (bench "Convolve Array Ix2" .
                 whnf (computeAs U . A.mapStencil (Fill 0) average3x3FilterConv))
            ]
        , bgroup
            "Average Par"
            [ env
                (return (computeAs U (arrDLightIx2 Par (tupleToIx2 t2))))
                (bench "Array Ix2" .
                 whnf (computeAs U . A.mapStencil (Fill 0) average3x3Filter))
            , env
                (return (computeAs U (arrDLightIx2 Par (tupleToIx2 t2))))
                (bench "Convolve Array Ix2" .
                 whnf (computeAs U . A.mapStencil (Fill 0) average3x3FilterConv))
            ]
        ]
    -- , bgroup
    --     "Sobel"
    --     [ bgroup
    --         "Horizontal"
    --         [ env
    --             (return (massDLightIx2 Seq (tupleToIx2 t2)))
    --             (bench "Massiv Ix2 U" .
    --              whnf (M.mapStencil (sobelX Edge)))
    --         , env
    --             (return (computeAs U (arrDLightIx2 Seq (tupleToIx2 t2))))
    --             (bench "Array Ix2 U" .
    --              whnf (computeAs U . A.mapStencil (sobelX Edge)))
    --         , env
    --             (return (computeUnboxedS (arrDLightSh2 (tupleToSh2 t2))))
    --             (bench "Repa DIM2 U" . whnf (computeUnboxedS . mapSobelRX))
    --         ]
    --     , bgroup
    --         "Vertical"
    --         [ env
    --             (return (computeAs U (arrDLightIx2 Seq (tupleToIx2 t2))))
    --             (bench "Array Ix2 U" .
    --              whnf (computeAs U . A.mapStencil (sobelY Edge)))
    --         , env
    --             (return (computeUnboxedS (arrDLightSh2 (tupleToSh2 t2))))
    --             (bench "Repa DIM2 U" . whnf (computeUnboxedS . mapSobelRY))
    --         ]
    --     , bgroup
    --         "Operator Fused Seq"
    --         [ env
    --             (return (computeAs U (arrDLightIx2 Seq (tupleToIx2 t2))))
    --             (bench "Array Ix2 U" .
    --              whnf (computeAs U . A.mapStencil (sobelOperator Edge)))
    --         , env
    --             (return (computeUnboxedS (arrDLightSh2 (tupleToSh2 t2))))
    --             (bench "Repa DIM2 U" . whnf (computeUnboxedS . sobelOperatorR))
    --         ]
    --     , bgroup
    --         "Operator Fused Par"
    --         [ env
    --             (return (computeAs U (arrDLightIx2 Par (tupleToIx2 t2))))
    --             (bench "Array Ix2 U" .
    --              whnf (computeAs U . A.mapStencil (sobelOperator Edge)))
    --         , env
    --             (return (computeUnboxedS (arrDLightSh2 (tupleToSh2 t2))))
    --             (bench "Repa DIM2 U" .
    --              whnf (runIdentity . computeUnboxedP . sobelOperatorR))
    --         ]
    --     , bgroup
    --         "Operator Unfused"
    --         [ env
    --             (return (computeAs U (arrDLightIx2 Seq (tupleToIx2 t2))))
    --             (bench "Array Ix2 U" . whnf (sobelOperatorUnfused Edge))
    --         , env
    --             (return (computeUnboxedS (arrDLightSh2 (tupleToSh2 t2))))
    --             (bench "Repa DIM2 U" . whnf sobelOperatorRUnfused)
    --         ]
    --     ]
    ]
