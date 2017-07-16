{-# LANGUAGE TypeFamilies #-}

module DyNet.Core (
    Model,
    Parameter,
    LookupParameter,
    Expression,
    Tensor,
    ComputationGraph,
    createModel,
    withNewComputationGraph,
    asScalar,
    asVector,
    addParameters,
    addParameters',
    addParametersWith,
    addParametersWith',
    addLookupParameters,
    addLookupParameters',
    addLookupParametersWith,
    addLookupParametersWith',
    printGraphviz,
    forward,
    backward,
    initialize,
    initialize',
    argmax,
    Debug(..),
    Dim,
    dim,
    dimSize,
    dimBatchElems,
    dimSumDims,
    dimTruncate,
    dimResize,
    dimNdims,
    dimRows,
    dimCols,
    dimAt,
    dimSet,
    dimTranspose,
    initNormal,
    initUniform,
    initConst,
    initIdentity,
    initGlorot,
    initSaxe,
    initFromFile,
    initFromVector
) where

import DyNet.Internal.Core
import DyNet.Internal.ExpVector
import Foreign.ForeignPtr ( finalizeForeignPtr )
import Data.List ( maximumBy )

initialize' :: [String] -> IO [String]
initialize' argv = initialize argv False

withNewComputationGraph :: (ComputationGraph -> IO a) -> IO a
withNewComputationGraph f = do
    cg@(ComputationGraph ptr) <- createComputationGraph
    res <- f cg
    -- deleteComputationGraph cg
    return res

argmax :: [Float] -> Int
argmax list = fst $ maximumBy (\(_, m) (_, n) -> compare m n) $ zip [0..] list

class Debug a where
    print_ :: a -> IO ()

instance Debug Dim where
    print_ = dimDebug

instance Debug Tensor where
    print_ = tensorDebug


addParameters' :: Dimension d => Model -> d -> IO Parameter
addParameters' m d = addParameters m d ""

addParametersWith' :: Dimension d => Model -> d -> ParameterInit -> IO Parameter
addParametersWith' m d init = addParametersWith m d init ""

addLookupParameters' :: Dimension d => Model -> Int -> d -> IO LookupParameter
addLookupParameters' m n d = addLookupParameters m n d ""

addLookupParametersWith' :: Dimension d => Model -> Int -> d -> ParameterInit -> IO LookupParameter
addLookupParametersWith' m n d init = addLookupParametersWith m n d init ""

