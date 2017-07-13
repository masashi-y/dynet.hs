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
    addLookupParameters,
    printGraphviz,
    forward,
    backward,
    initialize,
    initialize',
    argmax
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
    deleteComputationGraph cg
    return res

argmax :: [Float] -> Int
argmax list = fst $ maximumBy (\(_, m) (_, n) -> compare m n) $ zip [0..] list

