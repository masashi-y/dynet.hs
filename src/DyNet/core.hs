{-# LANGUAGE TypeFamilies #-}

module DyNet.Core (
    Model,
    Parameter,
    LookupParameter,
    Expression,
    Tensor,
    ComputationGraph,
    Dim,
    createModel,
    createComputationGraph,
    asScalar,
    addParameters,
    addLookupParameters,
    printGaphviz,
    forward,
    backward,
    initialize,
) where

import DyNet.Internal.Core
import DyNet.Internal.ExpVector

initialize :: [String] -> Bool -> IO ()
initialize argv shared_parameters = do
    let argc = fromIntegral $ length argv
        argv' = unwords argv
    dynetInitialize argc argv' shared_parameters

