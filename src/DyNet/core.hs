
module DyNet.Core (
    Model,
    Parameter,
    LookupParameter,
    Expression,
    Tensor,
    ComputationGraph,
    Dim,
    Trainer,
    createModel,
    createComputationGraph,
    createSimpleSGDTrainer,
    update,
    asScalar,
    addParameters,
    printGaphviz,
    forward,
    backward,
    initialize,
) where

import DyNet.Internal.Core

initialize :: [String] -> Bool -> IO ()
initialize argv shared_parameters = do
    let argc = fromIntegral $ length argv
        argv' = unwords argv
    dynetInitialize argc argv' shared_parameters
