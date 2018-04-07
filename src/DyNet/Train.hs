
module DyNet.Train (
    Trainer,
    createSimpleSGDTrainer,
    createSimpleSGDTrainer',
    createCyclicalSGDTrainer,
    createCyclicalSGDTrainer',
    createMomentumSGDTrainer,
    createMomentumSGDTrainer',
    createAdagradTrainer,
    createAdagradTrainer',
    createAdadeltaTrainer,
    createAdadeltaTrainer',
    createRMSPropTrainer,
    createRMSPropTrainer',
    createAdamTrainer,
    createAdamTrainer',
    update,
    updateEpoch,
    clipGradient,
    rescaleAndResetWeightDecay,
    status
) where

import DyNet.Internal.Train

{-|
@
    createSimpleSGDTrainer' m = createSimpleSGDTrainer m 0.1
@
-}
createSimpleSGDTrainer' m = createSimpleSGDTrainer m 0.1

{-|
@
    createCyclicalSGDTrainer' m = createCyclicalSGDTrainer m 0.01 0.1 2000 0.0 0.0
@
-}
createCyclicalSGDTrainer' m = createCyclicalSGDTrainer m 0.01 0.1 2000 0.0 0.0

{-|
@
    createMomentumSGDTrainer' m = createMomentumSGDTrainer m 0.01 0.9
@
-}
createMomentumSGDTrainer' m = createMomentumSGDTrainer m 0.01 0.9

{-|
@
    createAdagradTrainer' m = createAdagradTrainer m 0.1 1.0e-20
@
-}
createAdagradTrainer' m = createAdagradTrainer m 0.1 1.0e-20

{-|
@
    createAdadeltaTrainer' m = createAdadeltaTrainer m 1.0e-6 0.95
@
-}
createAdadeltaTrainer' m = createAdadeltaTrainer m 1.0e-6 0.95

{-|
@
    createRMSPropTrainer' m = createRMSPropTrainer m 0.1 1.0e-20 0.95
@
-}
createRMSPropTrainer' m = createRMSPropTrainer m 0.1 1.0e-20 0.95

{-|
@
    createAdamTrainer' m = createAdamTrainer m 0.001 0.9 0.999 1.0e-8
@
-}
createAdamTrainer' m = createAdamTrainer m 0.001 0.9 0.999 1.0e-8

