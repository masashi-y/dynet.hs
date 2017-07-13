
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

-- e0 = 0.1, edecay = 0.0
createSimpleSGDTrainer' m = createSimpleSGDTrainer m 0.1 0.0

-- e0_min = 0.01,  e0_max = 0.1,  step_size = 2000,  gamma = 0.0,  edecay = 0.0
createCyclicalSGDTrainer' m = createCyclicalSGDTrainer m 0.01 0.1 2000 0.0 0.0

-- e0 = 0.01,  mom = 0.9,  edecay = 0.0
createMomentumSGDTrainer' m = createMomentumSGDTrainer m 0.01 0.9 0.0

-- e0 = 0.1,  eps = 1e-20,  edecay = 0.0
createAdagradTrainer' m = createAdagradTrainer m 0.1 1.0e-20 0.0

-- eps = 1e-6,  rho = 0.95,  edecay = 0.0
createAdadeltaTrainer' m = createAdadeltaTrainer m 1.0e-6 0.95 0.0

-- e0 = 0.1,  eps = 1e-20,  rho = 0.95,  edecay = 0.0
createRMSPropTrainer' m = createRMSPropTrainer m 0.1 1.0e-20 0.95 0.0

-- e0 = 0.001,  beta_1 = 0.9,  beta_2 = 0.999,  eps = 1e-8,  edecay = 0.0
createAdamTrainer' m = createAdamTrainer m 0.001 0.9 0.999 1.0e-8 0.0
