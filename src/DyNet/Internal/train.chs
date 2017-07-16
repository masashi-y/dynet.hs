
module DyNet.Internal.Train where

{#import DyNet.Internal.Core #}

import Foreign.C.Types ( CInt )
import Foreign.Ptr
import Foreign.Storable


#include "trainer.h"

{#pointer *CTrainer as CTrainer foreign newtype #}

{#pointer *CSimpleSGDTrainer as SimpleSGDTrainer
    foreign finalizer delete_SimpleSGDTrainer newtype #}

{#pointer *CCyclicalSGDTrainer as CyclicalSGDTrainer
    foreign finalizer delete_CyclicalSGDTrainer newtype #}

{#pointer *CMomentumSGDTrainer as MomentumSGDTrainer
    foreign finalizer delete_MomentumSGDTrainer newtype #}

{#pointer *CAdagradTrainer as AdagradTrainer
    foreign finalizer delete_AdagradTrainer newtype #}

{#pointer *CAdadeltaTrainer as AdadeltaTrainer
    foreign finalizer delete_AdadeltaTrainer newtype #}

{#pointer *CRMSPropTrainer as RMSPropTrainer
    foreign finalizer delete_RMSPropTrainer newtype #}

{#pointer *CAdamTrainer as AdamTrainer
    foreign finalizer delete_AdamTrainer newtype #}


class Trainer a where
    withTrainer :: a -> (Ptr a -> IO b) -> IO b
    castTrainer :: a -> (Ptr CTrainer -> IO b) -> IO b
    castTrainer t f = withTrainer t (f . castPtr)

instance Trainer SimpleSGDTrainer where
    withTrainer = withSimpleSGDTrainer

instance Trainer CyclicalSGDTrainer where
    withTrainer = withCyclicalSGDTrainer

instance Trainer MomentumSGDTrainer where
    withTrainer = withMomentumSGDTrainer

instance Trainer AdagradTrainer where
    withTrainer = withAdagradTrainer

instance Trainer AdadeltaTrainer where
    withTrainer = withAdadeltaTrainer

instance Trainer RMSPropTrainer where
    withTrainer = withRMSPropTrainer

instance Trainer AdamTrainer where
    withTrainer = withAdamTrainer


-- {#fun Trainer_update_subset as ^
--     {`CTrainer', `UIntVector', `UIntVector', `Float'} -> `()' #}

{#fun Trainer_update as update
    `Trainer t' =>
    {castTrainer* `t', `Float'} -> `()' #}

{#fun Trainer_update_epoch as updateEpoch
    `Trainer t' =>
    {castTrainer* `t', `Float'} -> `()' #}

{#fun Trainer_clip_gradients as clipGradient
    `Trainer t' =>
    {castTrainer* `t', `Float'} -> `Float' #}

{#fun Trainer_rescale_and_reset_weight_decay as rescaleAndResetWeightDecay
    `Trainer t' =>
    {castTrainer* `t'} -> `()' #}

{#fun Trainer_status as status
    `Trainer t' =>
    {castTrainer* `t'} -> `()' #}


{#fun init_SimpleSGDTrainer as createSimpleSGDTrainer
    {+S, `Model', `Float', `Float'} -> `SimpleSGDTrainer' #}

{#fun init_CyclicalSGDTrainer as createCyclicalSGDTrainer
    {+S, `Model', `Float', `Float', `Float', `Float', `Float'} -> `CyclicalSGDTrainer' #}

{#fun init_MomentumSGDTrainer as createMomentumSGDTrainer
    {+S, `Model', `Float', `Float', `Float'} -> `MomentumSGDTrainer' #}

{#fun init_AdagradTrainer as createAdagradTrainer
    {+S, `Model', `Float', `Float', `Float'} -> `AdagradTrainer' #}

{#fun init_AdadeltaTrainer as createAdadeltaTrainer
    {+S, `Model', `Float', `Float', `Float'} -> `AdadeltaTrainer' #}

{#fun init_RMSPropTrainer as createRMSPropTrainer
    {+S, `Model', `Float', `Float', `Float', `Float'} -> `RMSPropTrainer' #}

{#fun init_AdamTrainer as createAdamTrainer
    {+S, `Model', `Float', `Float', `Float', `Float', `Float'} -> `AdamTrainer' #}



instance Storable SimpleSGDTrainer where
    sizeOf _ = sizeOfSimpleSGDTrainer
    alignment _ = 4
    peek = undefined
    poke = undefined

instance Storable CyclicalSGDTrainer where
    sizeOf _ = sizeOfCyclicalSGDTrainer
    alignment _ = 4
    peek = undefined
    poke = undefined

instance Storable MomentumSGDTrainer where
    sizeOf _ = sizeOfMomentumSGDTrainer
    alignment _ = 4
    peek = undefined
    poke = undefined

instance Storable AdagradTrainer where
    sizeOf _ = sizeOfAdagradTrainer
    alignment _ = 4
    peek = undefined
    poke = undefined

instance Storable AdadeltaTrainer where
    sizeOf _ = sizeOfAdadeltaTrainer
    alignment _ = 4
    peek = undefined
    poke = undefined

instance Storable RMSPropTrainer where
    sizeOf _ = sizeOfRMSPropTrainer
    alignment _ = 4
    peek = undefined
    poke = undefined

instance Storable AdamTrainer where
    sizeOf _ = sizeOfAdamTrainer
    alignment _ = 4
    peek = undefined
    poke = undefined

{#fun pure size_of_SimpleSGDTrainer as ^ {} -> `Int' #}
{#fun pure size_of_CyclicalSGDTrainer as ^ {} -> `Int' #}
{#fun pure size_of_MomentumSGDTrainer as ^ {} -> `Int' #}
{#fun pure size_of_AdagradTrainer as ^ {} -> `Int' #}
{#fun pure size_of_AdadeltaTrainer as ^ {} -> `Int' #}
{#fun pure size_of_RMSPropTrainer as ^ {} -> `Int' #}
{#fun pure size_of_AdamTrainer as ^ {} -> `Int' #}
