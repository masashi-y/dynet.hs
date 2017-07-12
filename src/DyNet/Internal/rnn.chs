
module DyNet.Internal.RNN where

{#import DyNet.Internal.Core #}
{#import DyNet.Internal.Vector #}
{#import DyNet.Internal.ExpVector #}

-- import qualified Foreign.Ptr as C2HSImp
import Foreign.Storable
import Foreign.C.Types ( CInt )

#include "rnn.h"


{#pointer *CSimpleRNNBuilder as SimpleRNNBuilder
    foreign finalizer delete_CSimpleRNNBuilder newtype #}


{#fun init_CSimpleRNNBuilder as createSimpleRNNBuilder
    {+S, `Int', `Int', `Int', `Model', `Bool'} -> `SimpleRNNBuilder' #}

{#fun simpleRNN_new_graph as ^
    {`SimpleRNNBuilder', `ComputationGraph', `Bool'} -> `()' #}

{#fun simpleRNN_start_new_sequence as ^
    {`SimpleRNNBuilder', `ExpressionVector'} -> `()' #}

{#fun simpleRNN_add_input as ^
    {`SimpleRNNBuilder', +S, `Expression'} -> `Expression' #}



{#pointer *CVanillaLSTMBuilder as VanillaLSTMBuilder
    foreign finalizer delete_CVanillaLSTMBuilder newtype #}

{#fun init_CVanillaLSTMBuilder as createVanillaLSTMBuilder
    {+S, `Int', `Int', `Int', `Model', `Bool'} -> `VanillaLSTMBuilder' #}

{#fun vanillaLSTM_new_graph as ^
    {`VanillaLSTMBuilder', `ComputationGraph', `Bool'} -> `()' #}

{#fun vanillaLSTM_start_new_sequence as ^
    {`VanillaLSTMBuilder', `ExpressionVector'} -> `()' #}

{#fun vanillaLSTM_add_input as ^
    {`VanillaLSTMBuilder', +S, `Expression'} -> `Expression' #}


instance Storable SimpleRNNBuilder where
    sizeOf _ = fromIntegral $ sizeOfSimpleRNNBuilder
    alignment _ = 4
    peek = undefined
    poke = undefined

instance Storable VanillaLSTMBuilder where
    sizeOf _ = fromIntegral $ sizeOfVanillaLSTMBuilder
    alignment _ = 4
    peek = undefined
    poke = undefined

foreign import ccall "size_of_SimpleRNNBuilder"
    sizeOfSimpleRNNBuilder :: CInt

foreign import ccall "size_of_VanillaLSTMBuilder"
    sizeOfVanillaLSTMBuilder :: CInt
