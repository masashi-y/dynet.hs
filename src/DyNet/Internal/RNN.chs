{-# LANGUAGE FlexibleContexts #-}

module DyNet.Internal.RNN where

{#import DyNet.Internal.Core #}
{#import DyNet.Internal.Vector #}
{#import DyNet.Internal.ExpVector #}

-- import qualified Foreign.Ptr as C2HSImp
import Foreign.Storable
import Foreign.Ptr
import Foreign.ForeignPtr ( castForeignPtr )
import Foreign.C.Types ( CInt )

#include "rnn.h"

{#pointer *CRNNBuilder as RNNBuilder foreign newtype #}

{#pointer *CSimpleRNNBuilder as SimpleRNNBuilder
    foreign finalizer delete_SimpleRNNBuilder newtype #}

{#pointer *CVanillaLSTMBuilder as VanillaLSTMBuilder
    foreign finalizer delete_VanillaLSTMBuilder newtype #}

{#pointer *CCoupledLSTMBuilder as CoupledLSTMBuilder
    foreign finalizer delete_CoupledLSTMBuilder newtype #}

{#pointer *CFastLSTMBuilder as FastLSTMBuilder
    foreign finalizer delete_FastLSTMBuilder newtype #}

{#pointer *CGRUBuilder as GRUBuilder
    foreign finalizer delete_GRUBuilder newtype #}


class RNN a where
    withRNN :: a -> (Ptr a -> IO b) -> IO b
    castRNN :: a -> (Ptr RNNBuilder -> IO b) -> IO b
    castRNN t f = withRNN t (f . castPtr)

    setDropout :: a -> Float -> IO ()
    setDropoutRate :: a -> Float -> Float -> Float -> IO ()
    disableDropout :: a -> IO ()
    setDropoutMasks :: a -> Int -> IO ()

instance RNN SimpleRNNBuilder where
    withRNN = withSimpleRNNBuilder
    setDropout (SimpleRNNBuilder rnn) r = rNNBuilderSetDropout (RNNBuilder $ castForeignPtr rnn) r
    setDropoutRate = error "no implementation"
    disableDropout (SimpleRNNBuilder rnn) = rNNBuilderDisableDropout (RNNBuilder $ castForeignPtr rnn)
    setDropoutMasks = error "no implementation"

instance RNN VanillaLSTMBuilder where
    withRNN = withVanillaLSTMBuilder
    setDropout = vanillaLSTMBuilderSetDropout
    setDropoutRate d dr _ = vanillaLSTMBuilderSetDropoutRate d dr
    disableDropout = vanillaLSTMBuilderDisableDropout
    setDropoutMasks = vanillaLSTMBuilderSetDropoutMasks

instance RNN CoupledLSTMBuilder where
    withRNN = withCoupledLSTMBuilder
    setDropout = coupledLSTMBuilderSetDropout
    setDropoutRate = coupledLSTMBuilderSetDropoutRate
    disableDropout = coupledLSTMBuilderDisableDropout
    setDropoutMasks = coupledLSTMBuilderSetDropoutMasks

instance RNN FastLSTMBuilder where
    withRNN = withFastLSTMBuilder
    setDropout (FastLSTMBuilder rnn) r = rNNBuilderSetDropout (RNNBuilder $ castForeignPtr rnn) r
    setDropoutRate = error "no implementation"
    disableDropout (FastLSTMBuilder rnn) = rNNBuilderDisableDropout (RNNBuilder $ castForeignPtr rnn)
    setDropoutMasks = error "no implementation"

instance RNN GRUBuilder where
    withRNN = withGRUBuilder
    setDropout (GRUBuilder rnn) r = rNNBuilderSetDropout (RNNBuilder $ castForeignPtr rnn) r
    setDropoutRate = error "no implementation"
    disableDropout (GRUBuilder rnn) = rNNBuilderDisableDropout (RNNBuilder $ castForeignPtr rnn)
    setDropoutMasks = error "no implementation"


{#fun state as state
    `RNN r' =>
    {castRNN* `r'} -> `Int' #}

{#fun RNNBuilder_new_graph as newGraph
    `RNN r' =>
    {castRNN* `r', `ComputationGraph', `Bool'} -> `()' #}

{#fun RNNBuilder_start_new_sequence as startNewSequence
    `(RNN r, Sequence Expression s)' =>
    {castRNN* `r', withSequence* `s'} -> `()' #}

{#fun RNNBuilder_set_h as set_h
    `(RNN r, Sequence Expression s)' =>
    {castRNN* `r', +S, `Int', withSequence* `s'} -> `Expression' #}

{#fun RNNBuilder_set_s as set_s
    `(RNN r, Sequence Expression s)' =>
    {castRNN* `r', +S, `Int', withSequence* `s'} -> `Expression' #}

{#fun RNNBuilder_add_input as addInput
    `(RNN r, IsExpr ex)' =>
    {castRNN* `r', +S, withExpr* `ex'} -> `Expression' #}

{#fun RNNBuilder_add_input_prev as addInputPrev
    `(RNN r, IsExpr ex)' =>
    {castRNN* `r', +S, `Int', withExpr* `ex'} -> `Expression' #}

{#fun RNNBuilder_rewind_one_step as rewindOneStep
    `RNN r' =>
    {castRNN* `r'} -> `()' #}

{#fun RNNBuilder_get_head as getHead
    `RNN r' =>
    {castRNN* `r', `Int'} -> `()' #}

{#fun RNNBuilder_set_dropout as ^
    {`RNNBuilder', `Float'} -> `()' #}

{#fun RNNBuilder_disable_dropout as ^
    {`RNNBuilder'} -> `()' #}

{#fun RNNBuilder_back as back
    `RNN r' =>
    {castRNN* `r', +S} -> `Expression' #}

{#fun RNNBuilder_final_h as final_h
    `RNN r' =>
    {castRNN* `r', +S} -> `ExpressionVector' #}

{#fun RNNBuilder_get_h as get_h
    `RNN r' =>
    {castRNN* `r', +S, `Int'} -> `ExpressionVector' #}

{#fun RNNBuilder_final_s as final_s
    `RNN r' =>
    {castRNN* `r', +S} -> `ExpressionVector' #}

{#fun RNNBuilder_get_s as get_s
    `RNN r' =>
    {castRNN* `r', +S, `Int'} -> `ExpressionVector' #}

{#fun RNNBuilder_num_h0_components as num_h0_components
    `RNN r' =>
    {castRNN* `r'} -> `Int' #}

{#fun RNNBuilder_copy as copy
    `RNN r' =>
    {castRNN* `r', `RNNBuilder'} -> `()' #}


-- void RNNBuilder_get_parameter_collection(CRNNBuilder* r, CParameterCollection* out);



{#fun init_SimpleRNNBuilder as createSimpleRNNBuilder
     {+S, `Int', `Int', `Int', `Model', `Bool'} -> `SimpleRNNBuilder' #}

{#fun pure size_of_SimpleRNNBuilder as ^ {} -> `Int' #}

{#fun SimpleRNNBuilder_add_auxiliary_input as addAuxiliaryInput
    `(IsExpr ex1, IsExpr ex2)' =>
    {`SimpleRNNBuilder', +S, withExpr* `ex1', withExpr* `ex2'} -> `Expression' #}




{#fun init_VanillaLSTMBuilder as createVanillaLSTMBuilder
    {+S, `Int', `Int', `Int', `Model', `Bool'} -> `VanillaLSTMBuilder' #}

{#fun pure size_of_VanillaLSTMBuilder as ^ {} -> `Int' #}

{#fun VanillaLSTMBuilder_set_dropout as ^
    {`VanillaLSTMBuilder', `Float'} -> `()' #}

{#fun VanillaLSTMBuilder_set_dropout_rate as ^
    {`VanillaLSTMBuilder', `Float', `Float'} -> `()' #}

{#fun VanillaLSTMBuilder_disable_dropout as ^
    {`VanillaLSTMBuilder'} -> `()' #}

{#fun VanillaLSTMBuilder_set_dropout_masks as ^
    {`VanillaLSTMBuilder', `Int'} -> `()' #}


{#fun init_CoupledLSTMBuilder as createCoupledLSTMBuilder
    {+S, `Int', `Int', `Int', `Model'} -> `CoupledLSTMBuilder' #}

{#fun pure size_of_CoupledLSTMBuilder as ^ {} -> `Int' #}

{#fun CoupledLSTMBuilder_set_dropout as ^
    {`CoupledLSTMBuilder', `Float'} -> `()' #}

{#fun CoupledLSTMBuilder_set_dropout_rate as ^
    {`CoupledLSTMBuilder', `Float', `Float', `Float'} -> `()' #}

{#fun CoupledLSTMBuilder_disable_dropout as ^
    {`CoupledLSTMBuilder'} -> `()' #}

{#fun CoupledLSTMBuilder_set_dropout_masks as ^
    {`CoupledLSTMBuilder', `Int'} -> `()' #}



{#fun init_FastLSTMBuilder as createFastSTMBuilder
    {+S, `Int', `Int', `Int', `Model'} -> `FastLSTMBuilder' #}

{#fun pure size_of_FastLSTMBuilder as ^ {} -> `Int' #}


{#fun init_GRUBuilder as createGRUuilder
    {+S, `Int', `Int', `Int', `Model'} -> `GRUBuilder' #}

{#fun pure size_of_GRUBuilder as ^ {} -> `Int' #}


instance Storable SimpleRNNBuilder where
    sizeOf _ = sizeOfSimpleRNNBuilder
    alignment _ = 4
    peek = undefined
    poke = undefined

instance Storable VanillaLSTMBuilder where
    sizeOf _ = sizeOfVanillaLSTMBuilder
    alignment _ = 4
    peek = undefined
    poke = undefined

instance Storable CoupledLSTMBuilder where
    sizeOf _ = sizeOfCoupledLSTMBuilder
    alignment _ = 4
    peek = undefined
    poke = undefined

instance Storable FastLSTMBuilder where
    sizeOf _ = sizeOfFastLSTMBuilder
    alignment _ = 4
    peek = undefined
    poke = undefined

instance Storable GRUBuilder where
    sizeOf _ = sizeOfGRUBuilder
    alignment _ = 4
    peek = undefined
    poke = undefined

