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

{-|
[@brief@] This provides a builder for the simplest RNN with tanh nonlinearity
[@details@] The equation for this RNN is :

\[
    \begin{split}
    h_t=\tanh(W_x x_t + W_h h_{t-1} + b)
    \end{split}
\]
-}
{#pointer *CSimpleRNNBuilder as SimpleRNNBuilder
    foreign finalizer delete_SimpleRNNBuilder newtype #}

{-|
[@brief@] VanillaLSTM allows the creation of a "standard" LSTM, ie with decoupled input and forget gates and no peephole connections
[@details@] This cell runs according to the following dynamics :

\[
   \begin{split}
    i_t & =\sigma(W_{ix}x_t+W_{ih}h_{t-1}+b_i)\\
    f_t & = \sigma(W_{fx}x_t+W_{fh}h_{t-1}+b_f+1)\\
    o_t & = \sigma(W_{ox}x_t+W_{oh}h_{t-1}+b_o)\\
    \tilde{c_t} & = \tanh(W_{cx}x_t+W_{ch}h_{t-1}+b_c)\\
    c_t & = c_{t-1}\circ f_t + \tilde{c_t}\circ i_t\\
    h_t & = \tanh(c_t)\circ o_t\\
   \end{split}
\]
-}
{#pointer *CVanillaLSTMBuilder as VanillaLSTMBuilder
    foreign finalizer delete_VanillaLSTMBuilder newtype #}

{-|
[@brief@] CoupledLSTMBuilder creates an LSTM unit with coupled input and forget gate as well as peepholes connections.
[@details@] More specifically, here are the equations for the dynamics of this cell :

\[
   \begin{split}
    i_t & =\sigma(W_{ix}x_t+W_{ih}h_{t-1}+W_{ic}c_{t-1}+b_i)\\
    \tilde{c_t} & = \tanh(W_{cx}x_t+W_{ch}h_{t-1}+b_c)\\
    c_t & = c_{t-1}\circ (1-i_t) + \tilde{c_t}\circ i_t\\
     & = c_{t-1} + (\tilde{c_t}-c_{t-1})\circ i_t\\
    o_t & = \sigma(W_{ox}x_t+W_{oh}h_{t-1}+W_{oc}c_{t}+b_o)\\
    h_t & = \tanh(c_t)\circ o_t\\
\end{split}
\]
-}
{#pointer *CCoupledLSTMBuilder as CoupledLSTMBuilder
    foreign finalizer delete_CoupledLSTMBuilder newtype #}

{-|
[@details@] FastLSTM replaces the matrices from cell to other units, by diagonal matrices.
-}
{#pointer *CFastLSTMBuilder as FastLSTMBuilder
    foreign finalizer delete_FastLSTMBuilder newtype #}

{#pointer *CGRUBuilder as GRUBuilder
    foreign finalizer delete_GRUBuilder newtype #}


class RNN a where
    withRNN :: a -> (Ptr a -> IO b) -> IO b
    castRNN :: a -> (Ptr RNNBuilder -> IO b) -> IO b
    castRNN t f = withRNN t (f . castPtr)

    {-|
    [@brief@] Set Dropout
    [@parameters@]

        * @d@ Dropout rate
    -}
    setDropout :: a -> Float -> IO ()
    setDropoutRate :: a -> Float -> Float -> Float -> IO ()

    {-|
    [@brief@] Disable Dropout
    [@details@] In general, you should disable dropout at test time
    -}
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


{-|
[@brief@] Get pointer to the current state
[@return@] Pointer to the current state
-}
{#fun state as state
    `RNN r' =>
    {castRNN* `r'} -> `Int' #}

{-|
[@brief@] Initialize with new computation graph
[@details@] call this to reset the builder when you are working with a newly created ComputationGraph object
[@parameters@]

    * @cg@ Computation graph
    * @update@ Update internal parameters while training
-}
{#fun RNNBuilder_new_graph as newGraph
    `RNN r' =>
    {castRNN* `r', `ComputationGraph', `Bool'} -> `()' #}

{-|
[@brief@] Reset for new sequence
[@details@] call this before add_input and after new_graph, when starting a new sequence on the same hypergraph.
[@parameters@]

    * @h_0@ `h_0` is used to initialize hidden layers at timestep 0 to given values
-}
{#fun RNNBuilder_start_new_sequence as startNewSequence
    `(RNN r, Sequence Expression s)' =>
    {castRNN* `r', withSequence* `s'} -> `()' #}

{-|
[@brief@] Explicitly set the output state of a node
[@parameters@]

    * @prev@ Pointer to the previous state
    * @h_new@ The new hidden state

[@return@] The hidden representation of the deepest layer
-}
{#fun RNNBuilder_set_h as set_h
    `(RNN r, Sequence Expression s)' =>
    {castRNN* `r', +S, `Int', withSequence* `s'} -> `Expression' #}

{-|
[@brief@] Set the internal state of a node (for lstms/grus)
[@details@] For RNNs without internal states (SimpleRNN, GRU...), this has the same behaviour as `set_h`
[@parameters@]

    * @prev@ Pointer to the previous state
    * @s_new@ The new state. Can be `{new_c[0],...,new_c[n]}` or `{new_c[0],...,new_c[n], new_h[0],...,new_h[n]}`

[@return@] The hidden representation of the deepest layer
-}
{#fun RNNBuilder_set_s as set_s
    `(RNN r, Sequence Expression s)' =>
    {castRNN* `r', +S, `Int', withSequence* `s'} -> `Expression' #}

{-|
[@brief@] Add another timestep by reading in the variable x
[@parameters@]

    * @x@ Input variable

[@return@] The hidden representation of the deepest layer
-}
{#fun RNNBuilder_add_input as addInput
    `(RNN r, IsExpr ex)' =>
    {castRNN* `r', +S, withExpr* `ex'} -> `Expression' #}

{-|
[@brief@] Add another timestep, with arbitrary recurrent connection.
[@details@] This allows you to define a recurrent connection to `prev`
  rather than to `head[cur]`. This can be used to construct trees, implement beam search, etc.
[@parameters@]

    * @prev@ Pointer to the previous state
    * @x@ Input variable

[@return@] The hidden representation of the deepest layer
-}
{#fun RNNBuilder_add_input_prev as addInputPrev
    `(RNN r, IsExpr ex)' =>
    {castRNN* `r', +S, `Int', withExpr* `ex'} -> `Expression' #}

{-|
[@brief@] Rewind the last timestep
[@details@] - this DOES NOT remove the variables from the computation graph,
  it just means the next time step will see a different previous state.
  You can rewind as many times as you want.
-}
{#fun RNNBuilder_rewind_one_step as rewindOneStep
    `RNN r' =>
    {castRNN* `r'} -> `()' #}

{-|
[@brief@] Return the RNN state that is the parent of `p`
[@details@] - This can be used in implementing complex structures
  such as trees, etc.
-}
{#fun RNNBuilder_get_head as getHead
    `RNN r' =>
    {castRNN* `r', `Int'} -> `()' #}

{#fun RNNBuilder_set_dropout as ^
    {`RNNBuilder', `Float'} -> `()' #}

{#fun RNNBuilder_disable_dropout as ^
    {`RNNBuilder'} -> `()' #}

{-|
[@brief@] Returns node (index) of most recent output
[@return@] Node (index) of most recent output
-}
{#fun RNNBuilder_back as back
    `RNN r' =>
    {castRNN* `r', +S} -> `Expression' #}

{-|
[@brief@] Access the final output of each hidden layer
[@return@] Final output of each hidden layer
-}
{#fun RNNBuilder_final_h as final_h
    `RNN r' =>
    {castRNN* `r', +S} -> `ExpressionVector' #}

{-|
[@brief@] Access the output of any hidden layer
[@parameters@]

    * @i@ Pointer to the step which output you want to access

[@return@] Output of each hidden layer at the given step
-}
{#fun RNNBuilder_get_h as get_h
    `RNN r' =>
    {castRNN* `r', +S, `Int'} -> `ExpressionVector' #}

{-|
[@brief@] Access the final state of each hidden layer
[@details@] This returns the state of each hidden layer,
  in a format that can be used in start_new_sequence
  (i.e. including any internal cell for LSTMs and the likes)
[@return@] vector containing, if it exists, the list of final
  internal states, followed by the list of final outputs for
  each layer
-}
{#fun RNNBuilder_final_s as final_s
    `RNN r' =>
    {castRNN* `r', +S} -> `ExpressionVector' #}

{-|
[@brief@] Access the state of any hidden layer
[@details@] See `final_s` for details
[@parameters@]

    * @i@ Pointer to the step which state you want to access

[@return@] Internal state of each hidden layer at the given step
-}
{#fun RNNBuilder_get_s as get_s
    `RNN r' =>
    {castRNN* `r', +S, `Int'} -> `ExpressionVector' #}

{-|
[@brief@] Number of components in `h_0`
[@return@] Number of components in `h_0`
-}
{#fun RNNBuilder_num_h0_components as num_h0_components
    `RNN r' =>
    {castRNN* `r'} -> `Int' #}

{-|
[@brief@] Copy the parameters of another builder.
[@parameters@]

    * @params@ RNNBuilder you want to copy parameters from.
-}
{#fun RNNBuilder_copy as copy
    `RNN r' =>
    {castRNN* `r', `RNNBuilder'} -> `()' #}


-- void RNNBuilder_get_parameter_collection(CRNNBuilder* r, CParameterCollection* out);



{-|
[@brief@] Builds a simple RNN
[@parameters@]

   * @layers@ Number of layers
   * @input_dim@ Dimension of the input
   * @hidden_dim@ Hidden layer (and output) size
   * @model@ ParameterCollection holding the parameters
   * @support_lags@ Allow for auxiliary output?
-}
{#fun init_SimpleRNNBuilder as createSimpleRNNBuilder
     {+S, `Int', `Int', `Int', `Model', `Bool'} -> `SimpleRNNBuilder' #}

{#fun pure size_of_SimpleRNNBuilder as ^ {} -> `Int' #}

{#fun SimpleRNNBuilder_add_auxiliary_input as addAuxiliaryInput
    `(IsExpr ex1, IsExpr ex2)' =>
    {`SimpleRNNBuilder', +S, withExpr* `ex1', withExpr* `ex2'} -> `Expression' #}




{-|
[@brief@] Constructor for the VanillaLSTMBuilder
[@parameters@]

   * @layers@ Number of layers
   * @input_dim@ Dimention of the input \(x_t\)
   * @hidden_dim@ Dimention of the hidden states \(h_t\) and \(c_t\)
   * @model@ ParameterCollection holding the parameters
   * @ln_lstm@ Whether to use layer normalization
   * @forget_bias@ value(float) to use as bias for the forget gate(default = 1.0)
-}
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


{-|
[@brief@] Constructor for the LSTMBuilder
[@parameters@]

   * @layers@ Number of layers
   * @input_dim@ Dimention of the input \(x_t\)
   * @hidden_dim@ Dimention of the hidden states \(h_t\) and \(c_t\)
   * @model@ ParameterCollection holding the parameters
-}
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

