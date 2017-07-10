
module DyNet.Internal.Expr where

{#import DyNet.Internal.Core #}

import Foreign.Storable
import DyNet.Vector

#include "dynet.h"

{#fun c_parameter as parameter
    {+S, `ComputationGraph', `Parameter'} -> `Expression' #} 

{#fun c_input_1 as input
    {+S,
     `ComputationGraph',
     withDim* `Dim',
     withVector `Vector Float'} -> `Expression' #} 

{#fun c_tanh as tanh
    {+S, `Expression'} -> `Expression' #} 

{#fun c_op_add as c_add -- not allowed "add" here
    {+S, `Expression', `Expression'} -> `Expression' #} 

add = c_add

{#fun c_op_mul as mul
    {+S, `Expression', `Expression'} -> `Expression' #} 

{#fun c_squared_distance as squaredDistance
    {+S, `Expression', `Expression'} -> `Expression' #} 

