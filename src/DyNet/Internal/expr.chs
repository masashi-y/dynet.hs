{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module DyNet.Internal.Expr where

{#import DyNet.Internal.Core #}

import Foreign.Ptr ( Ptr(..) )
import Foreign.Storable
import DyNet.Vector

#include "dynet.h"


class IsExpression a where
    f :: a -> (Ptr Expression -> IO b) -> IO b

instance IsExpression Expression where
    f = withExpression

instance IsExpression (IO Expression) where
    f exp g = exp >>= (\x -> withExpression x g)


-- parameter
{#fun c_parameter as parameter
    {+S, `ComputationGraph', `Parameter'} -> `Expression' #} 

-- input
{#fun c_input_1 as input
    {+S,
     `ComputationGraph',
     withDim* `Dim',
     withVector `Vector Float'} -> `Expression' #} 

-- tanh
{#fun c_tanh as tanh
    `IsExpression ex' =>
    {+S, f* `ex'} -> `Expression' #} 

-- add
{#fun c_op_add as c_add -- not allowed "add" here
    `(IsExpression ex1, IsExpression ex2)' =>
    {+S, f* `ex1', f* `ex2'} -> `Expression' #} 

add :: (IsExpression ex1, IsExpression ex2) => ex1 -> ex2 -> IO Expression
add = c_add

-- mul
{#fun c_op_mul as mul
    `(IsExpression ex1, IsExpression ex2)' =>
    {+S, f* `ex1', f* `ex2'} -> `Expression' #} 

-- squaredDistance
{#fun c_squared_distance as squaredDistance
    `(IsExpression ex1, IsExpression ex2)' =>
    {+S, f* `ex1', f* `ex2'} -> `Expression' #} 

