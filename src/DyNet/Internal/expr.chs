{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module DyNet.Internal.Expr where

{#import DyNet.Internal.Core #}
{#import DyNet.Internal.Vector #}
{#import DyNet.Internal.ExpVector #}

import qualified Foreign.Ptr as C2HSImp
import Foreign.Storable
---import DyNet.Vector

#include "dynet.h"


class IsExpr a where
    withExpr :: a -> (C2HSImp.Ptr Expression -> IO b) -> IO b

instance IsExpr Expression where
    withExpr = withExpression

instance IsExpr (IO Expression) where
    withExpr exp g = exp >>= (\x -> withExpression x g)


class FloatSequence s where
    withFS :: s -> (C2HSImp.Ptr (Vector Float) -> IO b) -> IO b

instance FloatSequence [Float] where
    withFS s f = fromList s >>= (\s' -> withFloatVector s' f)

instance FloatSequence FloatVector where
    withFS = withFloatVector

-- parameter
{#fun c_parameter as parameter
    {+S, `ComputationGraph', `Parameter'} -> `Expression' #} 

-- input
{#fun c_input_1 as input
    `(FloatSequence fs)' =>
    {+S,
     `ComputationGraph',
     withDim* `Dim',
     withFS* `fs'} -> `Expression' #} 

-- tanh
{#fun c_tanh as tanh
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #} 

-- add
{#fun c_op_add as c_add -- not allowed "add" here
    `(IsExpr ex1, IsExpr ex2)' =>
    {+S, withExpr* `ex1', withExpr* `ex2'} -> `Expression' #} 

add :: (IsExpr ex1, IsExpr ex2) => ex1 -> ex2 -> IO Expression
add = c_add

-- mul
{#fun c_op_mul as mul
    `(IsExpr ex1, IsExpr ex2)' =>
    {+S, withExpr* `ex1', withExpr* `ex2'} -> `Expression' #} 

-- squaredDistance
{#fun c_squared_distance as squaredDistance
    `(IsExpr ex1, IsExpr ex2)' =>
    {+S, withExpr* `ex1', withExpr* `ex2'} -> `Expression' #} 

withExpList list f = fromList list >>= (\x -> withExpressionVector x f)

-- affine_transform
{#fun c_affine_transform as affineTransform
    {+S, withExpList* `[Expression]'} -> `Expression' #} 

-- lookup
{#fun c_lookup_1 as lookup'
     {+S, `ComputationGraph', `LookupParameter', `UIntVector'} -> `Expression' #} 

-- concatenate
{#fun c_concat as concat
    {+S,  withExpList* `[Expression]', `Int'} -> `Expression' #} 

-- pickneglogsoftmax
{#fun c_pickneglogsoftmax as pickneglogsoftmax
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Int'} -> `Expression' #} 

-- sum
{#fun c_sum as sum
    {+S,  withExpList* `[Expression]'} -> `Expression' #} 

