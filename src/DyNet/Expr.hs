{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module DyNet.Expr (
    -- * Input operations
    input,
    input'',
    parameter,
    lookupParameter,
    constParameter,
    constLookupParameter,
    lookup,
    -- lookup,
    lookup',
    -- constLookup,
    constLookup',
    zeroes,
    -- ones,
    -- constant,
    randomNormal,
    randomBernoulli,
    randomUniform,
    randomGumbel,

    -- * Arithmetic operations
    neg,
    add,
    mul,
    div,
    sub,
    cdiv,
    cmult,
    colwiseAdd,
    affineTransform,
    sum,
    sumCols,
    sumBatches,
    sumElems,
    momentBatches,
    momentElems,
    momentDim,
    meanElems,
    meanBatches,
    meanDim,
    stdDim,
    stdElems,
    stdBatches,
    average,
    sqrt,
    abs,
    erf,
    tanh,
    exp,
    square,
    cube,
    log,
    lgamma,
    elu,
    selu,
    logistic,
    rectify,
    softsign,
    pow,
    max,
    dotProduct,
    bmin,
    bmax,

    -- * Probability / loss operations
    softmax,
    logSoftmax,
    logSoftmax',
    logsumexp,
    pickneglogsoftmax,
    pickneglogsoftmax',
    hinge,
    hinge',
    -- sparsemax
    -- sparsemaxLoss
    squaredDistance,
    squaredNorm,
    l2Norm,
    huberDistance,
    binaryLogLoss,
    pairwiseRankLoss,
    poissonLoss,
    l1Distance,

    -- * Flow operations
    nobackprop,
    flipGradient,
    reshape,
    transpose,
    selectRows,
    selectCols,
    -- pick,
    pick',
    pickRange,
    pickBatchElems,
    pickBatchElem,
    concatCols,
    concat,
    concat',
    concatToBatch,
    maxDim,
    minDim,

    -- * Noise operations
    noise,
    dropout,
    dropoutBatch,
    dropoutDim,
    blockDropout,

    -- * Convolution operations
    filter1dNarrow,
    kmaxPooling,
    foldRows,
    kmhNgram,
    conv2d,
    conv2d',
    maxpooling2d,

    -- * Tensor operations
    contract3d_1d,
    contract3d_1d',
    contract3d_1d_1d,
    contract3d_1d_1d',

    -- * Linear algebra operations
    inverse,
    logdet,
    traceOfProduct,

    -- * Normalization operations
    layerNorm,
    weightNorm,
) where

import Prelude hiding ( tanh, concat, sum, lookup,
                        exp, sqrt, abs, log, max, div )
import DyNet.Internal.Expr
import DyNet.Internal.Core
import DyNet.Core


class DyNum a b where
    -- | @x \`add\` y@ where both x and y are either 'IsExpr' or 'Float'
    add :: a -> b -> IO Expression

    -- | @x \`mul\` y@ where both x and y are either 'IsExpr' or 'Float'
    mul :: a -> b -> IO Expression

    -- | @x \`div\` y@ where the case x is 'IsExpr' and y either of 'IsExpr' or 'Float' is valid.
    div :: a -> b -> IO Expression

    -- | @x \`sub\` y@ where both x and y are either 'IsExpr' or 'Float'
    sub :: a -> b -> IO Expression

instance (IsExpr e1, IsExpr e2) => DyNum e1 e2 where
    add = op_add
    mul = op_mul
    div _ _ = error $ "no implementation"
    sub x y = x `op_add` (neg y)

instance IsExpr e => DyNum e Float where
    add = op_scalar_add
    mul = op_scalar_mul
    div = op_scalar_div
    sub x y = neg (y `op_scalar_sub` x)

instance IsExpr e => DyNum Float e where
    add = flip op_scalar_add
    mul = flip op_scalar_mul
    div _ _ = error $ "no implementation"
    sub = op_scalar_sub

