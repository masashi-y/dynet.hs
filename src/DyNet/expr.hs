{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module DyNet.Expr (
    input,
    input'',
    parameter,
    parameter',
    constParameter,
    constParameter',
    lookup,
    -- lookup,
    lookup',
    -- constLookup,
    constLookup',
    zeroes,
    randomNormal,
    randomBernoulli,
    randomUniform,
    randomGumbel,
    nobackprop,
    flipGradient,
    neg,
    add,
    mul,
    div,
    sub,
    cdiv,
    cmult,
    colwiseAdd,
    tanh,
    exp,
    square,
    sqrt,
    abs,
    erf,
    cube,
    log,
    lgamma,
    logistic,
    rectify,
    hinge,
    hinge',
    logSoftmax,
    logSoftmax',
    softmax,
    softsign,
    pow,
    bmin,
    bmax,
    noise,
    dropout,
    dropoutBatch,
    dropoutDim,
    blockDropout,
    reshape,
    transpose,
    affineTransform,
    inverse,
    logdet,
    traceOfProduct,
    dotProduct,
    squaredDistance,
    squaredNorm,
    l2Norm,
    huberDistance,
    l1Distance,
    binaryLogLoss,
    pairwiseRankLoss,
    poissonLoss,
    filter1dNarrow,
    kmaxPooling,
    foldRows,
    sumCols,
    kmhNgram,
    conv2d,
    conv2d',
    maxpooling2d,
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
    selectRows,
    selectCols,
    -- pick,
    pick',
    pickRange,
    pickBatchElems,
    pickBatchElem,
    pickneglogsoftmax,
    pickneglogsoftmax',
    contract3d_1d,
    contract3d_1d_1,
    contract3d_1d_1d,
    contract3d_1d_1d',
    elu,
    selu,
    average,
    concatCols,
    concat,
    concat',
    concatToBatch,
    sum,
    max,
    logsumexp,
    maxDim,
    minDim,
    layerNorm,
    weightNorm,
) where

import Prelude hiding ( tanh, concat, sum, lookup,
                        exp, sqrt, abs, log, max, div )
import DyNet.Internal.Expr
import DyNet.Core


class DyNum a b where
    add :: a -> b -> IO Expression
    mul :: a -> b -> IO Expression
    div :: a -> b -> IO Expression
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

