
module DyNet.Internal.Expr where

{#import DyNet.Internal.Core #}

import Foreign.Storable
import DyNet.Vector

#include "dynet.h"


-- input
{#fun c_input as input
    {+S, `ComputationGraph', float *ps} -> `Expression' #} 

-- input
{#fun c_input_1 as input'
    {+S, `ComputationGraph', withDim* `Dim', withVector `Vector Float'} -> `Expression' #} 

-- input
{#fun c_input_2 as input''
    {+S, `ComputationGraph', withDim* `Dim', withVector `Vector Word', withVector `Vector Float', `Float'} -> `Expression' #} 

-- parameter
{#fun c_parameter as parameter
    {+S, `ComputationGraph', `Parameter'} -> `Expression' #} 

-- parameter
{#fun c_parameter_1 as parameter'
    {+S, `ComputationGraph', `LookupParameter'} -> `Expression' #} 

-- const_parameter
{#fun c_const_parameter as constParameter
    {+S, `ComputationGraph', `Parameter'} -> `Expression' #} 

-- const_parameter
{#fun c_const_parameter_1 as constParameter'
    {+S, `ComputationGraph', `LookupParameter'} -> `Expression' #} 

-- lookup
{#fun c_lookup as lookup
    {+S, `ComputationGraph', `LookupParameter', unsigned* pindex} -> `Expression' #} 

-- lookup
{#fun c_lookup_1 as lookup'
    {+S, `ComputationGraph', `LookupParameter', withVector `Vector Word'} -> `Expression' #} 

-- const_lookup
{#fun c_const_lookup as constLookup
    {+S, `ComputationGraph', `LookupParameter', unsigned* pindex} -> `Expression' #} 

-- const_lookup
{#fun c_const_lookup_1 as constLookup'
    {+S, `ComputationGraph', `LookupParameter', withVector `Vector Word'} -> `Expression' #} 

-- zeroes
{#fun c_zeroes as zeroes
    {+S, `ComputationGraph', withDim* `Dim'} -> `Expression' #} 

-- random_normal
{#fun c_random_normal as randomNormal
    {+S, `ComputationGraph', withDim* `Dim'} -> `Expression' #} 

-- random_bernoulli
{#fun c_random_bernoulli as randomBernoulli
    {+S, `ComputationGraph', withDim* `Dim', `Float', `Float'} -> `Expression' #} 

-- random_uniform
{#fun c_random_uniform as randomUniform
    {+S, `ComputationGraph', withDim* `Dim', `Float', `Float'} -> `Expression' #} 

-- random_gumbel
{#fun c_random_gumbel as randomGumbel
    {+S, `ComputationGraph', withDim* `Dim', `Float', `Float'} -> `Expression' #} 

-- nobackprop
{#fun c_nobackprop as nobackprop
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #} 

-- flip_gradient
{#fun c_flip_gradient as flipGradient
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #} 

-- operator-
{#fun c_op_neg as neg
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #} 

-- operator+S
{#fun c_op_add as op_add
    `(IsExpr ex1, IsExpr ex2)' =>
    {+S, withExpr* `ex1', withExpr* `ex2'} -> `Expression' #} 

add = op_add

-- operator+S
{#fun c_op_scalar_add as op_scalar_add                                   -- TODO
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Float'} -> `Expression' #} 

-- operator*
{#fun c_op_mul as mul
    `(IsExpr ex1, IsExpr ex2)' =>
    {+S, withExpr* `ex1', withExpr* `ex2'} -> `Expression' #} 

-- operator*
{#fun c_op_scalar_mul as op_scalar_mul                                   -- TODO
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Float'} -> `Expression' #} 

-- operator/
{#fun c_op_scalar_div as op_scalar_div                                   -- TODO
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Float'} -> `Expression' #} 

-- operator-
{#fun c_op_scalar_sub as op_scalar_sub                                   -- TODO
    `IsExpr ex' =>
    {+S, `Float', withExpr* `ex'} -> `Expression' #} 

-- cdiv
{#fun c_cdiv as cdiv
    `(IsExpr ex1, IsExpr ex2)' =>
    {+S, withExpr* `ex1', withExpr* `ex2'} -> `Expression' #} 

-- cmult
{#fun c_cmult as cmult
    `(IsExpr ex1, IsExpr ex2)' =>
    {+S, withExpr* `ex1', withExpr* `ex2'} -> `Expression' #} 

-- colwise_add
{#fun c_colwise_add as colwiseAdd
    `(IsExpr ex1, IsExpr ex2)' =>
    {+S, withExpr* `ex1', withExpr* `ex2'} -> `Expression' #} 

-- tanh
{#fun c_tanh as tanh
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #} 

-- exp
{#fun c_exp as exp
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #} 

-- square
{#fun c_square as square
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #} 

-- sqrt
{#fun c_sqrt as sqrt
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #} 

-- abs
{#fun c_abs as abs
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #} 

-- erf
{#fun c_erf as erf
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #} 

-- cube
{#fun c_cube as cube
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #} 

-- log
{#fun c_log as log
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #} 

-- lgamma
{#fun c_lgamma as lgamma
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #} 

-- logistic
{#fun c_logistic as logistic
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #} 

-- rectify
{#fun c_rectify as rectify
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #} 

-- hinge
{#fun c_hinge as hinge
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Int', `Float'} -> `Expression' #} 

-- hinge
{#fun c_hinge_1 as hinge'
    `IsExpr ex' =>
    {+S, withExpr* `ex', withVector `Vector Word', `Float'} -> `Expression' #} 

-- log_softmax
{#fun c_log_softmax as logSoftmax
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #} 

-- log_softmax
{#fun c_log_softmax_1 as logSoftmax'
    `IsExpr ex' =>
    {+S, withExpr* `ex', withVector `Vector Word'} -> `Expression' #} 

-- softmax
{#fun c_softmax as softmax
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #} 

-- softsign
{#fun c_softsign as softsign
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #} 

-- pow
{#fun c_pow as pow
    `(IsExpr ex1, IsExpr ex2)' =>
    {+S, withExpr* `ex1', withExpr* `ex2'} -> `Expression' #} 

-- min
{#fun c_bmin as bmin
    `(IsExpr ex1, IsExpr ex2)' =>
    {+S, withExpr* `ex1', withExpr* `ex2'} -> `Expression' #} 

-- max
{#fun c_bmax as bmax
    `(IsExpr ex1, IsExpr ex2)' =>
    {+S, withExpr* `ex1', withExpr* `ex2'} -> `Expression' #} 

-- noise
{#fun c_noise as noise
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Float'} -> `Expression' #} 

-- dropout
{#fun c_dropout as dropout
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Float'} -> `Expression' #} 

-- dropout_batch
{#fun c_dropout_batch as dropoutBatch
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Float'} -> `Expression' #} 

-- dropout_dim
{#fun c_dropout_dim as dropoutDim
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Int', `Float'} -> `Expression' #} 

-- block_dropout
{#fun c_block_dropout as blockDropout
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Float'} -> `Expression' #} 

-- reshape
{#fun c_reshape as reshape
    `IsExpr ex' =>
    {+S, withExpr* `ex', withDim* `Dim'} -> `Expression' #} 

-- transpose
{#fun c_transpose as transpose
    `IsExpr ex' =>
    {+S, withExpr* `ex', withVector `Vector Word'} -> `Expression' #} 

-- affine_transform
{#fun c_affine_transform as affineTransform
    {+S,  withExpList* `[Expression]'} -> `Expression' #} 

-- inverse
{#fun c_inverse as inverse
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #} 

-- logdet
{#fun c_logdet as logdet
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #} 

-- trace_of_product
{#fun c_trace_of_product as traceOfProduct
    `(IsExpr ex1, IsExpr ex2)' =>
    {+S, withExpr* `ex1', withExpr* `ex2'} -> `Expression' #} 

-- dot_product
{#fun c_dot_product as dotProduct
    `(IsExpr ex1, IsExpr ex2)' =>
    {+S, withExpr* `ex1', withExpr* `ex2'} -> `Expression' #} 

-- squared_distance
{#fun c_squared_distance as squaredDistance
    `(IsExpr ex1, IsExpr ex2)' =>
    {+S, withExpr* `ex1', withExpr* `ex2'} -> `Expression' #} 

-- squared_norm
{#fun c_squared_norm as squaredNorm
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #} 

-- l2_norm
{#fun c_l2_norm as l2Norm
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #} 

-- huber_distance
{#fun c_huber_distance as huberDistance
    `(IsExpr ex1, IsExpr ex2)' =>
    {+S, withExpr* `ex1', withExpr* `ex2', `Float'} -> `Expression' #} 

-- l1_distance
{#fun c_l1_distance as l1Distance
    `(IsExpr ex1, IsExpr ex2)' =>
    {+S, withExpr* `ex1', withExpr* `ex2'} -> `Expression' #} 

-- binary_log_loss
{#fun c_binary_log_loss as binaryLogLoss
    `(IsExpr ex1, IsExpr ex2)' =>
    {+S, withExpr* `ex1', withExpr* `ex2'} -> `Expression' #} 

-- pairwise_rank_loss
{#fun c_pairwise_rank_loss as pairwiseRankLoss
    `(IsExpr ex1, IsExpr ex2)' =>
    {+S, withExpr* `ex1', withExpr* `ex2', `Float'} -> `Expression' #} 

-- poisson_loss
{#fun c_poisson_loss as poissonLoss
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Int'} -> `Expression' #} 

-- filter1d_narrow
{#fun c_filter1d_narrow as filter1dNarrow
    `(IsExpr ex1, IsExpr ex2)' =>
    {+S, withExpr* `ex1', withExpr* `ex2'} -> `Expression' #} 

-- kmax_pooling
{#fun c_kmax_pooling as kmaxPooling
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Int', `Int'} -> `Expression' #} 

-- fold_rows
{#fun c_fold_rows as foldRows
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Int'} -> `Expression' #} 

-- sum_cols
{#fun c_sum_cols as sumCols
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #} 

-- kmh_ngram
{#fun c_kmh_ngram as kmhNgram
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Int'} -> `Expression' #} 

-- conv2d
{#fun c_conv2d as conv2d
    `(IsExpr ex1, IsExpr ex2)' =>
    {+S, withExpr* `ex1', withExpr* `ex2', withVector `Vector Word', `Bool'} -> `Expression' #} 

-- conv2d
{#fun c_conv2d_1 as conv2d'
    `(IsExpr ex1, IsExpr ex2, IsExpr ex3)' =>
    {+S, withExpr* `ex1', withExpr* `ex2', withExpr* `ex3', withVector `Vector Word', `Bool'} -> `Expression' #} 

-- maxpooling2d
{#fun c_maxpooling2d as maxpooling2d
    {+S, `Expression', withVector `Vector Word', withVector `Vector Word', `Bool'} -> `Expression' #} 

-- sum_batches
{#fun c_sum_batches as sumBatches
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #} 

-- sum_elems
{#fun c_sum_elems as sumElems
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #} 

-- moment_batches
{#fun c_moment_batches as momentBatches
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Int'} -> `Expression' #} 

-- moment_elems
{#fun c_moment_elems as momentElems
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Int'} -> `Expression' #} 

-- moment_dim
{#fun c_moment_dim as momentDim
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Int', `Int'} -> `Expression' #} 

-- mean_elems
{#fun c_mean_elems as meanElems
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #} 

-- mean_batches
{#fun c_mean_batches as meanBatches
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #} 

-- mean_dim
{#fun c_mean_dim as meanDim
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Int'} -> `Expression' #} 

-- std_dim
{#fun c_std_dim as stdDim
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Int'} -> `Expression' #} 

-- std_elems
{#fun c_std_elems as stdElems
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #} 

-- std_batches
{#fun c_std_batches as stdBatches
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #} 

-- select_rows
{#fun c_select_rows as selectRows
    `IsExpr ex' =>
    {+S, withExpr* `ex', withVector `Vector Word'} -> `Expression' #} 

-- select_cols
{#fun c_select_cols as selectCols
    `IsExpr ex' =>
    {+S, withExpr* `ex', withVector `Vector Word'} -> `Expression' #} 

-- pick
{#fun c_pick as pick
    `IsExpr ex' =>
    {+S, withExpr* `ex', unsigned* pv, `Int'} -> `Expression' #} 

-- pick
{#fun c_pick_1 as pick'
    `IsExpr ex' =>
    {+S, withExpr* `ex', withVector `Vector Word', `Int'} -> `Expression' #} 

-- pick_range
{#fun c_pick_range as pickRange
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Int', `Int', `Int'} -> `Expression' #} 

-- pick_batch_elems
{#fun c_pick_batch_elems as pickBatchElems
    `IsExpr ex' =>
    {+S, withExpr* `ex', withVector `Vector Word'} -> `Expression' #} 

-- pick_batch_elem
{#fun c_pick_batch_elem as pickBatchElem
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Int'} -> `Expression' #} 

-- pickneglogsoftmax
{#fun c_pickneglogsoftmax as pickneglogsoftmax
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Int'} -> `Expression' #} 

-- pickneglogsoftmax
{#fun c_pickneglogsoftmax_1 as pickneglogsoftmax'
    `IsExpr ex' =>
    {+S, withExpr* `ex', withVector `Vector Word'} -> `Expression' #} 

-- contract3d_1d
{#fun c_contract3d_1d as contract3d_1d
    `(IsExpr ex1, IsExpr ex2)' =>
    {+S, withExpr* `ex1', withExpr* `ex2'} -> `Expression' #} 

-- contract3d_1d
{#fun c_contract3d_1d_1 as contract3d_1d_1
    `(IsExpr ex1, IsExpr ex2, IsExpr ex3)' =>
    {+S, withExpr* `ex1', withExpr* `ex2', withExpr* `ex3'} -> `Expression' #} 

-- contract3d_1d_1d
{#fun c_contract3d_1d_1d as contract3d_1d_1d
    `(IsExpr ex1, IsExpr ex2, IsExpr ex3)' =>
    {+S, withExpr* `ex1', withExpr* `ex2', withExpr* `ex3'} -> `Expression' #} 

-- contract3d_1d_1d
{#fun c_contract3d_1d_1d_1 as contract3d_1d_1d'
    `(IsExpr ex1, IsExpr ex2, IsExpr ex3, IsExpr ex4)' =>
    {+S, withExpr* `ex1', withExpr* `ex2', withExpr* `ex3', withExpr* `ex4'} -> `Expression' #} 

-- elu
{#fun c_elu as elu
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Float'} -> `Expression' #} 

-- selu
{#fun c_selu as selu
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #} 

-- average
{#fun c_average as average
    {+S,  withExpList* `[Expression]'} -> `Expression' #} 

-- concatenate_cols
{#fun c_concat_cols as concatCols
    {+S,  withExpList* `[Expression]'} -> `Expression' #} 

-- concatenate
{#fun c_concat as concat
    {+S,  withExpList* `[Expression]', `Int'} -> `Expression' #} 

-- concatenate_to_batch
{#fun c_concat_to_batch as concatToBatch
    {+S,  withExpList* `[Expression]'} -> `Expression' #} 

-- sum
{#fun c_sum as sum
    {+S,  withExpList* `[Expression]'} -> `Expression' #} 

-- max
{#fun c_max as max
    {+S,  withExpList* `[Expression]'} -> `Expression' #} 

-- logsumexp
{#fun c_logsumexp as logsumexp
    {+S,  withExpList* `[Expression]'} -> `Expression' #} 

-- max_dim
{#fun c_max_dim as maxDim
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Int'} -> `Expression' #} 

-- min_dim
{#fun c_min_dim as minDim
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Int'} -> `Expression' #} 

-- layer_norm
{#fun c_layer_norm as layerNorm
    `(IsExpr ex1, IsExpr ex2, IsExpr ex3)' =>
    {+S, withExpr* `ex1', withExpr* `ex2', withExpr* `ex3'} -> `Expression' #} 

-- weight_norm
{#fun c_weight_norm as weightNorm
    `(IsExpr ex1, IsExpr ex2)' =>
    {+S, withExpr* `ex1', withExpr* `ex2'} -> `Expression' #} 

