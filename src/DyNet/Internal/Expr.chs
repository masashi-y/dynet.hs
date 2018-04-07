{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module DyNet.Internal.Expr where

{#import DyNet.Internal.Core #}
{#import DyNet.Internal.Vector #}
{#import DyNet.Internal.ExpVector #}

import qualified Foreign.Ptr as C2HSImp
import Foreign.Storable
import Prelude hiding ( concat )
import Data.Int ( Int64(..) )
---import DyNet.Vector

#include "dynet.h"


-- This class enables "expressions" involved in computation graph
-- construction be either Expression or (IO Expression) type.

-- #################################################################
-- ############################ Operations #########################
-- #################################################################

-- input
-- {#fun c_input as input
--     {+S, `ComputationGraph', float *ps} -> `Expression' #}

-- input
{-|
[@brief@] Vector \/ matrix \/ tensor input
[@details@] Create an expression that represents a vector, matrix, or tensor
         input. The dimensions of the input are defined by @d@. So for example
         @input(g,{50},data)@ will result in a 50-length vector
         @input(g,{50,30},data)@ will result in a 50x30 matrix
         and so on, for an arbitrary number of dimensions.
         This function can also be used to import minibatched inputs. For example,
         if we have 10 examples in a minibatch, each with size 50x30, then we call
         @input(g,Dim({50,30},10),data)@
         The data vector "data" will contain the values used to fill the input, in
         column-major format. The length must add to the product of all dimensions in
         d.

[@parameters@]

    * @g@: Computation graph
    * @d@: Dimension of the input matrix
    * @data@: A vector of data points

[@return@] An expression representing data
-}
{#fun c_input_1 as input
    `(Sequence Float s, Dimension d)' =>
    {+S, `ComputationGraph', withDimension* `d', withSequence* `s'} -> `Expression' #}

-- input
{-|
[@brief@] Sparse vector input
[@details@] This operation takes input as a sparse matrix of index / value pairs. It is
            exactly the same as the standard input via vector reference, but sets all
            non-specified values to "defdata" and resets all others to the appropriate
            input values.

[@parameters@]

    * @g@: Computation graph
    * @d@: Dimension of the input matrix
    * @ids@: The indexes of the data points to update
    * @data@: The data points corresponding to each index
    * @defdata@: The default data with which to set the unspecified data points
    * @device@: The place device for the input value, default_device by default

[@return@] An expression representing data
-}
{#fun c_input_2 as input''
    `(Dimension d, Sequence Word s1, Sequence Float s2)' =>
    {+S,
     `ComputationGraph',
      withDimension* `d', withSequence* `s1', withSequence* `s2', `Float'} -> `Expression' #}

-- parameter
{-|
[@brief@] Load parameter
[@details@] Load parameters into the computation graph.

[@parameters@]

    * @g@: Computation graph
    * @p@: Parameter object to load

[@return@] An expression representing p
-}
{#fun c_parameter as parameter
    {+S, `ComputationGraph', `Parameter'} -> `Expression' #}

-- parameter
{-|
[@brief@] Load lookup parameter
[@details@] Load a full tensor of lookup parameters into the computation graph.
            Normally lookup parameters are accessed by using the lookup() function
            to grab a single element. However, in some cases we'll want to access
            all of the parameters in the entire set of lookup parameters for some
            reason. In this case you can use this function. In this case, the
            first dimensions in the returned tensor will be equivalent to the
            dimensions that we would get if we get calling the lookup() function,
            and the size of the final dimension will be equal to the size of the
            vocabulary.

[@parameters@]

    * @g@: Computation graph
    * @lp@: LookupParameter object to load

[@return@] An expression representing lp
-}
{#fun c_parameter_1 as lookupParameter
    {+S, `ComputationGraph', `LookupParameter'} -> `Expression' #}

-- const_parameter
{-|
[@brief@] Load constant parameters
[@details@] Load parameters into the computation graph, but prevent them from being
            updated when performing parameter update.

[@parameters@]

    * @g@: Computation graph
    * @p@: Parameter object to load

[@return@] An expression representing the constant p
-}
{#fun c_const_parameter as constParameter
    {+S, `ComputationGraph', `Parameter'} -> `Expression' #}

-- const_parameter
{-|
[@brief@] Load constant lookup parameters
[@details@] Load lookup parameters into the computation graph, but prevent them from being
            updated when performing parameter update.

[@parameters@]

    * @g@: Computation graph
    * @lp@: LookupParameter object to load

[@return@] An expression representing the constant lp
-}
{#fun c_const_parameter_1 as constLookupParameter
    {+S, `ComputationGraph', `LookupParameter'} -> `Expression' #}

-- lookup
{-|
[@brief@] Look up parameter
[@details@] Look up parameters according to an index, and load them into the
            computation graph. Do not perform gradient update on the parameters.

[@parameters@]

    * @g@: Computation graph
    * @p@: LookupParameter object from which to load
    * @index@: Index of the parameters within p

[@return@] A constant expression representing p[index]
-}
{#fun c_lookup_0 as lookup
    {+S, `ComputationGraph', `LookupParameter', `Int'} -> `Expression' #}

-- lookup
-- {#fun c_lookup as lookup
--     {+S, `ComputationGraph', `LookupParameter', unsigned* pindex} -> `Expression' #}

-- lookup
{-|
[@brief@] Look up parameters
[@details@] The mini-batched version of lookup. The resulting expression will be
            a mini-batch of parameters, where the "i"th element of the batch corresponds
            to the parameters at the position specified by the "i"th element of
            "indices"

[@parameters@]

    * @g@: Computation graph
    * @p@: LookupParameter object from which to load
    * @indices@: Index of the parameters at each position in the batch

[@return@] An expression with the "i"th batch element representing p[indices[i]]
-}
{#fun c_lookup_1 as lookup'
    `Sequence Word s' =>
    {+S, `ComputationGraph', `LookupParameter', withSequence* `s'} -> `Expression' #}

-- const_lookup
-- {#fun c_const_lookup as constLookup
--     {+S, `ComputationGraph', `LookupParameter', unsigned* pindex} -> `Expression' #}

-- const_lookup
{-|
[@brief@] Look up parameters
[@details@] Mini-batched lookup that will not update the parameters.

[@parameters@]

    * @g@: Computation graph
    * @p@: LookupParameter object from which to load
    * @indices@: Lookup indices

[@return@] A constant expression with the "i"th batch element representing p[indices[i]]
-}
{#fun c_const_lookup_1 as constLookup'
    `Sequence Word s' =>
    {+S, `ComputationGraph', `LookupParameter', withSequence* `s'} -> `Expression' #}

-- zeroes
{-|
[@brief@] Create an input full of zeros
[@details@] Create an input full of zeros, sized according to dimensions d.

[@parameters@]

    * @g@: Computation graph
    * @d@: The dimensions of the input

[@return@] A `d` dimensioned zero tensor
-}
{#fun c_zeroes as zeroes
    `Dimension d' =>
    {+S, `ComputationGraph', withDimension* `d'} -> `Expression' #}

-- random_normal
{-|
[@brief@] Create a random normal vector
[@details@] Create a vector distributed according to normal distribution with specified mean and standard deviation.

[@parameters@]

    * @g@: Computation graph
    * @d@: The dimensions of the input
    * @mean@: The mean of the distribution (default: 0.0)
    * @stddev@: The standard deviation of the distribution (default: 1.0)

[@return@] A "d" dimensioned normally distributed vector
-}
{#fun c_random_normal as randomNormal
    `Dimension d' =>
    {+S, `ComputationGraph', withDimension* `d', `Float', `Float'} -> `Expression' #}

-- random_bernoulli
{-|
[@brief@] Create a random bernoulli vector
[@details@] Create a vector distributed according to bernoulli distribution with parameter p.

[@parameters@]

    * @g@: Computation graph
    * @d@: The dimensions of the input
    * @p@: The bernoulli p parameter
    * @scale@: A scaling factor for the output ("active" elements will receive this value)

[@return@] A "d" dimensioned bernoulli distributed vector
-}
{#fun c_random_bernoulli as randomBernoulli
    `Dimension d' =>
    {+S, `ComputationGraph', withDimension* `d', `Float', `Float'} -> `Expression' #}

-- random_uniform
{-|
[@brief@] Create a random uniform vector
[@details@] Create a vector distributed according to uniform distribution with boundaries left and right.

[@parameters@]

    * @g@: Computation graph
    * @d@: The dimensions of the input
    * @left@: The left boundary
    * @right@: The right boundary

[@return@] A "d" dimensioned uniform distributed vector
-}
{#fun c_random_uniform as randomUniform
    `Dimension d' =>
    {+S, `ComputationGraph', withDimension* `d', `Float', `Float'} -> `Expression' #}

-- random_gumbel
{-|
[@brief@] Create a random Gumbel sampled vector
[@details@] Create a vector distributed according to a Gumbel distribution with the specified parameters. (Currently only the defaults of mu=0.0 and beta=1.0 supported.

[@parameters@]

    * @g@: Computation graph
    * @d@: The dimensions of the input
    * @mu@: The mu parameter
    * @beta@: The beta parameter

[@return@] A "d" dimensioned Gumbel distributed vector
-}
{#fun c_random_gumbel as randomGumbel
    `Dimension d' =>
    {+S, `ComputationGraph', withDimension* `d', `Float', `Float'} -> `Expression' #}

-- nobackprop
{-|
[@brief@] Prevent backprop
[@details@] This node has no effect on the forward pass, but prevents gradients from
            flowing backward during the backward pass. This is useful when there's
            a subgraph for which you don't want loss passed back to the parameters.

[@parameters@]

    * @x@: The input expression

[@return@] The new expression
-}
{#fun c_nobackprop as nobackprop
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #}

-- flip_gradient
{-|
[@brief@] Flip gradient
[@details@] This node has no effect on the forward pass, but inverts the gradient on backprop.
            This operation is widely used in adversarial networks.

[@parameters@]

    * @x@: The input expression

[@return@] An output expression containing the same as input (only effects the backprop process)
-}
{#fun c_flip_gradient as flipGradient
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #}

-- operator-
{-|
[@brief@] Negation
[@details@] Negate the passed argument.

[@parameters@]

    * @x@: An input expression

[@return@] The negation of x
-}
{#fun c_op_neg as neg
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #}

-- operator+S
{-|
[@brief@] Expression addition
[@details@] Add two expressions of the same dimensions.

[@parameters@]

    * @x@: The first input
    * @y@: The second input

[@return@] The sum of x and y
-}
{#fun c_op_add as op_add
    `(IsExpr ex1, IsExpr ex2)' =>
    {+S, withExpr* `ex1', withExpr* `ex2'} -> `Expression' #}

-- operator+S
{-|
[@brief@] Scalar addition
[@details@] Add a scalar to an expression

[@parameters@]

    * @x@: The expression
    * @y@: The scalar

[@return@] An expression equal to x, with every component increased by y
-}
{#fun c_op_scalar_add as op_scalar_add
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Float'} -> `Expression' #}

-- operator*
{-|
[@brief@] Matrix multiplication
[@details@] Multiply two matrices together. Like standard matrix multiplication, the
         second dimension of x and the first dimension of y must match.

[@parameters@]

    * @x@: The left-hand matrix
    * @y@: The right-hand matrix

[@return@] An expression x times y
-}
{#fun c_op_mul as op_mul
    `(IsExpr ex1, IsExpr ex2)' =>
    {+S, withExpr* `ex1', withExpr* `ex2'} -> `Expression' #}

-- operator*
{-|
[@brief@] Matrix-scalar multiplication
[@details@] Multiply an expression component-wise by a scalar.

[@parameters@]

    * @x@: The matrix
    * @y@: The scalar

[@return@] An expression where the ith element is x_i times y
-}
{#fun c_op_scalar_mul as op_scalar_mul
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Float'} -> `Expression' #}

-- operator/
{-|
[@brief@] Matrix-scalar division
[@details@] Divide an expression component-wise by a scalar.

[@parameters@]

    * @x@: The matrix
    * @y@: The scalar

[@return@] An expression where the ith element is x_i divided by y
-}
{#fun c_op_scalar_div as op_scalar_div
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Float'} -> `Expression' #}

-- operator-
{-|
[@brief@] Scalar subtraction
[@details@] Subtract a scalar from an expression

[@parameters@]

    * @x@: The expression from which to subtract
    * @y@: The scalar to subtract

[@return@] An expression where the ith element is x_i minus y
-}
{#fun c_op_scalar_sub as op_scalar_sub
    `IsExpr ex' =>
    {+S, `Float', withExpr* `ex'} -> `Expression' #}

-- cdiv
{-|
[@brief@] Componentwise division
[@details@] Divide an expressions component-wise by another, broadcasting dimensions (currently only of the second expression!) if necessary as follows:

    * When number of dimensions differ, we add dimensions of size 1 to make the number of dimensions match
    * Now, every dimensions is required to have matching size, or the dim size of the right expression must equal 1 (in which case it will be broadcasted)
    * In the same way, the batch sizes must match, or the batch size of the right expression must equal 1 in which case it will be broadcasted
    * The resulting tensor's dimensionality is thus determined as the max of both inputs at every position

[@parameters@]

    * @x@: The first input expression
    * @y@: The second input expression

[@return@] An expression where the ith element is equal to \(x_i / y_i\)
-}
{#fun c_cdiv as cdiv
    `(IsExpr ex1, IsExpr ex2)' =>
    {+S, withExpr* `ex1', withExpr* `ex2'} -> `Expression' #}

-- cmult
{-|
[@brief@] Componentwise multiply
[@details@] Multiply two expressions component-wise, broadcasting dimensions if necessary as follows:

    * When number of dimensions differ, we add dimensions of size 1 to make the number of dimensions match
    * Now, every dimensions is required to have matching size, or one of the dimensions must equal 1 (in which case it will be broadcasted)
    * In the same way, the batch dimension must match, or equal 1 in which case it will be broadcasted
    * The resulting tensor's dimensionality is thus determined as the max of both inputs at every position

[@parameters@]

    * @x@: The first input expression
    * @y@: The second input expression

[@return@] An expression where the ith element is equal to \(x_i*y_i\)
-}
{#fun c_cmult as cmult
    `(IsExpr ex1, IsExpr ex2)' =>
    {+S, withExpr* `ex1', withExpr* `ex2'} -> `Expression' #}

-- colwise_add
{-|
[@brief@] Columnwise addition
[@details@] Add vector "bias" to each column of matrix "x"

[@parameters@]

    * @x@: An MxN matrix
    * @bias@: A length M vector

[@return@] An expression where bias is added to each column of x
-}
{#fun c_colwise_add as colwiseAdd
    `(IsExpr ex1, IsExpr ex2)' =>
    {+S, withExpr* `ex1', withExpr* `ex2'} -> `Expression' #}

-- tanh
{-|
[@brief@] Hyperbolic tangent
[@details@] Elementwise calculation of the hyperbolic tangent

[@parameters@]

    * @x@: The input expression

[@return@] An expression where the ith element is equal to \(tanh(x_i)\)
-}
{#fun c_tanh as tanh
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #}

-- exp
{-|
[@brief@] Natural exponent
[@details@] Calculate elementwise \(y_i = e^{x_i}\)

[@parameters@]

    * @x@: The input expression

[@return@] An expression where the ith element is equal to \(e^{x_i}\)
-}
{#fun c_exp as exp
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #}

-- square
{-|
[@brief@] Square
[@details@] Calculate elementwise \(y_i = x_i^2\)

[@parameters@]

    * @x@: The input expression

[@return@] An expression where the ith element is equal to \(x_i^2\)
-}
{#fun c_square as square
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #}

-- sqrt
{-|
[@brief@] Square root
[@details@] Elementwise square root.

[@parameters@]

    * @x@: The input expression

[@return@] An expression where the ith element is equal to \(\sqrt(x_i)\)
-}
{#fun c_sqrt as sqrt
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #}

-- abs
{-|
[@brief@] Absolute value
[@details@] Elementwise absolute value.

[@parameters@]

    * @x@: The input expression

[@return@] An expression where the ith element is equal to \(\vert x_i\vert\)
-}
{#fun c_abs as abs
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #}

-- erf
{-|
[@brief@] Gaussian error function
[@details@] Elementwise calculation of the Gaussian error function

[@parameters@]

    * @x@: The input expression

[@return@] An expression where the ith element is equal to \(erf(x_i)\)
-}
{#fun c_erf as erf
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #}

-- cube
{-|
[@brief@] Cube
[@details@] Calculate elementwise \(y_i = x_i^3\)

[@parameters@]

    * @x@: The input expression

[@return@] An expression where the ith element is equal to \(x_i^3\)
-}
{#fun c_cube as cube
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #}

-- log
{-|
[@brief@] Logarithm
[@details@] Calculate the elementwise natural logarithm \(y_i = ln(x_i)\)

[@parameters@]

    * @x@: The input expression

[@return@] An expression where the ith element is equal to \(ln(x_i)\)
-}
{#fun c_log as log
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #}

-- lgamma
{-|
[@brief@] Log gamma
[@details@] Calculate elementwise \(y_i = ln(gamma(x_i))\)

[@parameters@]

    * @x@: The input expression

[@return@] An expression where the ith element is equal to \(ln(gamma(x_i))\)
-}
{#fun c_lgamma as lgamma
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #}

{-| TODO
[@brief@] Logistic sigmoid function
[@details@] Calculate elementwise \( y_i = 1/(1+e^{(-x_i)}) \)

[@parameters@]

    * @x@: The input expression

[@return@] An expression where the ith element is equal to \( y_i = 1/(1+e^{(-x_i)}) \)
-}
{#fun c_logistic as logistic
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #}

-- rectify
{-|
[@brief@] Rectifier
[@details@] Calculate elementwise the recitifer (ReLU) function \(y_i = max(x_i,0)\)

[@parameters@]

    * @x@: The input expression

[@return@] An expression where the ith element is equal to \(max(x_i,0)\)
-}
{#fun c_rectify as rectify
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #}

-- hinge
{-|
[@brief@] Hinge loss
[@details@] This expression calculates the hinge loss, formally expressed as:
          \( \text{hinge}(x,index,m) = \sum_{i \ne index} \max(0, m-x[index]+x[i]). \)

[@parameters@]

    * @x@: A vector of scores
    * @index@: The index of the correct candidate
    * @m@: The margin

[@return@] The hinge loss of candidate @index@ with respect to margin @m@
-}
{#fun c_hinge as hinge
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Int', `Float'} -> `Expression' #}

-- hinge
{-|
[@brief@] Batched hinge loss
[@details@] The same as hinge loss, but for the case where @x@ is a mini-batched tensor
            with @indices.size()@ batch elements, and @indices@ is a vector indicating
            the index of each of the correct elements for these elements.

[@parameters@]

    * @x@: A mini-batch of vectors with @indices.size()@ batch elements
    * @indices@: The indices of the correct candidates for each batch element
    * @m@: The margin

[@return@] The hinge loss of each mini-batch
-}
{#fun c_hinge_1 as hinge'
    `(IsExpr ex, Sequence Word s)' =>
    {+S, withExpr* `ex', withSequence* `s', `Float'} -> `Expression' #}

-- log_softmax
{-|
[@brief@] Log softmax
[@details@] The log softmax function normalizes each column to ensure that all
            values are between 0 and 1 and add to one by applying the
            \(e^{x[i]}/{\sum_j e^{x[j]}}\), then takes the log

[@parameters@]

    * @x@: A vector or matrix

[@return@] A vector or matrix after calculating the log softmax
-}
{#fun c_log_softmax as logSoftmax
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #}

-- log_softmax
{-|
[@brief@] Restricted log softmax
[@details@] The log softmax function calculated over only a subset of the vector elements. The
            elements to be included are set by the @restriction@ variable. All elements not
            included in @restriction@ are set to negative infinity.

[@parameters@]

    * @x@: A vector over which to calculate the softmax
    * @restriction@: The elements over which to calculate the softmax

[@return@] A vector with the log softmax over the specified elements
-}
{#fun c_log_softmax_1 as logSoftmax'
    `(IsExpr ex, Sequence Word s)' =>
    {+S, withExpr* `ex', withSequence* `s'} -> `Expression' #}

-- softmax
{-|
[@brief@] Softmax
[@details@] The softmax function normalizes each column to ensure that all
            values are between 0 and 1 and add to one by applying the
            \(e^{x[i]}/{\sum_j e^{x[j]}}\).

[@parameters@]

    * @x@: A vector or matrix
    * @d@: dimension to normalize over (default: 0)

[@return@] A vector or matrix after calculating the softmax
-}
{#fun c_softmax as softmax
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #}

-- softsign
{-|
[@brief@] Soft Sign
[@details@] Calculate elementwise the softsign function \(y_i = x_i/(1+|x_i|)\)

[@parameters@]

    * @x@: The input expression

[@return@] An expression where the ith element is equal to \(x_i/(1+|x_i|)\)
-}
{#fun c_softsign as softsign
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #}

-- pow
{-|
[@brief@] Power function
[@details@] Calculate an output where the ith element is equal to \( x_i^{y_i} \)

[@parameters@]

    * @x@: The input expression
    * @y@: The exponent expression

[@return@] An expression where the ith element is equal to \( x_i^{y_i} \)
-}
{#fun c_pow as pow
    `(IsExpr ex1, IsExpr ex2)' =>
    {+S, withExpr* `ex1', withExpr* `ex2'} -> `Expression' #}

-- min
{-|
[@brief@] Minimum
[@details@] Calculate an output where the ith element is \(min(x_i,y_i)\)

[@parameters@]

    * @x@: The first input expression
    * @y@: The second input expression

[@return@] An expression where the ith element is equal to \(min(x_i,y_i)\)
-}
{#fun c_bmin as bmin
    `(IsExpr ex1, IsExpr ex2)' =>
    {+S, withExpr* `ex1', withExpr* `ex2'} -> `Expression' #}

-- max
{-|
[@brief@] Maximum
[@details@] Calculate an output where the ith element is \(max(x_i,y_i)\)

[@parameters@]

    * @x@: The first input expression
    * @y@: The second input expression

[@return@] An expression where the ith element is equal to \(max(x_i,y_i)\)
-}
{#fun c_bmax as bmax
    `(IsExpr ex1, IsExpr ex2)' =>
    {+S, withExpr* `ex1', withExpr* `ex2'} -> `Expression' #}

-- noise
{-|
[@brief@] Gaussian noise
[@details@] Add gaussian noise to an expression.

[@parameters@]

    * @x@: The input expression
    * @stddev@: The standard deviation of the gaussian

[@return@] The noised expression
-}
{#fun c_noise as noise
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Float'} -> `Expression' #}

-- dropout
{-|
[@brief@] Dropout
[@details@]
     With a fixed probability, drop out (set to zero) nodes in the input
     expression, and **scale** the remaining nodes by 1 / p. Note that there are
     [two kinds of dropout](http://cs231n.github.io/neural-networks-2/#reg):

     * *Regular dropout:* where we perform dropout at training time and then\n
       scale outputs by p at test time.
     * *Inverted dropout:* where we perform dropout and scaling at training\n
       time, and do not need to do anything at test time.
     DyNet implements the latter, so you only need to apply dropout at training
     time, and do not need to perform scaling and test time.

[@parameters@]

    * @x@: The input expression
    * @p@: The dropout probability

[@return@] The dropped out expression
-}
{#fun c_dropout as dropout
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Float'} -> `Expression' #}

-- dropout_batch
{-|
[@brief@] Dropout entire elements of a minibatch
[@details@] Identical to the dropout operation except entire batch elements are dropped

[@parameters@]

    * @x@: The input expression
    * @p@: The dropout probability

[@return@] The dropped out expression
-}
{#fun c_dropout_batch as dropoutBatch
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Float'} -> `Expression' #}

-- dropout_dim
{-|
[@brief@] Dropout along a specific dimension
[@details@] Identical to the dropout operation except the dropout mask is the same across one dimension. Use this if you want to drop columns or lines in a matrix for example

   For now this only supports tensors of order <= 3 (with or without batch dimension)

[@parameters@]

    * @x@: The input expression
    * @d@: The dimension along which to drop
    * @p@: The dropout probability

[@return@] The dropped out expression
-}
{#fun c_dropout_dim as dropoutDim
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Int', `Float'} -> `Expression' #}

-- block_dropout
{-|
[@brief@] Block dropout
[@details@] Identical to the dropout operation, but either drops out *all*
            or *no* values in the expression, as opposed to making a decision
            about each value individually.

[@parameters@]

    * @x@: The input expression
    * @p@: The block dropout probability

[@return@] The block dropout expression
-}
{#fun c_block_dropout as blockDropout
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Float'} -> `Expression' #}

-- reshape
{-|
[@brief@] Reshape to another size
[@details@] This node reshapes a tensor to another size, without changing the
            underlying layout of the data. The layout of the data in DyNet is
            column-major, so if we have a 3x4 matrix

      \[
        \begin{pmatrix}
          x_{1,1} & x_{1,2} & x_{1,3} & x_{1,4} \\
          x_{2,1} & x_{2,2} & x_{2,3} & x_{2,4} \\
          x_{3,1} & x_{3,2} & x_{3,3} & x_{3,4} \\
        \end{pmatrix}
      \]

            and transform it into a 2x6 matrix, it will be rearranged as:

      \[
        \begin{pmatrix}
          x_{1,1} & x_{3,1} & x_{2,2} & x_{1,3} & x_{3,3} & x_{2,4} \\
          x_{2,1} & x_{1,2} & x_{3,2} & x_{2,3} & x_{1,4} & x_{3,4} \\
        \end{pmatrix}
      \]

           **Note:** This is O(1) for forward, and O(n) for backward.

[@parameters@]

    * @x@: The input expression
    * @d@: The new dimensions

[@return@] The reshaped expression
-}
{#fun c_reshape as reshape
    `(IsExpr ex, Dimension d)' =>
    {+S, withExpr* `ex', withDimension* `d'} -> `Expression' #}

-- transpose
{-|
[@brief@] Transpose a matrix
[@details@] Transpose a matrix or tensor, or if dims is specified shuffle the
            dimensions arbitrarily.
            **Note:** This is O(1) if either the row or column dimension is 1,
            and O(n) otherwise.

[@parameters@]

    * @x@: The input expression
    * @dims@: The dimensions to swap. The ith dimension of the output will be equal
            to the dims[i] dimension of the input. dims must have the same number
            of dimensions as x.

[@return@] The transposed / shuffled expression
-}
{#fun c_transpose as transpose
    `(IsExpr ex, Sequence Word s)' =>
    {+S, withExpr* `ex', withSequence* `s'} -> `Expression' #}

-- affine_transform
{-|
[@brief@] Affine transform
[@details@] This performs an affine transform over an arbitrary (odd) number of expressions
            held in the input initializer list xs.
            The first expression is the "bias," which is added to the expression as-is.
            The remaining expressions are multiplied together in pairs, then added.
            A very common usage case is the calculation of the score for a neural network
            layer (e.g. b + Wz) where b is the bias, W is the weight matrix, and z is the
            input. In this case xs[0] = b, xs[1] = W, and xs[2] = z.

[@parameters@]

    * @xs@: An initializer list containing an odd number of expressions

[@return@] An expression equal to: xs[0] + xs[1]*xs[2] + xs[3]*xs[4] + ...
-}
{#fun c_affine_transform as affineTransform
    `Sequence Expression s' =>
    {+S,  withSequence* `s'} -> `Expression' #}

-- inverse
{-|
[@brief@] Matrix Inverse
[@details@] Takes the inverse of a matrix (not implemented on GPU yet, although
            contributions are welcome: https://github.com/clab/dynet/issues/158).
            Note that back-propagating through an inverted matrix can also be the
            source of stability problems sometimes.

[@parameters@]

    * @x@: A square matrix

[@return@] The inverse of the matrix
-}
{#fun c_inverse as inverse
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #}

-- logdet
{-|
[@brief@] Log determinant
[@details@] Takes the log of the determinant of a matrix.
            (not implemented on GPU yet, although
            contributions are welcome: https://github.com/clab/dynet/issues/158).

[@parameters@]

    * @x@: A square matrix

[@return@] The log of its determinant
-}
{#fun c_logdet as logdet
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #}

-- trace_of_product
{-|
[@brief@] Trace of Matrix Product
[@details@] Takes the trace of the product of matrices.
            (not implemented on GPU yet, although
            contributions are welcome: https://github.com/clab/dynet/issues/158).

[@parameters@]

    * @x1@: A matrix
    * @x2@: Another matrix

[@return@] trace(x1 * x2)
-}
{#fun c_trace_of_product as traceOfProduct
    `(IsExpr ex1, IsExpr ex2)' =>
    {+S, withExpr* `ex1', withExpr* `ex2'} -> `Expression' #}

-- dot_product
{-|
[@brief@] Dot Product
[@details@] Calculate the dot product \(\sum_i x_i*y_i\)

[@parameters@]

    * @x@: The input expression
    * @y@: The input expression

[@return@] An expression equal to the dot product
-}
{#fun c_dot_product as dotProduct
    `(IsExpr ex1, IsExpr ex2)' =>
    {+S, withExpr* `ex1', withExpr* `ex2'} -> `Expression' #}

-- squared_distance
{-|
[@brief@] Squared distance
[@details@] The squared distance between values of @x@ and @y@: \(\sum_i (x_i-y_i)^2\).

[@parameters@]

    * @x@: A vector of values
    * @y@: Another vector of values

[@return@] The squared distance
-}
{#fun c_squared_distance as squaredDistance
    `(IsExpr ex1, IsExpr ex2)' =>
    {+S, withExpr* `ex1', withExpr* `ex2'} -> `Expression' #}

-- squared_norm
{-|
[@brief@] Squared norm
[@details@] The squared L2 norm of the values of x: \(\sum_i x_i^2\).

[@parameters@]

    * @x@: A vector of values

[@return@] The squared L2 norm
-}
{#fun c_squared_norm as squaredNorm
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #}

-- l2_norm
{-|
[@brief@] L2 norm
[@details@] The L2 norm of the values of x: \(\sum_i x_i^2\).

[@parameters@]

    * @x@: A vector of values

[@return@] The L2 norm
-}
{#fun c_l2_norm as l2Norm
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #}

-- huber_distance
{-|
[@brief@] Huber distance
[@details@] The huber distance between values of @x@ and @y@ parameterized
      by @c,@ \(\sum_i L_c(x_i, y_i)\) where:

      \[
        L_c(x, y) = \begin{cases}{lr}
          \frac{1}{2}(y - x)^2                   & \textrm{for } |y - f(x)| \le c, \\
          c\, |y - f(x)| - \frac{1}{2}c^2 & \textrm{otherwise.}
        \end{cases}
      \[

[@parameters@]

    * @x@: A vector of values
    * @y@: Another vector of values
    * @c@: The parameter of the huber distance parameterizing the cuttoff

[@return@] The huber distance
-}
{#fun c_huber_distance as huberDistance
    `(IsExpr ex1, IsExpr ex2)' =>
    {+S, withExpr* `ex1', withExpr* `ex2', `Float'} -> `Expression' #}

-- l1_distance
{-|
[@brief@] L1 distance
[@details@] The L1 distance between values of @x@ and @y@: \(\sum_i |x_i-y_i|\).

[@parameters@]

    * @x@: A vector of values
    * @y@: Another vector of values

[@return@] The squared distance
-}
{#fun c_l1_distance as l1Distance
    `(IsExpr ex1, IsExpr ex2)' =>
    {+S, withExpr* `ex1', withExpr* `ex2'} -> `Expression' #}

-- binary_log_loss
{-|
[@brief@] Binary log loss
[@details@] The log loss of a binary decision according to the sigmoid
            sigmoid function \(- \sum_i (y_i * ln(x_i) + (1-y_i) * ln(1-x_i)) \)

[@parameters@]

    * @x@: A vector of values
    * @y@: A vector of true answers

[@return@] The log loss of the sigmoid function
-}
{#fun c_binary_log_loss as binaryLogLoss
    `(IsExpr ex1, IsExpr ex2)' =>
    {+S, withExpr* `ex1', withExpr* `ex2'} -> `Expression' #}

-- pairwise_rank_loss
{-|
[@brief@] Pairwise rank loss
[@details@] A margin-based loss, where every margin violation for each pair of
            values is penalized: \(\sum_i max(m - x_i + y_i, 0)\)

[@parameters@]

    * @x@: A vector of values
    * @y@: A vector of true answers
    * @m@: The margin

[@return@] The pairwise rank loss
-}
{#fun c_pairwise_rank_loss as pairwiseRankLoss
    `(IsExpr ex1, IsExpr ex2)' =>
    {+S, withExpr* `ex1', withExpr* `ex2', `Float'} -> `Expression' #}

-- poisson_loss
{-|
[@brief@] Poisson loss
[@details@] The negative log probability of @y@ according to a Poisson
            distribution with parameter @x@. Useful in Poisson regression
            where, we try to predict the parameters of a Possion distribution
            to maximize the probability of data @y@.

[@parameters@]

    * @x@: The parameter of the Poisson distribution.
    * @y@: The target value

[@return@] The Poisson loss
-}
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
-- | deprecated but kept for backward compatibility
{#fun c_sum_cols as sumCols
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #}

-- kmh_ngram
{#fun c_kmh_ngram as kmhNgram
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Int'} -> `Expression' #}

-- conv2d
{-|
[@brief@] conv2d without bias
[@details@]
     2D convolution operator without bias parameters.
     'VALID' and 'SAME' convolutions are supported.
     Think about when stride is 1, the distinction:

     * *SAME*: output size is the same with input size. To do so, one needs to pad the input so the filter can sweep outside of the input maps.
     * *VALID*: output size shrinks by filter_size - 1, and the filters always sweep at valid positions inside the input maps. No padding needed.

     In detail, assume:

     * Input feature maps: (XH x XW x XC) x N
     * Filters: FH x FW x XC x FC, 4D tensor
     * Strides: strides[0] and strides[1] are row (h) and col (w) stride, respectively.

     For the *SAME* convolution: the output height (YH) and width (YW) are computed as:

     * YH = ceil(float(XH) / float(strides[0]))
     * YW = ceil(float(XW) / float(strides[1]))
     and the paddings are computed as:

     * pad_along_height = max((YH - 1) * strides[0] + FH - XH, 0)
     * pad_along_width = max((YW - 1) * strides[1] + FW - XW, 0)
     * pad_top = pad_along_height / 2
     * pad_bottom = pad_along_height - pad_top
     * pad_left = pad_along_width / 2
     * pad_right = pad_along_width - pad_left

     For the *VALID* convolution: the output height (YH) and width (YW) are computed as:

     * YH = ceil(float(XH - FH + 1) / float(strides[0]))
     * YW = ceil(float(XW - FW + 1) / float(strides[1]))

     and the paddings are always zeros.

[@parameters@]

    * @x@: The input feature maps: (H x W x Ci) x N (ColMaj), 3D tensor with an optional batch dimension
    * @f@: 2D convolution filters: H x W x Ci x Co (ColMaj), 4D tensor
    * @stride@: the row and column strides
    * @is_valid@: 'VALID' convolution or 'SAME' convolution, default is True ('VALID')

[@return@] The output feature maps (H x W x Co) x N, 3D tensor with an optional batch dimension
-}
{#fun c_conv2d as conv2d
    `(IsExpr ex1, IsExpr ex2, Sequence Word s)' =>
    {+S, withExpr* `ex1', withExpr* `ex2', withSequence* `s', `Bool'} -> `Expression' #}

-- conv2d
{-|
[@brief@] conv2d with bias
[@details@]
     2D convolution operator with bias parameters.
     'VALID' and 'SAME' convolutions are supported.
     Think about when stride is 1, the distinction:

     * *SAME*: output size is the same with input size. To do so, one needs to pad the input so the filter can sweep outside of the input maps.
     * *VALID*: output size shrinks by filter_size - 1, and the filters always sweep at valid positions inside the input maps. No padding needed.

     In detail, assume:

     * Input feature maps: XH x XW x XC x N
     * Filters: FH x FW x XC x FC
     * Strides: strides[0] and strides[1] are row (h) and col (w) stride, respectively.

     For the *SAME* convolution: the output height (YH) and width (YW) are computed as:

     * YH = ceil(float(XH) / float(strides[0]))
     * YW = ceil(float(XW) / float(strides[1]))
     and the paddings are computed as:

     * pad_along_height = max((YH - 1) * strides[0] + FH - XH, 0)
     * pad_along_width = max((YW - 1) * strides[1] + FW - XW, 0)
     * pad_top = pad_along_height / 2
     * pad_bottom = pad_along_height - pad_top
     * pad_left = pad_along_width / 2
     * pad_right = pad_along_width - pad_left

     For the *VALID* convolution: the output height (YH) and width (YW) are computed as:

     * YH = ceil(float(XH - FH + 1) / float(strides[0]))
     * YW = ceil(float(XW - FW + 1) / float(strides[1]))

     and the paddings are always zeros.

[@parameters@]

    * @x@: The input feature maps: (H x W x Ci) x N (ColMaj), 3D tensor with an optional batch dimension
    * @f@: 2D convolution filters: H x W x Ci x Co (ColMaj), 4D tensor
    * @b@: The bias (1D: Ci)
    * @stride@: the row and column strides
    * @is_valid@: 'VALID' convolution or 'SAME' convolution, default is True ('VALID')

[@return@] The output feature maps (H x W x Co) x N, 3D tensor with an optional batch dimension
-}
{#fun c_conv2d_1 as conv2d'
    `(IsExpr ex1, IsExpr ex2, IsExpr ex3, Sequence Word s)' =>
    {+S, withExpr* `ex1', withExpr* `ex2', withExpr* `ex3', withSequence* `s', `Bool'} -> `Expression' #}

-- maxpooling2d
{-|
[@brief@] maxpooling2d
[@details@]
     2D maxpooling operator.

[@parameters@]

    * @x@: The input feature maps: (H x W x Ci) x N (ColMaj), 3D tensor with an optional batch dimension
    * @ksize@: the height and width of the maxpooling2d window or kernel
    * @stride@: the row and column strides
    * @is_valid@: 'VALID' or 'SAME' (see comments for conv2d) , default is True ('VALID')

[@return@] The output feature maps (H x W x Co) x N, 3D tensor with an optional batch dimension
-}
{#fun c_maxpooling2d as maxpooling2d
    `(Sequence Word s1, Sequence Word s2)' =>
    {+S, `Expression', withSequence* `s1', withSequence* `s2', `Bool'} -> `Expression' #}

-- sum_batches
{-|
[@brief@] Sum over minibatches
[@details@] Sum an expression that consists of multiple minibatches into one of
            equal dimension but with only a single minibatch. This is useful
            for summing loss functions at the end of minibatch training.

[@parameters@]

    * @x@: The input mini-batched expression

[@return@] An expression with a single batch
-}
{#fun c_sum_batches as sumBatches
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #}

-- sum_elems
{-|
[@brief@] Sum all elements
[@details@] Sum all the elements in an expression.

[@parameters@]

    * @x@: The input expression

[@return@] The sum of all of its elements
-}
{#fun c_sum_elems as sumElems
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #}

-- moment_batches
{-|
[@brief@] Compute moment over minibatches
[@details@] Compute the moment of order \(r\), \(\frac 1 n\sum_{i=1}^nx_i^r\) along the batch dimension

[@parameters@]

    * @x@: The input mini-batched expression
    * @r@: Order of the moment

[@return@] An expression with a single batch
-}
{#fun c_moment_batches as momentBatches
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Int'} -> `Expression' #}

-- moment_elems
{-|
[@brief@] Compute moment over all elements
[@details@] Compute the moment of order \(r\), \(\frac 1 n\sum_{i=1}^nx_i^r\) over all the elements in each batch of the expression

[@parameters@]

    * @x@: The input mini-batched expression
    * @r@: Order of the moment

[@return@] A scalar expression (with a potential batch dimension)
-}
{#fun c_moment_elems as momentElems
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Int'} -> `Expression' #}

-- moment_dim
{-|
[@brief@] Compute moment along a specific dimension
[@details@] Compute the moment of order \(r\), \(\frac 1 n\sum_{i=1}^nx_i^r\) along a specific dimension

[@parameters@]

    * @x@: The input mini-batched expression
    * @d@: Dimensions along which to reduce
    * @r@: Order of the moment
    * @b@: Whether to include batch dimension (default: false)
    * @n@: If > 0, overwrite the n in the equation by this value, useful for masking (default: 0)

[@return@] An expression with |d| less dimensions and possibly dropped batch dimension
-}
{#fun c_moment_dim as momentDim
    `(IsExpr ex, Sequence Word s)' =>
    {+S, withExpr* `ex', withSequence* `s', `Int', `Bool', `Int'} -> `Expression' #}

-- mean_elems
{-|
[@brief@] Compute mean over all elements
[@details@] Computes \(\frac 1 n\sum_{i=1}^nx_i\) over all the elements in each batch of the expression

[@parameters@]

    * @x@: The input mini-batched expression

[@return@] A scalar expression (with a potential batch dimension)
-}
{#fun c_mean_elems as meanElems
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #}

-- mean_batches
{-|
[@brief@] Compute mean over minibatches
[@details@] Computes \(\frac 1 n\sum_{i=1}^nx_i\) along the batch dimension

[@parameters@]

    * @x@: The input mini-batched expression

[@return@] An expression with a single batch
-}
{#fun c_mean_batches as meanBatches
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #}

-- mean_dim
{-|
[@brief@] Compute mean along  a specific dimension
[@details@] Computes \(\frac 1 n\sum_{i=1}^nx_i\) along a specific dimension

[@parameters@]

    * @x@: The input mini-batched expression
    * @d@: Dimensions along which to reduce
    * @b@: Whether to include batch dimension (default: false)
    * @n@: If > 0, overwrite the n in the equation by this value, useful for masking (default: 0)

[@return@] An expression with |d| less dimensions and possibly dropped batch dimension
-}
{#fun c_mean_dim as meanDim
    `(IsExpr ex, Sequence Word s)' =>
    {+S, withExpr* `ex', withSequence* `s', `Bool', `Int'} -> `Expression' #}

-- std_dim
{-|
[@brief@] Compute standard deviation along an arbitrary dimension
[@details@] Computes \(\frac 1 n\sum_{i=1}^n(x_i -\mu)^2\) where \(\mu=\frac 1 n\sum_{i=1}^nx_i\) along an arbitrary dimension

[@parameters@]

    * @x@: The input mini-batched expression
    * @d@: Dimensions along which to reduce
    * @b@: Whether to include batch dimension (default: false)
    * @n@: If > 0, overwrite the n in the equation by this value, useful for masking (default: 0)

[@return@] An expression with |d| less dimensions and possibly dropped batch dimension
-}
{#fun c_std_dim as stdDim
    `(IsExpr ex, Sequence Word s)' =>
    {+S, withExpr* `ex', withSequence* `s', `Bool', `Int'} -> `Expression' #}

-- std_elems
{-|
[@brief@] Compute Standard deviation over all elements
[@details@] Computes \(\frac 1 n\sum_{i=1}^n(x_i -\mu)^2\) where \(\mu=\frac 1 n\sum_{i=1}^nx_i\) over all the elements in each batch of the expression

[@parameters@]

    * @x@: The input mini-batched expression

[@return@] A scalar expression (with a potential batch dimension)
-}
{#fun c_std_elems as stdElems
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #}

-- std_batches
{-|
[@brief@] Compute standard deviation over minibatches
[@details@] Computes \(\frac 1 n\sum_{i=1}^n(x_i -\mu)^2\) where \(\mu=\frac 1 n\sum_{i=1}^nx_i\) along the batch dimension

[@parameters@]

    * @x@: The input mini-batched expression

[@return@] A scalar expression (with a potential batch dimension)
-}
{#fun c_std_batches as stdBatches
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #}

-- select_rows
{-|
[@brief@] Select rows
[@details@] Select a subset of rows of a matrix.

[@parameters@]

    * @x@: The input expression
    * @rows@: The rows to extract

[@return@] An expression containing the selected rows
-}
{#fun c_select_rows as selectRows
    `(IsExpr ex, Sequence Word s)' =>
    {+S, withExpr* `ex', withSequence* `s'} -> `Expression' #}

-- select_cols
{-|
[@brief@] Select columns
[@details@] Select a subset of columns of a matrix. select_cols is more
            efficient than select_rows since DyNet uses column-major order.

[@parameters@]

    * @x@: The input expression
    * @columns@: The columns to extract

[@return@] An expression containing the selected columns
-}
{#fun c_select_cols as selectCols
    `(IsExpr ex, Sequence Word s)' =>
    {+S, withExpr* `ex', withSequence* `s'} -> `Expression' #}

-- pick
-- {#fun c_pick as pick
--     `IsExpr ex' =>
--     {+S, withExpr* `ex', unsigned* pv, `Int'} -> `Expression' #}

-- pick
{-|
[@brief@] Batched pick
[@details@] Pick elements from multiple batches.

[@parameters@]

    * @x@: The input expression
    * @v@: A vector of indicies to choose, one for each batch in the
            input expression.
    * @d@: The dimension along which to choose the elements

[@return@] A mini-batched expression containing the picked elements
-}
{#fun c_pick_1 as pick'
    `(IsExpr ex, Sequence Word s)' =>
    {+S, withExpr* `ex', withSequence* `s', `Int'} -> `Expression' #}

-- pick_range
{-|
[@brief@] Pick range of elements
[@details@] Pick a range of elements from an expression.

[@parameters@]

    * @x@: The input expression
    * @s@: The start index
    * @e@: The end index
    * @d@: The dimension along which to pick

[@return@] The value of {x[v],...,x[u]}
-}
{#fun c_pick_range as pickRange
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Int', `Int', `Int'} -> `Expression' #}

-- pick_batch_elems
{-|
[@brief@] Pick batch elements (Modifiable).

[@details@] Pick several batch elements from a batched expression. For a Tensor with 3 batch elements:

\[
  \begin{pmatrix}
    x_{1,1,1} & x_{1,1,2} \\
    x_{1,2,1} & x_{1,2,2} \\
  \end{pmatrix}
  \begin{pmatrix}
    x_{2,1,1} & x_{2,1,2} \\
    x_{2,2,1} & x_{2,2,2} \\
  \end{pmatrix}
  \begin{pmatrix}
    x_{3,1,1} & x_{3,1,2} \\
    x_{3,2,1} & x_{3,2,2} \\
  \end{pmatrix}
\]

    @pick_batch_elems(t, {1, 2})@ will return a Tensor of with 2 batch elements:

\[
  \begin{pmatrix}
    x_{2,1,1} & x_{2,1,2} \\
    x_{2,2,1} & x_{2,2,2} \\
  \end{pmatrix}
  \begin{pmatrix}
    x_{3,1,1} & x_{3,1,2} \\
    x_{3,2,1} & x_{3,2,2} \\
  \end{pmatrix}
\]

[@parameters@]

    * @x@: The input expression
    * @v@: A vector of indicies of the batch elements to be picked.

[@return@] The expression of picked batch elements. The batch elements is a tensor
        whose `bd` equals to the size of vector `v`.
-}
{#fun c_pick_batch_elems as pickBatchElems
    `(IsExpr ex, Sequence Word s)' =>
    {+S, withExpr* `ex', withSequence* `s'} -> `Expression' #}

-- pick_batch_elem
{-| [@brief@] Pick batch element (Modifiable).

[@details@] Pick batch element from a batched expression. For a Tensor with 3 batch elements:

\[
  \begin{pmatrix}
    x_{1,1,1} & x_{1,1,2} \\
    x_{1,2,1} & x_{1,2,2} \\
  \end{pmatrix}
  \begin{pmatrix}
    x_{2,1,1} & x_{2,1,2} \\
    x_{2,2,1} & x_{2,2,2} \\
  \end{pmatrix}
  \begin{pmatrix}
    x_{3,1,1} & x_{3,1,2} \\
    x_{3,2,1} & x_{3,2,2} \\
  \end{pmatrix}
\]

@pick_batch_elem(t, 1)@ will return a Tensor of

\[
  \begin{pmatrix}
    x_{2,1,1} & x_{2,1,2} \\
    x_{2,2,1} & x_{2,2,2} \\
  \end{pmatrix}
\]

[@parameters@]

    * @x@: The input expression
    * @v@: The index of the batch element to be picked.

[@return@] The expression of picked batch element. The picked element is a tensor
        whose `bd` equals to one.
-}
{#fun c_pick_batch_elem as pickBatchElem
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Int'} -> `Expression' #}

-- pickneglogsoftmax
{-|
[@brief@] Negative softmax log likelihood
[@details@] This function takes in a vector of scores @x@, and performs a log softmax, takes
         the negative, and selects the likelihood corresponding to the element @v@. This is
         perhaps the most standard loss function for training neural networks to predict
         one out of a set of elements.

[@parameters@]

    * @x@: A vector of scores
    * @v@: The element with which to calculate the loss

[@return@] The negative log likelihood of element @v@ after taking the softmax
-}
{#fun c_pickneglogsoftmax as pickneglogsoftmax
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Int'} -> `Expression' #}

-- pickneglogsoftmax
{-|
[@brief@] Batched negative softmax log likelihood
[@details@] This function is similar to standard pickneglogsoftmax, but calculates loss with
         respect to multiple batch elements. The input will be a mini-batch of score vectors
         where the number of batch elements is equal to the number of indices in @v@.

[@parameters@]

    * @x@: An expression with vectors of scores over N batch elements
    * @v@: A size-N vector indicating the index with respect to all the batch elements

[@return@] The negative log likelihoods over all the batch elements
-}
{#fun c_pickneglogsoftmax_1 as pickneglogsoftmax'
    `(IsExpr ex, Sequence Word s)' =>
    {+S, withExpr* `ex', withSequence* `s'} -> `Expression' #}

-- contract3d_1d
{-|
[@brief@] Contracts a rank 3 tensor and a rank 1 tensor into a rank 2 tensor
[@details@] The resulting tensor \(z\) has coordinates \(z_ij = \sum_k x_{ijk} y_k\)

[@parameters@]

    * @x@: Rank 3 tensor
    * @y@: Vector

[@return@] Matrix
-}
{#fun c_contract3d_1d as contract3d_1d
    `(IsExpr ex1, IsExpr ex2)' =>
    {+S, withExpr* `ex1', withExpr* `ex2'} -> `Expression' #}

-- contract3d_1d
{-|
[@brief@] Same as `contract3d_1d` with an additional bias parameter
[@details@] The resulting tensor \(z\) has coordinates \(z_{ij} = b_{ij}+\sum_k x_{ijk} y_k\)

[@parameters@]

    * @x@: Rank 3 tensor
    * @y@: Vector
    * @b@: Bias matrix
[@return@] Matrix
-}
{#fun c_contract3d_1d_1 as contract3d_1d'
    `(IsExpr ex1, IsExpr ex2, IsExpr ex3)' =>
    {+S, withExpr* `ex1', withExpr* `ex2', withExpr* `ex3'} -> `Expression' #}

-- contract3d_1d_1d
{-|
[@brief@] Contracts a rank 3 tensor and two rank 1 tensor into a rank 1 tensor
[@details@] This is the equivalent of calling 'contract3d_1d' and then performing a matrix vector multiplication.

   The resulting tensor \(t\) has coordinates \(t_i = \sum_{j,k} x_{ijk} y_k z_j\)

[@parameters@]

    * @x@: Rank 3 tensor
    * @y@: Vector
    * @z@: Vector
[@return@] Vector
-}
{#fun c_contract3d_1d_1d as contract3d_1d_1d
    `(IsExpr ex1, IsExpr ex2, IsExpr ex3)' =>
    {+S, withExpr* `ex1', withExpr* `ex2', withExpr* `ex3'} -> `Expression' #}

-- contract3d_1d_1d
{-|
[@brief@] Same as `contract3d_1d_1d` with an additional bias parameter
[@details@] This is the equivalent of calling `contract3d_1d` and then performing an affine transform.
   The resulting tensor \(t\) has coordinates \(t_i = b_i + \sum_{j,k} x_{ijk} y_k z_j\)

[@parameters@]

    * @x@: Rank 3 tensor
    * @y@: Vector
    * @z@: Vector
    * @b@: Bias vector
[@return@] Vector
-}
{#fun c_contract3d_1d_1d_1 as contract3d_1d_1d'
    `(IsExpr ex1, IsExpr ex2, IsExpr ex3, IsExpr ex4)' =>
    {+S, withExpr* `ex1', withExpr* `ex2', withExpr* `ex3', withExpr* `ex4'} -> `Expression' #}

-- elu
{-|
[@brief@] Exponential Linear Unit
[@details@] Calculate elementwise the function

\[
y_i = \left\{\begin{array}{lr}
           x_i, & \text{if } x>0\\
           \alpha\times(e^{x_i} - 1), & \text{if }x\leqslant 0\\
         \end{array}\right.
\]

Reference: [Clevert et al., 2015](https://arxiv.org/abs/1511.07289v5)

[@parameters@]

    * @x@: The input expression

[@return@] An expression where the ith element is equal to \(\text{ELU}(x_i, \alpha)\)
-}
{#fun c_elu as elu
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Float'} -> `Expression' #}

-- selu
{-|
[@brief@] Scaled Exponential Linear Unit (SELU)
[@details@] Calculate elementwise the function

\[
y_i = \lambda\times\left\{\begin{array}{lr}
           x_i, & \text{if } x>0\\
           \alpha\times(e^{x_i} - 1), & \text{if }x\leqslant 0\\
         \end{array}\right.
\]

With

\[
\begin{split}
\lambda &=\texttt{1.0507009873554804934193349852946}\\
\alpha &=\texttt{1.6732632423543772848170429916717}\\
\end{split}
\]

Reference: [Klambaouer et al., 2017](https://arxiv.org/abs/1706.02515)

[@parameters@]

    * @x@: The input expression

[@return@] An expression where the ith element is equal to \(\text{SELU}(x_i)\)
-}
{#fun c_selu as selu
    `IsExpr ex' =>
    {+S, withExpr* `ex'} -> `Expression' #}

-- average
{-|
[@brief@] Average
[@details@] This performs an elementwise average over all the expressions in xs

[@parameters@]

    * @xs@: An initializer list containing expressions

[@return@] An expression where the ith element is equal to (xs[0][i] + xs[1][i] + ...)/|xs|
-}
{#fun c_average as average
    `Sequence Expression s' =>
    {+S,  withSequence* `s'} -> `Expression' #}

-- concatenate_cols
{-|
[@brief@] Concatenate columns
[@details@] Perform a concatenation of the columns in multiple expressions.
         All expressions must have the same number of rows.

[@parameters@]

    * @xs@: The input expressions

[@return@] The expression with the columns concatenated
-}
{#fun c_concat_cols as concatCols
    `Sequence Expression s' =>
    {+S,  withSequence* `s'} -> `Expression' #}

-- concatenate
{-|
[@brief@] Concatenate
[@details@] Perform a concatenation of multiple expressions along
         a particular dimension.
         All expressions must have the same dimensions except for
         the dimension to be concatenated (rows by default).

[@parameters@]

    * xs The input expressions
    * d The dimension along which to perform concatenation

[@return@] The expression with the specified dimension concatenated
-}
{#fun c_concat as concat
    `Sequence Expression s' =>
    {+S,  withSequence* `s', `Int'} -> `Expression' #}

{-|
@
    concat' x = concat x 0
@
-}
concat' x = concat x 0

-- concatenate_to_batch
{-|
[@brief@] Concatenate list of expressions to a single batched expression
[@details@] Perform a concatenation of several expressions along the batch dimension.
         All expressions must have the same shape except for the batch dimension.

[@parameters@]

    * @xs@: The input expressions

[@return@] The expression with the batch dimensions concatenated
-}
{#fun c_concat_to_batch as concatToBatch
    `Sequence Expression s' =>
    {+S,  withSequence* `s'} -> `Expression' #}

-- sum
{-|
[@brief@] Sum
[@details@] This performs an elementwise sum over all the expressions in xs

    * @xs@: An initializer list containing expressions

[@return@] An expression where the ith element is equal to xs[0][i] + xs[1][i] + ...
-}
{#fun c_sum as sum
    `Sequence Expression s' =>
    {+S,  withSequence* `s'} -> `Expression' #}

-- max
{-|
[@brief@] Maximum
[@details@] Calculate an output where the ith element is \(max(x_i,y_i)\)

[@parameters@]

    * @x@: The first input expression
    * @y@: The second input expression

[@return@] An expression where the ith element is equal to \(max(x_i,y_i)\)
-}
{#fun c_max as max
    `Sequence Expression s' =>
    {+S,  withSequence* `s'} -> `Expression' #}

-- logsumexp
{-|
[@brief@] Log, sum, exp
[@details@] The elementwise "logsumexp" function that calculates
  \(ln(\sum_i e^{xs_i})\), used in adding probabilities in the log domain.

[@parameters@]

    * @xs@: Expressions with respect to which to calculate the logsumexp.

[@return@] The result.
-}
{#fun c_logsumexp as logsumexp
    `Sequence Expression s' =>
    {+S,  withSequence* `s'} -> `Expression' #}

-- max_dim
{-|
[@brief@] Max out through a dimension
[@details@] Select out a element \/ row \/ column \/ sub-tensor from an expression,
         with maximum value along a given dimension.
         This will result in the dimension of the tensor being reduced
         by 1.

[@parameters@]

    * @x@: The input expression
    * @d@: The dimension along which to choose the element

[@return@] An expression of sub-tensor with max value along dimension d
-}
{#fun c_max_dim as maxDim
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Int'} -> `Expression' #}

-- min_dim
{-|
[@brief@] Min out through a dimension
[@details@] Select out a element / row / column / sub-tensor from an expression,
         with minimum value along a given dimension.
         This will result in the dimension of the tensor being reduced
         by 1.

[@parameters@]

    * @x@: The input expression
    * @d@: The dimension along which to choose the element

[@return@] An expression of sub-tensor with min value along dimension d
-}
{#fun c_min_dim as minDim
    `IsExpr ex' =>
    {+S, withExpr* `ex', `Int'} -> `Expression' #}

-- layer_norm
{-|
[@brief@] Layer normalization
[@details@] Performs layer normalization :

\[
\begin{split}
   \mu &= \frac 1 n \sum_{i=1}^n x_i\\
   \sigma &= \sqrt{\frac 1 n \sum_{i=1}^n (x_i-\mu)^2}\\
   y&=\frac {\boldsymbol{g}} \sigma \circ (\boldsymbol{x}-\mu) + \boldsymbol{b}\\
\end{split}
\]

Reference : [Ba et al., 2016](http://arxiv.org/abs/1607.06450)

[@parameters@]

    * @x@: Input expression (possibly batched)
    * @g@: Gain (same dimension as x, no batch dimension)
    * @b@: Bias (same dimension as x, no batch dimension)
[@return@] An expression of the same dimension as `x`
-}
{#fun c_layer_norm as layerNorm
    `(IsExpr ex1, IsExpr ex2, IsExpr ex3)' =>
    {+S, withExpr* `ex1', withExpr* `ex2', withExpr* `ex3'} -> `Expression' #}

-- weight_norm
{-|
[@brief@] Weight normalization
[@details@] Performs weight normalization :

\[
\begin{split}
   \hat{w} &= g\frac{w}{\Vert w\Vert}\\
\end{split}
\]

Reference : [Salimans, Kingma 2016](https://arxiv.org/abs/1602.07868)

[@parameters@]

    * @w@: Input expression (weight parameter)
    * @g@: Gain (scalar expression, usually also a parameter)
[@return@] An expression of the same dimension as \(w\)
-}
{#fun c_weight_norm as weightNorm
    `(IsExpr ex1, IsExpr ex2)' =>
    {+S, withExpr* `ex1', withExpr* `ex2'} -> `Expression' #}
