
#include "dynet/dynet.h"
#include "dynet/expr.h"

#include "dynet.h"

using namespace dynet;

// input
void c_input(CExpression* out, CComputationGraph* g, float *ps) {
    Expression res = input(*reinterpret_cast<ComputationGraph*>(g), ps);
    *reinterpret_cast<Expression*>(out) = res;
}

// input
void c_input_1(CExpression* out, CComputationGraph* g, CDim* d, FloatVector* pdata) {
    Expression res = input(*reinterpret_cast<ComputationGraph*>(g), *reinterpret_cast<Dim*>(d), reinterpret_cast<std::vector<float>*>(pdata));
    *reinterpret_cast<Expression*>(out) = res;
}

// input
void c_input_2(CExpression* out, CComputationGraph* g, CDim* d, UIntVector* ids, FloatVector* data, float defdata) {
    Expression res = input(*reinterpret_cast<ComputationGraph*>(g), *reinterpret_cast<Dim*>(d), *reinterpret_cast<std::vector<unsigned>*>(ids), *reinterpret_cast<std::vector<float>*>(data), defdata);
    *reinterpret_cast<Expression*>(out) = res;
}

// parameter
void c_parameter(CExpression* out, CComputationGraph* g, CParameter* p) {
    Expression res = parameter(*reinterpret_cast<ComputationGraph*>(g), *reinterpret_cast<Parameter*>(p));
    *reinterpret_cast<Expression*>(out) = res;
}

// parameter
void c_parameter_1(CExpression* out, CComputationGraph* g, CLookupParameter* p) {
    Expression res = parameter(*reinterpret_cast<ComputationGraph*>(g), *reinterpret_cast<LookupParameter*>(p));
    *reinterpret_cast<Expression*>(out) = res;
}

// const_parameter
void c_const_parameter(CExpression* out, CComputationGraph* g, CParameter* p) {
    Expression res = const_parameter(*reinterpret_cast<ComputationGraph*>(g), *reinterpret_cast<Parameter*>(p));
    *reinterpret_cast<Expression*>(out) = res;
}

// const_parameter
void c_const_parameter_1(CExpression* out, CComputationGraph* g, CLookupParameter* p) {
    Expression res = const_parameter(*reinterpret_cast<ComputationGraph*>(g), *reinterpret_cast<LookupParameter*>(p));
    *reinterpret_cast<Expression*>(out) = res;
}

// lookup
void c_lookup(CExpression* out, CComputationGraph* g, CLookupParameter* p, unsigned* pindex) {
    Expression res = lookup(*reinterpret_cast<ComputationGraph*>(g), *reinterpret_cast<LookupParameter*>(p), pindex);
    *reinterpret_cast<Expression*>(out) = res;
}

// lookup
void c_lookup_0(CExpression* out, CComputationGraph* g, CLookupParameter* p, unsigned pindex) {
    Expression res = lookup(*reinterpret_cast<ComputationGraph*>(g), *reinterpret_cast<LookupParameter*>(p), pindex);
    *reinterpret_cast<Expression*>(out) = res;
}

// lookup
void c_lookup_1(CExpression* out, CComputationGraph* g, CLookupParameter* p, UIntVector* pindices) {
    Expression res = lookup(*reinterpret_cast<ComputationGraph*>(g), *reinterpret_cast<LookupParameter*>(p), *reinterpret_cast<std::vector<unsigned>*>(pindices));
    *reinterpret_cast<Expression*>(out) = res;
}

// const_lookup
void c_const_lookup(CExpression* out, CComputationGraph* g, CLookupParameter* p, unsigned* pindex) {
    Expression res = const_lookup(*reinterpret_cast<ComputationGraph*>(g), *reinterpret_cast<LookupParameter*>(p), pindex);
    *reinterpret_cast<Expression*>(out) = res;
}

// const_lookup
void c_const_lookup_1(CExpression* out, CComputationGraph* g, CLookupParameter* p, UIntVector* pindices) {
    Expression res = const_lookup(*reinterpret_cast<ComputationGraph*>(g), *reinterpret_cast<LookupParameter*>(p), reinterpret_cast<std::vector<unsigned>*>(pindices));
    *reinterpret_cast<Expression*>(out) = res;
}

// zeroes
void c_zeroes(CExpression* out, CComputationGraph* g, CDim* d) {
    Expression res = zeroes(*reinterpret_cast<ComputationGraph*>(g), *reinterpret_cast<Dim*>(d));
    *reinterpret_cast<Expression*>(out) = res;
}

// random_normal
void c_random_normal(CExpression* out, CComputationGraph* g, CDim* d) {
    Expression res = random_normal(*reinterpret_cast<ComputationGraph*>(g), *reinterpret_cast<Dim*>(d));
    *reinterpret_cast<Expression*>(out) = res;
}

// random_bernoulli
void c_random_bernoulli(CExpression* out, CComputationGraph* g, CDim* d, float p, float scale) {
    Expression res = random_bernoulli(*reinterpret_cast<ComputationGraph*>(g), *reinterpret_cast<Dim*>(d), p, scale);
    *reinterpret_cast<Expression*>(out) = res;
}

// random_uniform
void c_random_uniform(CExpression* out, CComputationGraph* g, CDim* d, float left, float right) {
    Expression res = random_uniform(*reinterpret_cast<ComputationGraph*>(g), *reinterpret_cast<Dim*>(d), left, right);
    *reinterpret_cast<Expression*>(out) = res;
}

// random_gumbel
void c_random_gumbel(CExpression* out, CComputationGraph* g, CDim* d, float left, float right) {
    Expression res = random_gumbel(*reinterpret_cast<ComputationGraph*>(g), *reinterpret_cast<Dim*>(d), left, right);
    *reinterpret_cast<Expression*>(out) = res;
}

// nobackprop
void c_nobackprop(CExpression* out, CExpression* x) {
    Expression res = nobackprop(*reinterpret_cast<Expression*>(x));
    *reinterpret_cast<Expression*>(out) = res;
}

// flip_gradient
void c_flip_gradient(CExpression* out, CExpression* x) {
    Expression res = flip_gradient(*reinterpret_cast<Expression*>(x));
    *reinterpret_cast<Expression*>(out) = res;
}

// operator-
void c_op_neg(CExpression* out, CExpression* x) {
    Expression res = -(*reinterpret_cast<Expression*>(x));
    *reinterpret_cast<Expression*>(out) = res;
}

// operator+
void c_op_add(CExpression* out, CExpression* x, CExpression* y) {
    Expression res = (*reinterpret_cast<Expression*>(x)) + (*reinterpret_cast<Expression*>(y));
    *reinterpret_cast<Expression*>(out) = res;
}

// operator+
void c_op_scalar_add(CExpression* out, CExpression* x, float y) {
    Expression res = (*reinterpret_cast<Expression*>(x)) + y;
    *reinterpret_cast<Expression*>(out) = res;
}

// operator*
void c_op_mul(CExpression* out, CExpression* x, CExpression* y) {
    Expression res = (*reinterpret_cast<Expression*>(x)) * (*reinterpret_cast<Expression*>(y));
    *reinterpret_cast<Expression*>(out) = res;
}

// operator*
void c_op_scalar_mul(CExpression* out, CExpression* x, float y) {
    Expression res = (*reinterpret_cast<Expression*>(x)) * y;
    *reinterpret_cast<Expression*>(out) = res;
}

// operator/
void c_op_scalar_div(CExpression* out, CExpression* x, float y) {
    Expression res = (*reinterpret_cast<Expression*>(x)) / y;
    *reinterpret_cast<Expression*>(out) = res;
}

// operator-
void c_op_scalar_sub(CExpression* out, float y, CExpression* x) {
    Expression res = y - (*reinterpret_cast<Expression*>(x));
    *reinterpret_cast<Expression*>(out) = res;
}

// cdiv
void c_cdiv(CExpression* out, CExpression* x, CExpression* y) {
    Expression res = cdiv(*reinterpret_cast<Expression*>(x), *reinterpret_cast<Expression*>(y));
    *reinterpret_cast<Expression*>(out) = res;
}

// cmult
void c_cmult(CExpression* out, CExpression* x, CExpression* y) {
    Expression res = cmult(*reinterpret_cast<Expression*>(x), *reinterpret_cast<Expression*>(y));
    *reinterpret_cast<Expression*>(out) = res;
}

// colwise_add
void c_colwise_add(CExpression* out, CExpression* x, CExpression* bias) {
    Expression res = colwise_add(*reinterpret_cast<Expression*>(x), *reinterpret_cast<Expression*>(bias));
    *reinterpret_cast<Expression*>(out) = res;
}

// tanh
void c_tanh(CExpression* out, CExpression* x) {
    Expression res = tanh(*reinterpret_cast<Expression*>(x));
    *reinterpret_cast<Expression*>(out) = res;
}

// exp
void c_exp(CExpression* out, CExpression* x) {
    Expression res = exp(*reinterpret_cast<Expression*>(x));
    *reinterpret_cast<Expression*>(out) = res;
}

// square
void c_square(CExpression* out, CExpression* x) {
    Expression res = square(*reinterpret_cast<Expression*>(x));
    *reinterpret_cast<Expression*>(out) = res;
}

// sqrt
void c_sqrt(CExpression* out, CExpression* x) {
    Expression res = sqrt(*reinterpret_cast<Expression*>(x));
    *reinterpret_cast<Expression*>(out) = res;
}

// abs
void c_abs(CExpression* out, CExpression* x) {
    Expression res = abs(*reinterpret_cast<Expression*>(x));
    *reinterpret_cast<Expression*>(out) = res;
}

// erf
void c_erf(CExpression* out, CExpression* x) {
    Expression res = erf(*reinterpret_cast<Expression*>(x));
    *reinterpret_cast<Expression*>(out) = res;
}

// cube
void c_cube(CExpression* out, CExpression* x) {
    Expression res = cube(*reinterpret_cast<Expression*>(x));
    *reinterpret_cast<Expression*>(out) = res;
}

// log
void c_log(CExpression* out, CExpression* x) {
    Expression res = log(*reinterpret_cast<Expression*>(x));
    *reinterpret_cast<Expression*>(out) = res;
}

// lgamma
void c_lgamma(CExpression* out, CExpression* x) {
    Expression res = lgamma(*reinterpret_cast<Expression*>(x));
    *reinterpret_cast<Expression*>(out) = res;
}

// logistic
void c_logistic(CExpression* out, CExpression* x) {
    Expression res = logistic(*reinterpret_cast<Expression*>(x));
    *reinterpret_cast<Expression*>(out) = res;
}

// rectify
void c_rectify(CExpression* out, CExpression* x) {
    Expression res = rectify(*reinterpret_cast<Expression*>(x));
    *reinterpret_cast<Expression*>(out) = res;
}

// hinge
void c_hinge(CExpression* out, CExpression* x, unsigned index, float m) {
    Expression res = hinge(*reinterpret_cast<Expression*>(x), index, m);
    *reinterpret_cast<Expression*>(out) = res;
}

// hinge
void c_hinge_1(CExpression* out, CExpression* x, UIntVector* vs, float m) {
    Expression res = hinge(*reinterpret_cast<Expression*>(x), *reinterpret_cast<std::vector<unsigned>*>(vs), m);
    *reinterpret_cast<Expression*>(out) = res;
}

// log_softmax
void c_log_softmax(CExpression* out, CExpression* x) {
    Expression res = log_softmax(*reinterpret_cast<Expression*>(x));
    *reinterpret_cast<Expression*>(out) = res;
}

// log_softmax
void c_log_softmax_1(CExpression* out, CExpression* x, UIntVector* restriction) {
    Expression res = log_softmax(*reinterpret_cast<Expression*>(x), *reinterpret_cast<std::vector<unsigned>*>(restriction));
    *reinterpret_cast<Expression*>(out) = res;
}

// softmax
void c_softmax(CExpression* out, CExpression* x) {
    Expression res = softmax(*reinterpret_cast<Expression*>(x));
    *reinterpret_cast<Expression*>(out) = res;
}

// softsign
void c_softsign(CExpression* out, CExpression* x) {
    Expression res = softsign(*reinterpret_cast<Expression*>(x));
    *reinterpret_cast<Expression*>(out) = res;
}

// pow
void c_pow(CExpression* out, CExpression* x, CExpression* y) {
    Expression res = pow(*reinterpret_cast<Expression*>(x), *reinterpret_cast<Expression*>(y));
    *reinterpret_cast<Expression*>(out) = res;
}

// min
void c_bmin(CExpression* out, CExpression* x, CExpression* y) {
    Expression res = min(*reinterpret_cast<Expression*>(x), *reinterpret_cast<Expression*>(y));
    *reinterpret_cast<Expression*>(out) = res;
}

// max
void c_bmax(CExpression* out, CExpression* x, CExpression* y) {
    Expression res = max(*reinterpret_cast<Expression*>(x), *reinterpret_cast<Expression*>(y));
    *reinterpret_cast<Expression*>(out) = res;
}

// noise
void c_noise(CExpression* out, CExpression* x, float stddev) {
    Expression res = noise(*reinterpret_cast<Expression*>(x), stddev);
    *reinterpret_cast<Expression*>(out) = res;
}

// dropout
void c_dropout(CExpression* out, CExpression* x, float p) {
    Expression res = dropout(*reinterpret_cast<Expression*>(x), p);
    *reinterpret_cast<Expression*>(out) = res;
}

// dropout_batch
void c_dropout_batch(CExpression* out, CExpression* x, float p) {
    Expression res = dropout_batch(*reinterpret_cast<Expression*>(x), p);
    *reinterpret_cast<Expression*>(out) = res;
}

// dropout_dim
void c_dropout_dim(CExpression* out, CExpression* x, unsigned d, float p) {
    Expression res = dropout_dim(*reinterpret_cast<Expression*>(x), d, p);
    *reinterpret_cast<Expression*>(out) = res;
}

// block_dropout
void c_block_dropout(CExpression* out, CExpression* x, float p) {
    Expression res = block_dropout(*reinterpret_cast<Expression*>(x), p);
    *reinterpret_cast<Expression*>(out) = res;
}

// reshape
void c_reshape(CExpression* out, CExpression* x, CDim* d) {
    Expression res = reshape(*reinterpret_cast<Expression*>(x), *reinterpret_cast<Dim*>(d));
    *reinterpret_cast<Expression*>(out) = res;
}

// transpose
void c_transpose(CExpression* out, CExpression* x, UIntVector* dims) {
    Expression res = transpose(*reinterpret_cast<Expression*>(x), *reinterpret_cast<std::vector<unsigned>*>(dims));
    *reinterpret_cast<Expression*>(out) = res;
}

// affine_transform
void c_affine_transform(CExpression* out, ExpressionVector* xs) {
    Expression res = affine_transform(*reinterpret_cast<std::vector<Expression>*>(xs));
    *reinterpret_cast<Expression*>(out) = res;
}

// inverse
void c_inverse(CExpression* out, CExpression* x) {
    Expression res = inverse(*reinterpret_cast<Expression*>(x));
    *reinterpret_cast<Expression*>(out) = res;
}

// logdet
void c_logdet(CExpression* out, CExpression* x) {
    Expression res = logdet(*reinterpret_cast<Expression*>(x));
    *reinterpret_cast<Expression*>(out) = res;
}

// trace_of_product
void c_trace_of_product(CExpression* out, CExpression* x, CExpression* y) {
    Expression res = trace_of_product(*reinterpret_cast<Expression*>(x), *reinterpret_cast<Expression*>(y));
    *reinterpret_cast<Expression*>(out) = res;
}

// dot_product
void c_dot_product(CExpression* out, CExpression* x, CExpression* y) {
    Expression res = dot_product(*reinterpret_cast<Expression*>(x), *reinterpret_cast<Expression*>(y));
    *reinterpret_cast<Expression*>(out) = res;
}

// squared_distance
void c_squared_distance(CExpression* out, CExpression* x, CExpression* y) {
    Expression res = squared_distance(*reinterpret_cast<Expression*>(x), *reinterpret_cast<Expression*>(y));
    *reinterpret_cast<Expression*>(out) = res;
}

// squared_norm
void c_squared_norm(CExpression* out, CExpression* x) {
    Expression res = squared_norm(*reinterpret_cast<Expression*>(x));
    *reinterpret_cast<Expression*>(out) = res;
}

// l2_norm
void c_l2_norm(CExpression* out, CExpression* x) {
    Expression res = l2_norm(*reinterpret_cast<Expression*>(x));
    *reinterpret_cast<Expression*>(out) = res;
}

// huber_distance
void c_huber_distance(CExpression* out, CExpression* x, CExpression* y, float c) {
    Expression res = huber_distance(*reinterpret_cast<Expression*>(x), *reinterpret_cast<Expression*>(y), c);
    *reinterpret_cast<Expression*>(out) = res;
}

// l1_distance
void c_l1_distance(CExpression* out, CExpression* x, CExpression* y) {
    Expression res = l1_distance(*reinterpret_cast<Expression*>(x), *reinterpret_cast<Expression*>(y));
    *reinterpret_cast<Expression*>(out) = res;
}

// binary_log_loss
void c_binary_log_loss(CExpression* out, CExpression* x, CExpression* y) {
    Expression res = binary_log_loss(*reinterpret_cast<Expression*>(x), *reinterpret_cast<Expression*>(y));
    *reinterpret_cast<Expression*>(out) = res;
}

// pairwise_rank_loss
void c_pairwise_rank_loss(CExpression* out, CExpression* x, CExpression* y, float m) {
    Expression res = pairwise_rank_loss(*reinterpret_cast<Expression*>(x), *reinterpret_cast<Expression*>(y), m);
    *reinterpret_cast<Expression*>(out) = res;
}

// poisson_loss
void c_poisson_loss(CExpression* out, CExpression* x, unsigned y) {
    Expression res = poisson_loss(*reinterpret_cast<Expression*>(x), y);
    *reinterpret_cast<Expression*>(out) = res;
}

// filter1d_narrow
void c_filter1d_narrow(CExpression* out, CExpression* x, CExpression* f) {
    Expression res = filter1d_narrow(*reinterpret_cast<Expression*>(x), *reinterpret_cast<Expression*>(f));
    *reinterpret_cast<Expression*>(out) = res;
}

// kmax_pooling
void c_kmax_pooling(CExpression* out, CExpression* x, unsigned k, unsigned d) {
    Expression res = kmax_pooling(*reinterpret_cast<Expression*>(x), k, d);
    *reinterpret_cast<Expression*>(out) = res;
}

// fold_rows
void c_fold_rows(CExpression* out, CExpression* x, unsigned nrows) {
    Expression res = fold_rows(*reinterpret_cast<Expression*>(x), nrows);
    *reinterpret_cast<Expression*>(out) = res;
}

// sum_cols
void c_sum_cols(CExpression* out, CExpression* x) {
    Expression res = sum_cols(*reinterpret_cast<Expression*>(x));
    *reinterpret_cast<Expression*>(out) = res;
}

// kmh_ngram
void c_kmh_ngram(CExpression* out, CExpression* x, unsigned n) {
    Expression res = kmh_ngram(*reinterpret_cast<Expression*>(x), n);
    *reinterpret_cast<Expression*>(out) = res;
}

// conv2d
void c_conv2d(CExpression* out, CExpression* x, CExpression* f, UIntVector* stride, bool is_valid) {
    Expression res = conv2d(*reinterpret_cast<Expression*>(x), *reinterpret_cast<Expression*>(f), *reinterpret_cast<std::vector<unsigned>*>(stride), is_valid);
    *reinterpret_cast<Expression*>(out) = res;
}

// conv2d
void c_conv2d_1(CExpression* out, CExpression* x, CExpression* f, CExpression* b, UIntVector* stride, bool is_valid) {
    Expression res = conv2d(*reinterpret_cast<Expression*>(x), *reinterpret_cast<Expression*>(f), *reinterpret_cast<Expression*>(b), *reinterpret_cast<std::vector<unsigned>*>(stride), is_valid);
    *reinterpret_cast<Expression*>(out) = res;
}

// maxpooling2d
void c_maxpooling2d(CExpression* out, CExpression* x, UIntVector* ksize, UIntVector* stride, bool is_valid) {
    Expression res = maxpooling2d(*reinterpret_cast<Expression*>(x), *reinterpret_cast<std::vector<unsigned>*>(ksize), *reinterpret_cast<std::vector<unsigned>*>(stride), is_valid);
    *reinterpret_cast<Expression*>(out) = res;
}

// sum_batches
void c_sum_batches(CExpression* out, CExpression* x) {
    Expression res = sum_batches(*reinterpret_cast<Expression*>(x));
    *reinterpret_cast<Expression*>(out) = res;
}

// sum_elems
void c_sum_elems(CExpression* out, CExpression* x) {
    Expression res = sum_elems(*reinterpret_cast<Expression*>(x));
    *reinterpret_cast<Expression*>(out) = res;
}

// moment_batches
void c_moment_batches(CExpression* out, CExpression* x, unsigned r) {
    Expression res = moment_batches(*reinterpret_cast<Expression*>(x), r);
    *reinterpret_cast<Expression*>(out) = res;
}

// moment_elems
void c_moment_elems(CExpression* out, CExpression* x, unsigned r) {
    Expression res = moment_elems(*reinterpret_cast<Expression*>(x), r);
    *reinterpret_cast<Expression*>(out) = res;
}

// moment_dim
void c_moment_dim(CExpression* out, CExpression* x, unsigned d, unsigned r) {
    Expression res = moment_dim(*reinterpret_cast<Expression*>(x), d, r);
    *reinterpret_cast<Expression*>(out) = res;
}

// mean_elems
void c_mean_elems(CExpression* out, CExpression* x) {
    Expression res = mean_elems(*reinterpret_cast<Expression*>(x));
    *reinterpret_cast<Expression*>(out) = res;
}

// mean_batches
void c_mean_batches(CExpression* out, CExpression* x) {
    Expression res = mean_batches(*reinterpret_cast<Expression*>(x));
    *reinterpret_cast<Expression*>(out) = res;
}

// mean_dim
void c_mean_dim(CExpression* out, CExpression* x, unsigned d) {
    Expression res = mean_dim(*reinterpret_cast<Expression*>(x), d);
    *reinterpret_cast<Expression*>(out) = res;
}

// std_dim
void c_std_dim(CExpression* out, CExpression* x, unsigned d) {
    Expression res = std_dim(*reinterpret_cast<Expression*>(x), d);
    *reinterpret_cast<Expression*>(out) = res;
}

// std_elems
void c_std_elems(CExpression* out, CExpression* x) {
    Expression res = std_elems(*reinterpret_cast<Expression*>(x));
    *reinterpret_cast<Expression*>(out) = res;
}

// std_batches
void c_std_batches(CExpression* out, CExpression* x) {
    Expression res = std_batches(*reinterpret_cast<Expression*>(x));
    *reinterpret_cast<Expression*>(out) = res;
}

// select_rows
void c_select_rows(CExpression* out, CExpression* x, UIntVector* rs) {
    Expression res = select_rows(*reinterpret_cast<Expression*>(x), *reinterpret_cast<std::vector<unsigned>*>(rs));
    *reinterpret_cast<Expression*>(out) = res;
}

// select_cols
void c_select_cols(CExpression* out, CExpression* x, UIntVector* cs) {
    Expression res = select_cols(*reinterpret_cast<Expression*>(x), *reinterpret_cast<std::vector<unsigned>*>(cs));
    *reinterpret_cast<Expression*>(out) = res;
}

// pick
void c_pick(CExpression* out, CExpression* x, unsigned* pv, unsigned d) {
    Expression res = pick(*reinterpret_cast<Expression*>(x), pv, d);
    *reinterpret_cast<Expression*>(out) = res;
}

// pick
void c_pick_1(CExpression* out, CExpression* x, UIntVector* pv, unsigned d) {
    Expression res = pick(*reinterpret_cast<Expression*>(x), reinterpret_cast<std::vector<unsigned>*>(pv), d);
    *reinterpret_cast<Expression*>(out) = res;
}

// pick_range
void c_pick_range(CExpression* out, CExpression* x, unsigned v, unsigned u, unsigned d) {
    Expression res = pick_range(*reinterpret_cast<Expression*>(x), v, u, d);
    *reinterpret_cast<Expression*>(out) = res;
}

// pick_batch_elems
void c_pick_batch_elems(CExpression* out, CExpression* x, UIntVector* vs) {
    Expression res = pick_batch_elems(*reinterpret_cast<Expression*>(x), *reinterpret_cast<std::vector<unsigned>*>(vs));
    *reinterpret_cast<Expression*>(out) = res;
}

// pick_batch_elem
void c_pick_batch_elem(CExpression* out, CExpression* x, unsigned v) {
    Expression res = pick_batch_elem(*reinterpret_cast<Expression*>(x), v);
    *reinterpret_cast<Expression*>(out) = res;
}

// pickneglogsoftmax
void c_pickneglogsoftmax(CExpression* out, CExpression* x, unsigned v) {
    Expression res = pickneglogsoftmax(*reinterpret_cast<Expression*>(x), v);
    *reinterpret_cast<Expression*>(out) = res;
}

// pickneglogsoftmax
void c_pickneglogsoftmax_1(CExpression* out, CExpression* x, UIntVector* vs) {
    Expression res = pickneglogsoftmax(*reinterpret_cast<Expression*>(x), *reinterpret_cast<std::vector<unsigned>*>(vs));
    *reinterpret_cast<Expression*>(out) = res;
}

// contract3d_1d
void c_contract3d_1d(CExpression* out, CExpression* x, CExpression* y) {
    Expression res = contract3d_1d(*reinterpret_cast<Expression*>(x), *reinterpret_cast<Expression*>(y));
    *reinterpret_cast<Expression*>(out) = res;
}

// contract3d_1d
void c_contract3d_1d_1(CExpression* out, CExpression* x, CExpression* y, CExpression* b) {
    Expression res = contract3d_1d(*reinterpret_cast<Expression*>(x), *reinterpret_cast<Expression*>(y), *reinterpret_cast<Expression*>(b));
    *reinterpret_cast<Expression*>(out) = res;
}

// contract3d_1d_1d
void c_contract3d_1d_1d(CExpression* out, CExpression* x, CExpression* y, CExpression* z) {
    Expression res = contract3d_1d_1d(*reinterpret_cast<Expression*>(x), *reinterpret_cast<Expression*>(y), *reinterpret_cast<Expression*>(z));
    *reinterpret_cast<Expression*>(out) = res;
}

// contract3d_1d_1d
void c_contract3d_1d_1d_1(CExpression* out, CExpression* x, CExpression* y, CExpression* z, CExpression* b) {
    Expression res = contract3d_1d_1d(*reinterpret_cast<Expression*>(x), *reinterpret_cast<Expression*>(y), *reinterpret_cast<Expression*>(z), *reinterpret_cast<Expression*>(b));
    *reinterpret_cast<Expression*>(out) = res;
}

// elu
void c_elu(CExpression* out, CExpression* x, float alpha) {
    Expression res = elu(*reinterpret_cast<Expression*>(x), alpha);
    *reinterpret_cast<Expression*>(out) = res;
}

// selu
void c_selu(CExpression* out, CExpression* x) {
    Expression res = selu(*reinterpret_cast<Expression*>(x));
    *reinterpret_cast<Expression*>(out) = res;
}

// average
void c_average(CExpression* out, ExpressionVector* xs) {
    Expression res = average(*reinterpret_cast<std::vector<Expression>*>(xs));
    *reinterpret_cast<Expression*>(out) = res;
}

// concatenate_cols
void c_concat_cols(CExpression* out, ExpressionVector* xs) {
    Expression res = concatenate_cols(*reinterpret_cast<std::vector<Expression>*>(xs));
    *reinterpret_cast<Expression*>(out) = res;
}

// concatenate
void c_concat(CExpression* out, ExpressionVector* xs, unsigned d) {
    Expression res = concatenate(*reinterpret_cast<std::vector<Expression>*>(xs), d);
    *reinterpret_cast<Expression*>(out) = res;
}

// concatenate_to_batch
void c_concat_to_batch(CExpression* out, ExpressionVector* xs) {
    Expression res = concatenate_to_batch(*reinterpret_cast<std::vector<Expression>*>(xs));
    *reinterpret_cast<Expression*>(out) = res;
}

// sum
void c_sum(CExpression* out, ExpressionVector* xs) {
    Expression res = sum(*reinterpret_cast<std::vector<Expression>*>(xs));
    *reinterpret_cast<Expression*>(out) = res;
}

// max
void c_max(CExpression* out, ExpressionVector* xs) {
    Expression res = max(*reinterpret_cast<std::vector<Expression>*>(xs));
    *reinterpret_cast<Expression*>(out) = res;
}

// logsumexp
void c_logsumexp(CExpression* out, ExpressionVector* xs) {
    Expression res = logsumexp(*reinterpret_cast<std::vector<Expression>*>(xs));
    *reinterpret_cast<Expression*>(out) = res;
}

// max_dim
void c_max_dim(CExpression* out, CExpression* x, unsigned d) {
    Expression res = max_dim(*reinterpret_cast<Expression*>(x), d);
    *reinterpret_cast<Expression*>(out) = res;
}

// min_dim
void c_min_dim(CExpression* out, CExpression* x, unsigned d) {
    Expression res = min_dim(*reinterpret_cast<Expression*>(x), d);
    *reinterpret_cast<Expression*>(out) = res;
}

// layer_norm
void c_layer_norm(CExpression* out, CExpression* x, CExpression* g, CExpression* b) {
    Expression res = layer_norm(*reinterpret_cast<Expression*>(x), *reinterpret_cast<Expression*>(g), *reinterpret_cast<Expression*>(b));
    *reinterpret_cast<Expression*>(out) = res;
}

// weight_norm
void c_weight_norm(CExpression* out, CExpression* w, CExpression* g) {
    Expression res = weight_norm(*reinterpret_cast<Expression*>(w), *reinterpret_cast<Expression*>(g));
    *reinterpret_cast<Expression*>(out) = res;
}
