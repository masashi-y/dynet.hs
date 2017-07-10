
#ifndef __INCUDE_C_DYNET_H
#define __INCUDE_C_DYNET_H

#include "vector.h"

#ifndef __cplusplus
#include <stdbool.h>
#endif

struct CDim;
typedef struct CDim CDim;

struct CTensor;
typedef struct CTensor CTensor;

struct CModel;
typedef struct CModel CModel;

struct CParameter;
typedef struct CParameter CParameter;

struct CLookupParameter;
typedef struct CLookupParameter CLookupParameter;

struct CParameterInit;
typedef struct CParameterInit CParameterInit;

struct CComputationGraph;
typedef struct CComputationGraph CComputationGraph;

struct CExpression;
typedef struct CExpression CExpression;

struct CTrainer;
typedef struct CTrainer CTrainer;

#ifdef __cplusplus
extern "C" {
#endif
    void dynet_initialize(int c, char** arg, bool shared_parameters); // TODO

    CDim* new_Dim_v(LongVector* ds);
    CDim* new_Dim_v_int(LongVector* ds, int bs);
    unsigned size_of_Dim();
    void init_Dim_v(CDim* d, LongVector* ds);
    void init_Dim_v_int(CDim* d, LongVector* ds, int bs);
    int Dim_size(CDim* d);
    unsigned int Dim_batch_elems(CDim* d);
    int Dim_sum_dims(CDim* d);
    // CDim* Dim_truncate(CDim* d);
    void Dim_resize(CDim* d, unsigned i);
    int Dim_ndims(CDim* d);
    int Dim_rows(CDim* d);
    int Dim_cols(CDim* d);
    unsigned Dim_at(CDim* d, unsigned i);
    void Dim_set(CDim* d, unsigned i, unsigned s);
    void delete_Dim(CDim* d);
    // int Dim_size(CDim* d, unsigned i);
    // CDim* Dim_transpose(CDim* d);

    void doNothing(void *);  // dummy deleter
    // Model (ParameterCollection)
    CModel* new_Model();
    void init_Model(CModel* m);
    void delete_Model(CModel* m);
    void delete_Parameter(CParameter* p);
    void delete_LookupParameter(CLookupParameter* p);
    unsigned size_of_Model();
    unsigned size_of_Parameter();
    unsigned size_of_LookupParameter();
    void Model_add_parameters(CModel* m, CParameter* p, CDim* d);
    void Model_add_parameters_1(CModel* m, CParameter* p, CDim* d, CParameterInit* initializer, char* name);
    void Model_add_lookup_parameters(CModel* m, CLookupParameter* p, unsigned n, CDim* d);
    void Model_add_lookup_parameters_1(CModel* m, CLookupParameter* p, unsigned n, CDim* d, CParameterInit* initializer, char* name);
    void Parameter_get_fullname(CParameter* p, char* out);
    //     // float gradient_l2_norm() const
    //     vector[CParameterStorage] Model_parameters_list()
    //     CModel Model_add_subcollection(string name)
    //     string Model_get_fullname()

    unsigned size_of_Tensor();
    void delete_Tensor(CTensor* t);
    CParameterInit* new_CParameterInitNormal(float m, float v);
    void init_CParameterInitNormal(CParameterInit* p, float m, float v);

    /***** ComputationGraph *****/
    CComputationGraph* new_ComputationGraph();
    void init_ComputationGraph(CComputationGraph* g);
    unsigned size_of_ComputationGraph();
    void delete_ComputationGraph(CComputationGraph* g);
    void ComputationGraph_print_graphviz(CComputationGraph* g);
    const CTensor* ComputationGraph_forward(CComputationGraph* g, CExpression* expr);
    void ComputationGraph_backward(CComputationGraph* g, CExpression* expr);

    float c_as_scalar(const CTensor* t);
    void Trainer_update(CTrainer* t, float s);
    CTrainer* new_SimpleSGDTrainer(CModel* m, float e0, float edecay);
    void init_SimpleSGDTrainer(CTrainer* t, CModel* m, float e0, float edecay);
    unsigned size_of_SimpleSGDTrainer();
    void delete_SimpleSGDTrainer(CTrainer* t);
    unsigned size_of_Trainer();
    void delete_Trainer(CTrainer* t);

    unsigned size_of_Expression();
    void delete_Expression(CExpression* x);
    // CExpression c_input "dynet::input" (CComputationGraph& g, float s)   #
    void c_input (CExpression* out, CComputationGraph* g, float *ps);
    void c_input_1 (CExpression* out, CComputationGraph* g, CDim* d, FloatVector* pdata);
    void c_input_2 (CExpression* out, CComputationGraph* g, CDim* d, UIntVector* ids, FloatVector* data, float defdata);
    void c_parameter (CExpression* out, CComputationGraph* g, CParameter* p);
    void c_parameter_1 (CExpression* out, CComputationGraph* g, CLookupParameter* p);
    void c_const_parameter (CExpression* out, CComputationGraph* g, CParameter* p);
    void c_const_parameter_1 (CExpression* out, CComputationGraph* g, CLookupParameter* p);
    // CExpression c_lookup "lookup" (CComputationGraph* g, CLookupParameter* p, unsigned index)
    void c_lookup (CExpression* out, CComputationGraph* g, CLookupParameter* p, unsigned* pindex);
    void c_lookup_1 (CExpression* out, CComputationGraph* g, CLookupParameter* p, UIntVector* pindices);
    // CExpression c_const_lookup "const_lookup" (CComputationGraph* g, CLookupParameter* p, unsigned index)
    void c_const_lookup (CExpression* out, CComputationGraph* g, CLookupParameter* p, unsigned* pindex);
    void c_const_lookup_1 (CExpression* out, CComputationGraph* g, CLookupParameter* p, UIntVector* pindices);
    void c_zeroes (CExpression* out, CComputationGraph* g, CDim* d);
    void c_random_normal (CExpression* out, CComputationGraph* g, CDim* d);
    void c_random_bernoulli (CExpression* out, CComputationGraph* g, CDim* d, float p, float scale);
    void c_random_uniform (CExpression* out, CComputationGraph* g, CDim* d, float left, float right);
    void c_random_gumbel (CExpression* out, CComputationGraph* g, CDim* d, float left, float right);

    //  identity function, but derivative is not propagated through it
    void c_nobackprop (CExpression* out, CExpression* x);
    //  identity function, but derivative takes negative as propagated through it
    void c_flip_gradient (CExpression* out, CExpression* x);

    void c_op_neg (CExpression* out, CExpression* x);
    void c_op_add (CExpression* out, CExpression* x, CExpression* y);
    void c_op_scalar_add (CExpression* out, CExpression* x, float y);
    void c_op_mul (CExpression* out, CExpression* x, CExpression* y);
    void c_op_scalar_mul (CExpression* out, CExpression* x, float y);
    void c_op_scalar_div (CExpression* out, CExpression* x, float y);
    void c_op_scalar_sub (CExpression* out, float y, CExpression* x);

    void c_bmax (CExpression* out, CExpression* x, CExpression* y);
    void c_bmin (CExpression* out, CExpression* x, CExpression* y);

    void c_cdiv (CExpression* out, CExpression* x, CExpression* y);
    void c_cmult (CExpression* out, CExpression* x, CExpression* y);
    void c_colwise_add (CExpression* out, CExpression* x, CExpression* bias);

    void c_tanh (CExpression* out, CExpression* x);
    void c_exp (CExpression* out, CExpression* x);
    void c_square (CExpression* out, CExpression* x);
    void c_sqrt (CExpression* out, CExpression* x);
    void c_abs (CExpression* out, CExpression* x);
    void c_erf (CExpression* out, CExpression* x);
    void c_cube (CExpression* out, CExpression* x);
    void c_log (CExpression* out, CExpression* x);
    void c_lgamma (CExpression* out, CExpression* x);
    void c_logistic (CExpression* out, CExpression* x);
    void c_rectify (CExpression* out, CExpression* x);
    void c_hinge (CExpression* out, CExpression* x, unsigned index, float m);
    void c_hinge_1 (CExpression* out, CExpression* x, UIntVector* vs, float m);
    void c_log_softmax (CExpression* out, CExpression* x);
    void c_log_softmax_1 (CExpression* out, CExpression* x, UIntVector* restriction);
    void c_softmax (CExpression* out, CExpression* x);
    void c_sparsemax (CExpression* out, CExpression* x);
    void c_softsign (CExpression* out, CExpression* x);
    void c_pow (CExpression* out, CExpression* x, CExpression* y);
    void c_bmin (CExpression* out, CExpression* x, CExpression* y);
    void c_bmax (CExpression* out, CExpression* x, CExpression* y);
    void c_noise (CExpression* out, CExpression* x, float stddev);
    void c_dropout (CExpression* out, CExpression* x, float p);
    void c_dropout_batch (CExpression* out, CExpression* x, float p);
    void c_dropout_dim (CExpression* out, CExpression* x, unsigned d, float p);
    void c_block_dropout (CExpression* out, CExpression* x, float p);

    void c_reshape (CExpression* out, CExpression* x, CDim* d);
    void c_transpose (CExpression* out, CExpression* x, UIntVector* dims);

    void c_affine_transform (CExpression* out, const ExpressionVector* xs);

    void c_inverse (CExpression* out, CExpression* x);
    void c_logdet (CExpression* out, CExpression* x);
    void c_trace_of_product (CExpression* out, CExpression* x, CExpression* y);

    void c_dot_product (CExpression* out, CExpression* x, CExpression* y);
    void c_squared_distance (CExpression* out, CExpression* x, CExpression* y);
    void c_squared_norm (CExpression* out, CExpression* x);
    void c_l2_norm (CExpression* out, CExpression* x);
    void c_huber_distance (CExpression* out, CExpression* x, CExpression* y, float c);
    void c_l1_distance (CExpression* out, CExpression* x, CExpression* y);
    void c_binary_log_loss (CExpression* out, CExpression* x, CExpression* y);
    void c_pairwise_rank_loss (CExpression* out, CExpression* x, CExpression* y, float m);
    void c_poisson_loss (CExpression* out, CExpression* x, unsigned y);

    // CExpression c_conv1d_narrow (CExpression* x, CExpression* f);
    // CExpression c_conv1d_wide (CExpression* x, CExpression* f);
    void c_filter1d_narrow (CExpression* out, CExpression* x, CExpression* f);
    void c_kmax_pooling (CExpression* out, CExpression* x, unsigned k, unsigned d);
    void c_fold_rows (CExpression* out, CExpression* x, unsigned nrows);
    void c_sum_cols (CExpression* out, CExpression* x);
    void c_kmh_ngram (CExpression* out, CExpression* x, unsigned n);
    void c_conv2d (CExpression* out, CExpression* x, CExpression* f, UIntVector* stride, bool is_valid);
    void c_conv2d_1 (CExpression* out, CExpression* x, CExpression* f, CExpression* b, UIntVector* stride, bool is_valid);
    void c_maxpooling2d (CExpression* out, CExpression* x, UIntVector* ksize, UIntVector* stride, bool is_valid);

    void c_sum_batches (CExpression* out, CExpression* x);
    void c_sum_elems (CExpression* out, CExpression* x);
    void c_moment_batches (CExpression* out, CExpression* x, unsigned r);
    void c_moment_elems (CExpression* out, CExpression* x, unsigned r);
    void c_moment_dim (CExpression* out, CExpression* x, unsigned d, unsigned r);
    void c_mean_elems (CExpression* out, CExpression* x);
    void c_mean_batches (CExpression* out, CExpression* x);
    void c_mean_dim (CExpression* out, CExpression* x, unsigned d);
    void c_std_dim (CExpression* out, CExpression* x, unsigned d);
    void c_std_elems (CExpression* out, CExpression* x);
    void c_std_batches (CExpression* out, CExpression* x);

    // CExpression c_pick (CExpression* x, unsigned v);
    void c_select_rows (CExpression* out, CExpression* x, UIntVector* rs);
    void c_select_cols (CExpression* out, CExpression* x, UIntVector* cs);
    void c_pick (CExpression* out, CExpression* x, unsigned* pv, unsigned d);
    void c_pick_1 (CExpression* out, CExpression* x, UIntVector* pv, unsigned d);
    void c_pick_range (CExpression* out, CExpression* x, unsigned v, unsigned u, unsigned d);

    void c_pick_batch_elems (CExpression* out, CExpression* x, UIntVector* vs);
    void c_pick_batch_elem (CExpression* out, CExpression* x, unsigned v);
    void c_pickneglogsoftmax (CExpression* out, CExpression* x, unsigned v);
    void c_pickneglogsoftmax_1 (CExpression* out, CExpression* x, UIntVector* vs);

    void c_contract3d_1d (CExpression* out, CExpression* x, CExpression* y);
    void c_contract3d_1d_1 (CExpression* out, CExpression* x, CExpression* y, CExpression* b);
    void c_contract3d_1d_1d (CExpression* out, CExpression* x, CExpression* y, CExpression* z);
    void c_contract3d_1d_1d_1 (CExpression* out, CExpression* x, CExpression* y, CExpression* z, CExpression* b);

    void c_elu (CExpression* out, CExpression* x, float alpha);
    void c_selu (CExpression* out, CExpression* x);

    //  expecting a vector of CExpression
    void c_average     (CExpression* out, ExpressionVector* xs);
    void c_concat_cols (CExpression* out, ExpressionVector* xs);
    void c_concat      (CExpression* out, ExpressionVector* xs, unsigned d);
    void c_concat_to_batch      (CExpression* out, ExpressionVector* xs);

    void c_sum            (CExpression* out, ExpressionVector* xs);
    void c_max            (CExpression* out, ExpressionVector* xs);
    void c_logsumexp      (CExpression* out, ExpressionVector* xs);

    void c_max_dim (CExpression* out, CExpression* x, unsigned d);
    void c_min_dim (CExpression* out, CExpression* x, unsigned d);

    void c_layer_norm (CExpression* out, CExpression* x, CExpression* g, CExpression* b);
    void c_weight_norm (CExpression* out, CExpression* w, CExpression* g);
#ifdef __cplusplus
}
#endif

#endif
