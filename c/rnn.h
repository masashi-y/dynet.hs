
#ifndef __INCUDE_C_RNN_H
#define __INCUDE_C_RNN_H

#include "dynet.h"

struct CRNNBuilder;
typedef struct CRNNBuilder CRNNBuilder;

struct CSimpleRNNBuilder;
typedef struct CSimpleRNNBuilder CSimpleRNNBuilder;

struct CVanillaLSTMBuilder;
typedef struct CVanillaLSTMBuilder CVanillaLSTMBuilder;

struct CCoupledLSTMBuilder;
typedef struct CCoupledLSTMBuilder CCoupledLSTMBuilder;

struct CDeepLSTMBuilder;
typedef struct CDeepLSTMBuilder CDeepLSTMBuilder;

struct CFastLSTMBuilder;
typedef struct CFastLSTMBuilder CFastLSTMBuilder;

struct CGRUBuilder;
typedef struct CGRUBuilder CGRUBuilder;

#ifdef __cplusplus
extern "C" {
#endif

int state(CRNNBuilder* r);
void RNNBuilder_new_graph(CRNNBuilder* r, CComputationGraph* cg, bool update);
void RNNBuilder_start_new_sequence(CRNNBuilder* r, ExpressionVector* h_0);
void RNNBuilder_set_h(CRNNBuilder* r, CExpression* out, int* prev, ExpressionVector* h_new);
void RNNBuilder_set_s(CRNNBuilder* r, CExpression* out, int* prev, ExpressionVector* s_new);
void RNNBuilder_add_input(CRNNBuilder* r, CExpression* out, CExpression* x);
void RNNBuilder_add_input_prev(CRNNBuilder* r, CExpression* out, int* prev, CExpression* x);
void RNNBuilder_rewind_one_step(CRNNBuilder* r);
int RNNBuilder_get_head(CRNNBuilder* r, int* p);
void RNNBuilder_set_dropout(CRNNBuilder* r, float d);
void RNNBuilder_disable_dropout(CRNNBuilder* r);
void RNNBuilder_back(CRNNBuilder* r, CExpression* out);
void RNNBuilder_final_h(CRNNBuilder* r, ExpressionVector* out);
void RNNBuilder_get_h(CRNNBuilder* r, ExpressionVector* out, int i);
void RNNBuilder_final_s(CRNNBuilder* r, ExpressionVector* out);
void RNNBuilder_get_s(CRNNBuilder* r, ExpressionVector* out, int i);
unsigned RNNBuilder_num_h0_components(CRNNBuilder* r);
void RNNBuilder_copy(CRNNBuilder* r, CRNNBuilder* params);
// void RNNBuilder_get_parameter_collection(CRNNBuilder* r, CParameterCollection* out);



CSimpleRNNBuilder* new_SimpleRNNBuilder(unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m, bool support_lags);
void init_SimpleRNNBuilder(CSimpleRNNBuilder* rnn, unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m, bool support_lags);
void delete_SimpleRNNBuilder(CSimpleRNNBuilder* m);
unsigned size_of_SimpleRNNBuilder();
void SimpleRNNBuilder_add_auxiliary_input(CSimpleRNNBuilder* m, CExpression* out,  CExpression* x, CExpression* aux);



CVanillaLSTMBuilder* new_VanillaLSTMBuilder(unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m, bool ln_lstm);
void init_VanillaLSTMBuilder(CVanillaLSTMBuilder* rnn, unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m, bool ln_lstm);
void delete_VanillaLSTMBuilder(CVanillaLSTMBuilder* m);
unsigned size_of_VanillaLSTMBuilder();
void VanillaLSTMBuilder_set_dropout(CVanillaLSTMBuilder* rnn, float d);
void VanillaLSTMBuilder_set_dropout_rate(CVanillaLSTMBuilder* rnn, float d, float d_r);
void VanillaLSTMBuilder_disable_dropout(CVanillaLSTMBuilder* rnn);
void VanillaLSTMBuilder_set_dropout_masks(CVanillaLSTMBuilder* rnn, unsigned batch_size);


CCoupledLSTMBuilder* new_CoupledLSTMBuilder(unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m);
void init_CoupledLSTMBuilder(CCoupledLSTMBuilder* rnn, unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m);
void delete_CoupledLSTMBuilder(CCoupledLSTMBuilder* m);
unsigned size_of_CoupledLSTMBuilder();
void CoupledLSTMBuilder_set_dropout(CCoupledLSTMBuilder* rnn, float d);
void CoupledLSTMBuilder_set_dropout_rate(CCoupledLSTMBuilder* rnn, float d, float d_r, float d_c);
void CoupledLSTMBuilder_disable_dropout(CCoupledLSTMBuilder* rnn);
void CoupledLSTMBuilder_set_dropout_masks(CCoupledLSTMBuilder* rnn, unsigned batch_size);



CFastLSTMBuilder* new_FastLSTMBuilder(unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m);
void init_FastLSTMBuilder(CFastLSTMBuilder* rnn, unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m);
void delete_FastLSTMBuilder(CFastLSTMBuilder* m);
unsigned size_of_FastLSTMBuilder();

CGRUBuilder* new_GRUBuilder(unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m);
void init_GRUBuilder(CGRUBuilder* rnn, unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m);
void delete_GRUBuilder(CGRUBuilder* m);
unsigned size_of_GRUBuilder();



// CTreeLSTMBuilder* new_TreeLSTMBuilder(unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m);
// void init_TreeLSTMBuilder(CTreeLSTMBuilder* rnn, unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m);
// void delete_TreeLSTMBuilder(CTreeLSTMBuilder* m);
// unsigned size_of_TreeLSTMBuilder();

// void add_input(CTreeLSTMBuilder* rnn, CExpression* out, int id, IntVector* children, CExpression* x);
//
// CNaryTreeLSTMBuilder* new_NaryTreeLSTMBuilder(unsigned N, unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m);
// void init_NaryTreeLSTMBuilder(CNaryTreeLSTMBuilder* rnn, unsigned N, unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m);
// void delete_NaryTreeLSTMBuilder(CNaryTreeLSTMBuilder* m);
// unsigned size_of_NaryTreeLSTMBuilder();
//
//
// CUnidirectionalTreeLSTMBuilder* new_UnidirectionalTreeLSTMBuilder(unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m);
// void init_UnidirectionalTreeLSTMBuilder(CUnidirectionalTreeLSTMBuilder* rnn, unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m);
// void delete_UnidirectionalTreeLSTMBuilder(CUnidirectionalTreeLSTMBuilder* m);
// unsigned size_of_UnidirectionalTreeLSTMBuilder();
//
//
// CBidirectionalTreeLSTMBuilder* new_BidirectionalTreeLSTMBuilder(unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m);
// void init_BidirectionalTreeLSTMBuilder(CBidirectionalTreeLSTMBuilder* rnn, unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m);
// void delete_BidirectionalTreeLSTMBuilder(CBidirectionalTreeLSTMBuilder* m);
// unsigned size_of_BidirectionalTreeLSTMBuilder();


// CDeepLSTMBuilder* new_DeepLSTMBuilder(unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m);
// void init_DeepLSTMBuilder(CDeepLSTMBuilder* rnn, unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m);
// void delete_DeepLSTMBuilder(CDeepLSTMBuilder* m);
// unsigned size_of_DeepLSTMBuilder();

#ifdef __cplusplus
}
#endif

#endif
