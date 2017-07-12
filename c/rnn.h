
#ifndef __INCUDE_C_RNN_H
#define __INCUDE_C_RNN_H

#include "dynet.h"

struct CSimpleRNNBuilder;
typedef struct CSimpleRNNBuilder CSimpleRNNBuilder;

struct CVanillaLSTMBuilder;
typedef struct CVanillaLSTMBuilder CVanillaLSTMBuilder;


#ifdef __cplusplus
extern "C" {
#endif

CSimpleRNNBuilder* new_CSimpleRNNBuilder(unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m, bool support_lags);
void init_CSimpleRNNBuilder(CSimpleRNNBuilder* rnn, unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m, bool support_lags);
void delete_CSimpleRNNBuilder(CSimpleRNNBuilder* m);
unsigned size_of_SimpleRNNBuilder();
void simpleRNN_new_graph(CSimpleRNNBuilder* rnn, CComputationGraph* g, bool update);
void simpleRNN_start_new_sequence(CSimpleRNNBuilder* rnn, ExpressionVector* h0);
void simpleRNN_add_input(CSimpleRNNBuilder* rnn, CExpression* out, CExpression* x);


CVanillaLSTMBuilder* new_CVanillaLSTMBuilder(unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m, bool ln_lstm);
void init_CVanillaLSTMBuilder(CVanillaLSTMBuilder* rnn, unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m, bool ln_lstm);
void delete_CVanillaLSTMBuilder(CVanillaLSTMBuilder* m);
unsigned size_of_VanillaLSTMBuilder();
void vanillaLSTM_new_graph(CVanillaLSTMBuilder* rnn, CComputationGraph* g, bool update);
void vanillaLSTM_start_new_sequence(CVanillaLSTMBuilder* rnn, ExpressionVector* h0);
void vanillaLSTM_add_input(CVanillaLSTMBuilder* rnn, CExpression* out, CExpression* x);

#ifdef __cplusplus
}
#endif

#endif
