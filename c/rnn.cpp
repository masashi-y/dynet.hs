
#include <vector>
#include <iostream>
#include "rnn.h"

#include "dynet/rnn.h"
#include "dynet/lstm.h"
#include "dynet/expr.h"

using namespace dynet;
using namespace std;

CSimpleRNNBuilder* new_CSimpleRNNBuilder(unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m, bool support_lags) {
    return reinterpret_cast<CSimpleRNNBuilder*>(
            new SimpleRNNBuilder(
                layers, wembed_dim, hidden_dim, *reinterpret_cast<ParameterCollection*>(m), support_lags));
}

void init_CSimpleRNNBuilder(CSimpleRNNBuilder* rnn, unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m, bool support_lags) {
    (new (rnn) SimpleRNNBuilder(layers, wembed_dim, hidden_dim, *reinterpret_cast<ParameterCollection*>(m), support_lags));
}

void delete_CSimpleRNNBuilder(CSimpleRNNBuilder* m) {
    delete reinterpret_cast<SimpleRNNBuilder*>(m);
}

unsigned size_of_SimpleRNNBuilder() {
    return sizeof(SimpleRNNBuilder);
}

void simpleRNN_new_graph(CSimpleRNNBuilder* rnn, CComputationGraph* g, bool update) {
    reinterpret_cast<SimpleRNNBuilder*>(rnn)->
        new_graph(*reinterpret_cast<ComputationGraph*>(g), update);
}

void simpleRNN_start_new_sequence(CSimpleRNNBuilder* rnn, ExpressionVector* h0) {
    reinterpret_cast<SimpleRNNBuilder*>(rnn)->
        start_new_sequence(*reinterpret_cast<std::vector<Expression>*>(h0));
}

void simpleRNN_add_input(CSimpleRNNBuilder* rnn, CExpression* out, CExpression* x) {
    Expression res = reinterpret_cast<SimpleRNNBuilder*>(rnn)->
        add_input(*reinterpret_cast<Expression*>(x));
    *reinterpret_cast<Expression*>(out) = res;
}





CVanillaLSTMBuilder* new_CVanillaLSTMBuilder(
        unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m, bool ln_lstm) {
    return reinterpret_cast<CVanillaLSTMBuilder*>(
            new VanillaLSTMBuilder(layers, wembed_dim, hidden_dim, *reinterpret_cast<ParameterCollection*>(m), ln_lstm));
}

void init_CVanillaLSTMBuilder(CVanillaLSTMBuilder* rnn, unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m, bool ln_lstm) {
    (new (rnn) VanillaLSTMBuilder(layers, wembed_dim, hidden_dim, *reinterpret_cast<ParameterCollection*>(m), ln_lstm));
}

void delete_CVanillaLSTMBuilder(CVanillaLSTMBuilder* m) {
    delete reinterpret_cast<VanillaLSTMBuilder*>(m);
}

unsigned size_of_VanillaLSTMBuilder() {
    return sizeof(VanillaLSTMBuilder);
}

void vanillaLSTM_new_graph(CVanillaLSTMBuilder* rnn, CComputationGraph* g, bool update) {
    reinterpret_cast<VanillaLSTMBuilder*>(rnn)->
        new_graph(*reinterpret_cast<ComputationGraph*>(g), update);
}

void vanillaLSTM_start_new_sequence(CVanillaLSTMBuilder* rnn, ExpressionVector* h0) {
    reinterpret_cast<VanillaLSTMBuilder*>(rnn)->
        start_new_sequence(*reinterpret_cast<std::vector<Expression>*>(h0));
}

void vanillaLSTM_add_input(CVanillaLSTMBuilder* rnn, CExpression* out, CExpression* x) {
    Expression res = reinterpret_cast<VanillaLSTMBuilder*>(rnn)->
        add_input(*reinterpret_cast<Expression*>(x));
    *reinterpret_cast<Expression*>(out) = res;
}

