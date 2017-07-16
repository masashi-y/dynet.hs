
#include <vector>
#include <iostream>
#include "rnn.h"

#include "dynet/rnn.h"
#include "dynet/lstm.h"
#include "dynet/fast-lstm.h"
#include "dynet/gru.h"
#include "dynet/expr.h"

using namespace dynet;
using namespace std;


int state(CRNNBuilder* r) {
    return reinterpret_cast<RNNBuilder*>(r)->state();
}

void RNNBuilder_new_graph(CRNNBuilder* r, CComputationGraph* cg, bool update) {
    reinterpret_cast<RNNBuilder*>(r)->new_graph(*reinterpret_cast<ComputationGraph*>(cg), update);
}

void RNNBuilder_start_new_sequence(CRNNBuilder* r, ExpressionVector* h_0) {
    reinterpret_cast<RNNBuilder*>(r)->start_new_sequence(*reinterpret_cast<std::vector<Expression>*>(h_0));
}

void RNNBuilder_set_h(CRNNBuilder* r, CExpression* out, int* prev, ExpressionVector* h_new) {
    *reinterpret_cast<Expression*>(out) =
        reinterpret_cast<RNNBuilder*>(r)->set_h(*prev, *reinterpret_cast<std::vector<Expression>*>(h_new));
}

void RNNBuilder_set_s(CRNNBuilder* r, CExpression* out, int* prev, ExpressionVector* s_new) {
    *reinterpret_cast<Expression*>(out) =
        reinterpret_cast<RNNBuilder*>(r)->set_s(*prev, *reinterpret_cast<std::vector<Expression>*>(s_new));
}

void RNNBuilder_add_input(CRNNBuilder* r, CExpression* out, CExpression* x) {
    *reinterpret_cast<Expression*>(out) =
        reinterpret_cast<RNNBuilder*>(r)->add_input(*reinterpret_cast<Expression*>(x));
}

void RNNBuilder_add_input_prev(CRNNBuilder* r, CExpression* out, int* prev, CExpression* x) {
    *reinterpret_cast<Expression*>(out) =
        reinterpret_cast<RNNBuilder*>(r)->add_input(*prev, *reinterpret_cast<Expression*>(x));
}

void RNNBuilder_rewind_one_step(CRNNBuilder* r) {
    reinterpret_cast<RNNBuilder*>(r)->rewind_one_step();
}

int RNNBuilder_get_head(CRNNBuilder* r, int* p) {
    return reinterpret_cast<RNNBuilder*>(r)->get_head(*p);
}

void RNNBuilder_set_dropout(CRNNBuilder* r, float d) {
    reinterpret_cast<RNNBuilder*>(r)->set_dropout(d);
}

void RNNBuilder_disable_dropout(CRNNBuilder* r) {
    reinterpret_cast<RNNBuilder*>(r)->disable_dropout();
}

void RNNBuilder_back(CRNNBuilder* r, CExpression* out) {
    *reinterpret_cast<Expression*>(out) =
        reinterpret_cast<RNNBuilder*>(r)->back();
}

void RNNBuilder_final_h(CRNNBuilder* r, ExpressionVector* out) {
    *reinterpret_cast<std::vector<Expression>*>(out) =
        reinterpret_cast<RNNBuilder*>(r)->final_h();
}

void RNNBuilder_get_h(CRNNBuilder* r, ExpressionVector* out, int i) {
    *reinterpret_cast<std::vector<Expression>*>(out) =
        reinterpret_cast<RNNBuilder*>(r)->get_h(i);
}

void RNNBuilder_final_s(CRNNBuilder* r, ExpressionVector* out) {
    *reinterpret_cast<std::vector<Expression>*>(out) =
        reinterpret_cast<RNNBuilder*>(r)->final_s();
}

void RNNBuilder_get_s(CRNNBuilder* r, ExpressionVector* out, int i) {
    *reinterpret_cast<std::vector<Expression>*>(out) =
        reinterpret_cast<RNNBuilder*>(r)->get_s(i);
}

unsigned RNNBuilder_num_h0_components(CRNNBuilder* r) {
    return reinterpret_cast<RNNBuilder*>(r)->num_h0_components();
}

void RNNBuilder_copy(CRNNBuilder* r, CRNNBuilder* params) {
    reinterpret_cast<RNNBuilder*>(r)->copy(*reinterpret_cast<RNNBuilder*>(params));
}

// returns reference
// void RNNBuilder_get_parameter_collection(CRNNBuilder* r, CParameterCollection* out) {
//     *reinterpret_cast<ParameterCollection*>(out) =
//         &r->get_parameter_collection();
// }








CSimpleRNNBuilder* new_SimpleRNNBuilder(unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m, bool support_lags) {
    return reinterpret_cast<CSimpleRNNBuilder*>(
            new SimpleRNNBuilder(
                layers, wembed_dim, hidden_dim, *reinterpret_cast<ParameterCollection*>(m), support_lags));
}

void init_SimpleRNNBuilder(CSimpleRNNBuilder* rnn, unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m, bool support_lags) {
    (new (rnn) SimpleRNNBuilder(layers, wembed_dim, hidden_dim, *reinterpret_cast<ParameterCollection*>(m), support_lags));
}

void delete_SimpleRNNBuilder(CSimpleRNNBuilder* m) {
    reinterpret_cast<SimpleRNNBuilder*>(m)->~SimpleRNNBuilder();
}

unsigned size_of_SimpleRNNBuilder() {
    return sizeof(SimpleRNNBuilder);
}


void SimpleRNNBuilder_add_auxiliary_input(CSimpleRNNBuilder* m, Expression* out,  Expression* x, Expression* aux) {
    *reinterpret_cast<Expression*>(out) =
        reinterpret_cast<SimpleRNNBuilder*>(m)->
            add_auxiliary_input(*reinterpret_cast<Expression*>(x), *reinterpret_cast<Expression*>(aux));
}




CVanillaLSTMBuilder* new_VanillaLSTMBuilder(
        unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m, bool ln_lstm) {
    return reinterpret_cast<CVanillaLSTMBuilder*>(
            new VanillaLSTMBuilder(layers, wembed_dim, hidden_dim, *reinterpret_cast<ParameterCollection*>(m), ln_lstm));
}

void init_VanillaLSTMBuilder(CVanillaLSTMBuilder* rnn, unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m, bool ln_lstm) {
    (new (rnn) VanillaLSTMBuilder(layers, wembed_dim, hidden_dim, *reinterpret_cast<ParameterCollection*>(m), ln_lstm));
}

void delete_VanillaLSTMBuilder(CVanillaLSTMBuilder* m) {
    reinterpret_cast<VanillaLSTMBuilder*>(m)->~VanillaLSTMBuilder();
}

unsigned size_of_VanillaLSTMBuilder() {
    return sizeof(VanillaLSTMBuilder);
}

void VanillaLSTMBuilder_set_dropout(CVanillaLSTMBuilder* rnn, float d) {
    reinterpret_cast<VanillaLSTMBuilder*>(rnn)->
        set_dropout(d);
}

void VanillaLSTMBuilder_set_dropout_rate(CVanillaLSTMBuilder* rnn, float d, float d_r) {
    reinterpret_cast<VanillaLSTMBuilder*>(rnn)->
        set_dropout(d, d_r);
}

void VanillaLSTMBuilder_disable_dropout(CVanillaLSTMBuilder* rnn) {
    reinterpret_cast<VanillaLSTMBuilder*>(rnn)->
        disable_dropout();
}

void VanillaLSTMBuilder_set_dropout_masks(CVanillaLSTMBuilder* rnn, unsigned batch_size) {
    reinterpret_cast<VanillaLSTMBuilder*>(rnn)->
        set_dropout_masks(batch_size);
}




CCoupledLSTMBuilder* new_CoupledLSTMBuilder(
        unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m) {
    return reinterpret_cast<CCoupledLSTMBuilder*>(
            new CoupledLSTMBuilder(layers, wembed_dim, hidden_dim, *reinterpret_cast<ParameterCollection*>(m)));
}

void init_CoupledLSTMBuilder(CCoupledLSTMBuilder* rnn, unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m) {
    (new (rnn) CoupledLSTMBuilder(layers, wembed_dim, hidden_dim, *reinterpret_cast<ParameterCollection*>(m)));
}

void delete_CoupledLSTMBuilder(CCoupledLSTMBuilder* m) {
    reinterpret_cast<CoupledLSTMBuilder*>(m)->~CoupledLSTMBuilder();
}

unsigned size_of_CoupledLSTMBuilder() {
    return sizeof(CoupledLSTMBuilder);
}

void CoupledLSTMBuilder_set_dropout(CCoupledLSTMBuilder* rnn, float d) {
    reinterpret_cast<CoupledLSTMBuilder*>(rnn)->
        set_dropout(d);
}

void CoupledLSTMBuilder_set_dropout_rate(CCoupledLSTMBuilder* rnn, float d, float d_r, float d_c) {
    reinterpret_cast<CoupledLSTMBuilder*>(rnn)->
        set_dropout(d, d_r, d_c);
}

void CoupledLSTMBuilder_disable_dropout(CCoupledLSTMBuilder* rnn) {
    reinterpret_cast<CoupledLSTMBuilder*>(rnn)->
        disable_dropout();
}

void CoupledLSTMBuilder_set_dropout_masks(CCoupledLSTMBuilder* rnn, unsigned batch_size) {
    reinterpret_cast<CoupledLSTMBuilder*>(rnn)->
        set_dropout_masks(batch_size);
}



CFastLSTMBuilder* new_FastLSTMBuilder(
        unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m) {
    return reinterpret_cast<CFastLSTMBuilder*>(
            new FastLSTMBuilder(layers, wembed_dim, hidden_dim, *reinterpret_cast<ParameterCollection*>(m)));
}

void init_FastLSTMBuilder(CFastLSTMBuilder* rnn, unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m) {
    (new (rnn) FastLSTMBuilder(layers, wembed_dim, hidden_dim, *reinterpret_cast<ParameterCollection*>(m)));
}

void delete_FastLSTMBuilder(CFastLSTMBuilder* m) {
    reinterpret_cast<FastLSTMBuilder*>(m)->~FastLSTMBuilder();
}

unsigned size_of_FastLSTMBuilder() {
    return sizeof(FastLSTMBuilder);
}



CGRUBuilder* new_GRUBuilder(
        unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m) {
    return reinterpret_cast<CGRUBuilder*>(
            new GRUBuilder(layers, wembed_dim, hidden_dim, *reinterpret_cast<ParameterCollection*>(m)));
}

void init_GRUBuilder(CGRUBuilder* rnn, unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m) {
    (new (rnn) GRUBuilder(layers, wembed_dim, hidden_dim, *reinterpret_cast<ParameterCollection*>(m)));
}

void delete_GRUBuilder(CGRUBuilder* m) {
    reinterpret_cast<GRUBuilder*>(m)->~GRUBuilder();
}

unsigned size_of_GRUBuilder() {
    return sizeof(GRUBuilder);
}




// void add_input(CTreeLSTMBuilder* rnn, CExpression* out, int id, IntVector* children, CExpression* x) {
//     *reinterpret_cast<Expression*>(out) =
//         reinterpret_cast<TreeLSTMBuilder*>(rnn)->
//             add_input(id,
//                     *reinterpret_cast<std::vector<int>*>(children),
//                     *reinterpret_cast<Expression*>(x));
// }
//
//
//
// CNaryTreeLSTMBuilder* new_NaryTreeLSTMBuilder(
//         unsigned N, unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m) {
//     return reinterpret_cast<CNaryTreeLSTMBuilder*>(
//             new NaryTreeLSTMBuilder(N, layers, wembed_dim, hidden_dim, *reinterpret_cast<ParameterCollection*>(m)));
// }
//
// void init_NaryTreeLSTMBuilder(CNaryTreeLSTMBuilder* rnn, unsigned N, unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m) {
//     NaryTreeLSTMBuilder res(N, layers, wembed_dim, hidden_dim, *reinterpret_cast<ParameterCollection*>(m));
//     *reinterpret_cast<NaryTreeLSTMBuilder*>(rnn) = res;
// }
//
// void delete_NaryTreeLSTMBuilder(CNaryTreeLSTMBuilder* m) {
//     reinterpret_cast<NaryTreeLSTMBuilder*>(m)->~NaryTreeLSTMBuilder();
// }
//
// unsigned size_of_NaryTreeLSTMBuilder() {
//     return sizeof(NaryTreeLSTMBuilder);
// }
//
//
//
// CUnidirectionalTreeLSTMBuilder* new_UnidirectionalTreeLSTMBuilder(
//         unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m) {
//     return reinterpret_cast<CUnidirectionalTreeLSTMBuilder*>(
//             new UnidirectionalTreeLSTMBuilder(layers, wembed_dim, hidden_dim, *reinterpret_cast<ParameterCollection*>(m)));
// }
//
// void init_UnidirectionalTreeLSTMBuilder(CUnidirectionalTreeLSTMBuilder* rnn, unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m) {
//     (new (rnn) UnidirectionalTreeLSTMBuilder(layers, wembed_dim, hidden_dim, *reinterpret_cast<ParameterCollection*>(m)));
// }
//
// void delete_UnidirectionalTreeLSTMBuilder(CUnidirectionalTreeLSTMBuilder* m) {
//     reinterpret_cast<UnidirectionalTreeLSTMBuilder*>(m)->~UnidirectionalTreeLSTMBuilder();
// }
//
// unsigned size_of_UnidirectionalTreeLSTMBuilder() {
//     return sizeof(UnidirectionalTreeLSTMBuilder);
// }
//
//
//
// CBidirectionalTreeLSTMBuilder* new_BidirectionalTreeLSTMBuilder(
//         unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m) {
//     return reinterpret_cast<CBidirectionalTreeLSTMBuilder*>(
//             new BidirectionalTreeLSTMBuilder(layers, wembed_dim, hidden_dim, *reinterpret_cast<ParameterCollection*>(m)));
// }
//
// void init_BidirectionalTreeLSTMBuilder(CBidirectionalTreeLSTMBuilder* rnn, unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m) {
//     (new (rnn) BidirectionalTreeLSTMBuilder(layers, wembed_dim, hidden_dim, *reinterpret_cast<ParameterCollection*>(m)));
// }
//
// void delete_BidirectionalTreeLSTMBuilder(CBidirectionalTreeLSTMBuilder* m) {
//     reinterpret_cast<BidirectionalTreeLSTMBuilder*>(m)->~BidirectionalTreeLSTMBuilder();
// }
//
// unsigned size_of_BidirectionalTreeLSTMBuilder() {
//     return sizeof(BidirectionalTreeLSTMBuilder);
// }
// CDeepLSTMBuilder* new_DeepLSTMBuilder(
//         unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m) {
//     return reinterpret_cast<CDeepLSTMBuilder*>(
//             new DeepLSTMBuilder(layers, wembed_dim, hidden_dim, *reinterpret_cast<ParameterCollection*>(m)));
// }
// 
// void init_DeepLSTMBuilder(CDeepLSTMBuilder* rnn, unsigned layers, unsigned wembed_dim, unsigned hidden_dim, CModel* m) {
//     (new (rnn) DeepLSTMBuilder(layers, wembed_dim, hidden_dim, *reinterpret_cast<ParameterCollection*>(m)));
// }
// 
// void delete_DeepLSTMBuilder(CDeepLSTMBuilder* m) {
//     reinterpret_cast<DeepLSTMBuilder*>(m)->~DeepLSTMBuilder();
// }
// 
// unsigned size_of_DeepLSTMBuilder() {
//     return sizeof(DeepLSTMBuilder);
// }
// 
// 
