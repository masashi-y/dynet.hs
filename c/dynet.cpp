
#include <iostream>
#include <string>
#include <string.h>
#include "dynet/dim.h"
#include "dynet/model.h"
#include "dynet/init.h"
#include "dynet/param-init.h"
#include "dynet/tensor.h"
#include "dynet/dynet.h"
#include "dynet/expr.h"
#include "dynet/training.h"

using namespace dynet;
using namespace std;

#include "dynet.h"

void doNothing(void *) {}  // dummy deleter
void dynet_initialize(int* c, char** arg, bool shared_parameters) {
    initialize(*c, arg, shared_parameters);
}

CDim* new_Dim_v(LongVector* ds) {
    return reinterpret_cast<CDim*>(new Dim(*ds));
}

CDim* new_Dim_v_int(LongVector* ds, int bs) {
    return reinterpret_cast<CDim*>(new Dim(*ds));
}

void init_Dim_v(CDim* d, LongVector* ds) {
    (new (d) Dim(*ds));
}

void init_Dim_v_int(CDim* d, LongVector* ds, int bs) {
    new (d) Dim(*ds, bs);
}

unsigned size_of_Dim() {
    return sizeof(Dim);
}

int Dim_size(CDim* d) {
    return reinterpret_cast<Dim*>(d)->size();
}

unsigned int Dim_batch_elems(CDim* d) {
    return reinterpret_cast<Dim*>(d)->batch_elems();
}

int Dim_sum_dims(CDim* d) {
    return reinterpret_cast<Dim*>(d)->sum_dims();
}

void Dim_truncate(CDim* d, CDim* out) {
    *reinterpret_cast<Dim*>(out) = reinterpret_cast<Dim*>(d)->truncate();
}

void Dim_resize(CDim* d, unsigned i) {
    return reinterpret_cast<Dim*>(d)->resize(i);
}

int Dim_ndims(CDim* d) {
    return reinterpret_cast<Dim*>(d)->ndims();
}

int Dim_rows(CDim* d) {
    return reinterpret_cast<Dim*>(d)->rows();
}

int Dim_cols(CDim* d) {
    return reinterpret_cast<Dim*>(d)->cols();
}

unsigned Dim_at(CDim* d, unsigned i) {
    return (*reinterpret_cast<Dim*>(d))[i];
}

void Dim_set(CDim* d, unsigned i, unsigned s) {
    return reinterpret_cast<Dim*>(d)->set(i, s);
}

void delete_Dim(CDim* d) {
    reinterpret_cast<Dim*>(d)->~Dim();
}

void Dim_transpose(CDim* d, CDim* out) {
    *reinterpret_cast<Dim*>(out) = reinterpret_cast<Dim*>(d)->transpose();
}

void Dim_debug(CDim* d) {
    cerr << *reinterpret_cast<Dim*>(d);
}

unsigned size_of_Model() {
    return sizeof(ParameterCollection);
}

unsigned size_of_Parameter() {
    return sizeof(Parameter);
}

void delete_Parameter(CParameter* p) {
    reinterpret_cast<Parameter*>(p)->~Parameter();
}

unsigned size_of_LookupParameter() {
    return sizeof(LookupParameter);
}

void delete_LookupParameter(CLookupParameter* p) {
    reinterpret_cast<LookupParameter*>(p)->~LookupParameter();
}


CModel* new_Model() {
    return reinterpret_cast<CModel*>(new ParameterCollection());
}

void init_Model(CModel* m) {
    new (m) ParameterCollection();
}

void delete_Model(CModel* m) {
    reinterpret_cast<ParameterCollection*>(m)->~ParameterCollection();
}

void Parameter_get_fullname(CParameter* p, char* out) {
    std::string name = reinterpret_cast<Parameter*>(p)->get_fullname();
    strcpy(out, name.c_str());
}

void Model_add_parameters(CModel* m, CParameter* p, CDim* d) {
   Parameter pp = reinterpret_cast<ParameterCollection*>(m)->add_parameters(*reinterpret_cast<Dim*>(d));
   *reinterpret_cast<Parameter*>(p) = pp;
}

void Model_add_parameters_1(CModel* m, CParameter* p, CDim* d, CParameterInit* initializer, char* name) {
   Parameter pp = reinterpret_cast<ParameterCollection*>(m)->add_parameters(
     *reinterpret_cast<Dim*>(d), *reinterpret_cast<ParameterInit*>(initializer), std::string(name));
   *reinterpret_cast<Parameter*>(p) = pp;
}

void Model_add_lookup_parameters(CModel* m, CLookupParameter* p, unsigned n, CDim* d) {
   LookupParameter pp = reinterpret_cast<ParameterCollection*>(m)->add_lookup_parameters(n, *reinterpret_cast<Dim*>(d));
   *reinterpret_cast<LookupParameter*>(p) = pp;
}

void Model_add_lookup_parameters_1(CModel* m, CLookupParameter* p, unsigned n, CDim* d, CParameterInit* initializer, char* name) {
   LookupParameter pp = reinterpret_cast<ParameterCollection*>(m)->add_lookup_parameters(
     n, *reinterpret_cast<Dim*>(d), *reinterpret_cast<ParameterInit*>(initializer), std::string(name));
   *reinterpret_cast<LookupParameter*>(p) = pp;
}

CParameterInit* new_CParameterInitNormal(float m, float v) {
    return reinterpret_cast<CParameterInit*>(new ParameterInitNormal(m, v));
}

void init_CParameterInitNormal(CParameterInit* p, float m, float v) {
    new (p) ParameterInitNormal(m, v);
}

CExpression* new_Expression() {
    return reinterpret_cast<CExpression*>(new Expression());
}

unsigned size_of_Expression() {
    return sizeof(Expression);
}

void delete_Expression(CExpression* x) {
    cout << "delete" << endl;
    reinterpret_cast<Expression*>(x)->~Expression();
}


unsigned size_of_Tensor() {
    return sizeof(Tensor);
}

void delete_Tensor(CTensor* t) {
    reinterpret_cast<Tensor*>(t)->~Tensor();
}

void Tensor_debug(CTensor* t) {
    cerr << *reinterpret_cast<Tensor*>(t);
}

ComputationGraph* _cg = nullptr;


CComputationGraph* new_ComputationGraph() {
    if (_cg != nullptr)
        delete _cg;
    _cg = new ComputationGraph();
    return reinterpret_cast<CComputationGraph*>(_cg);
}

void init_ComputationGraph(CComputationGraph* g) {
    new (g) ComputationGraph();
}

unsigned size_of_ComputationGraph() {
    return sizeof(ComputationGraph);
}

void delete_ComputationGraph(CComputationGraph* g) {
    reinterpret_cast<ComputationGraph*>(g)->~ComputationGraph();
}

void ComputationGraph_print_graphviz(CComputationGraph* g) {
    reinterpret_cast<ComputationGraph*>(g)->print_graphviz();
}

const CTensor* ComputationGraph_forward(CComputationGraph* g, CExpression* expr) {
    const Tensor* res = &reinterpret_cast<ComputationGraph*>(g)->forward(*reinterpret_cast<Expression*>(expr));
    return reinterpret_cast<const CTensor*>(res);
}
void ComputationGraph_backward(CComputationGraph* g, CExpression* expr) {
    reinterpret_cast<ComputationGraph*>(g)->backward(*reinterpret_cast<Expression*>(expr));
}

float c_as_scalar(const CTensor* t) {
    return as_scalar(*reinterpret_cast<const Tensor*>(t));
}

void c_as_vector(FloatVector* out, const CTensor* t) {
    new (out) std::vector<float>(
        as_vector(*reinterpret_cast<const Tensor*>(t)));
}

const CTensor* ComputationGraph_incremental_forward(CComputationGraph* g, CExpression* last) {
    const Tensor* res = &reinterpret_cast<ComputationGraph*>(g)->
        incremental_forward(*reinterpret_cast<Expression*>(last));
    return reinterpret_cast<const CTensor*>(res);
}
void ComputationGraph_invalidate(CComputationGraph* g) {
    reinterpret_cast<ComputationGraph*>(g)->invalidate();
}

void ComputationGraph_clear(CComputationGraph* g) {
    reinterpret_cast<ComputationGraph*>(g)->clear();
}
void ComputationGraph_checkpoint(CComputationGraph* g) {
    reinterpret_cast<ComputationGraph*>(g)->checkpoint();
}

void ComputationGraph_revert(CComputationGraph* g) {
    reinterpret_cast<ComputationGraph*>(g)->revert();
}

void ComputationGraph_get_dimension(CComputationGraph* g, CDim* out, int index) {
    Dim& res = reinterpret_cast<ComputationGraph*>(g)->get_dimension(index);
    *reinterpret_cast<Dim*>(out) = res;
}

