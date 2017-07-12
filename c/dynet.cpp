
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

#include "dynet.h"

void doNothing(void *) {}  // dummy deleter
void dynet_initialize(int c, char** arg, bool shared_parameters) { initialize(c, arg, shared_parameters); }

CDim* new_Dim_v(LongVector* ds) { return reinterpret_cast<CDim*>(new Dim(*ds)); }
CDim* new_Dim_v_int(LongVector* ds, int bs) { return reinterpret_cast<CDim*>(new Dim(*ds)); }
void init_Dim_v(CDim* d, LongVector* ds) { (new (d) Dim(*ds)); }
void init_Dim_v_int(CDim* d, LongVector* ds, int bs) { new (d) Dim(*ds, bs); }
unsigned size_of_Dim() { return sizeof(Dim); }
int Dim_size(CDim* d) { return reinterpret_cast<Dim*>(d)->size(); }
// CDim* Dim_truncate(CDim* d) { return reinterpret_cast<Dim*>(d)->truncate(); }
void Dim_resize(CDim* d, unsigned i) { return reinterpret_cast<Dim*>(d)->resize(i); }
int Dim_ndims(CDim* d) { return reinterpret_cast<Dim*>(d)->ndims(); }
int Dim_rows(CDim* d) { return reinterpret_cast<Dim*>(d)->rows(); }
int Dim_cols(CDim* d) { return reinterpret_cast<Dim*>(d)->cols(); }
unsigned Dim_at(CDim* d, unsigned i) { return (*reinterpret_cast<Dim*>(d))[i]; }
void Dim_set(CDim* d, unsigned i, unsigned s) { return reinterpret_cast<Dim*>(d)->set(i, s); }
void delete_Dim(CDim* d) { delete reinterpret_cast<Dim*>(d); }
// int Dim_size(CDim* d, unsigned i) { return reinterpret_cast<Dim*>(d)->size(i); }
// CDim* Dim_transpose(CDim* d) { return reinterpret_cast<Dim*>(d)->transpose(); }

unsigned size_of_Model() { return sizeof(ParameterCollection); }
unsigned size_of_Parameter() { return sizeof(Parameter); }
void delete_Parameter(CParameter* p) { delete reinterpret_cast<Parameter*>(p); }
unsigned size_of_LookupParameter() { return sizeof(LookupParameter); }
void delete_LookupParameter(CLookupParameter* p) { delete reinterpret_cast<LookupParameter*>(p); }

CModel* new_Model() { return reinterpret_cast<CModel*>(new ParameterCollection()); }
void init_Model(CModel* m) { new (m) ParameterCollection(); }
void delete_Model(CModel* m) { delete reinterpret_cast<ParameterCollection*>(m); }
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

CParameterInit* new_CParameterInitNormal(float m, float v) { return reinterpret_cast<CParameterInit*>(new ParameterInitNormal(m, v)); }
void init_CParameterInitNormal(CParameterInit* p, float m, float v) { new (p) ParameterInitNormal(m, v); }

CExpression* new_Expression() { return reinterpret_cast<CExpression*>(new Expression()); }
unsigned size_of_Expression() { return sizeof(Expression); }
void delete_Expression(CExpression* x) { delete reinterpret_cast<Expression*>(x); }

unsigned size_of_Tensor() { return sizeof(Tensor); }
void delete_Tensor(CTensor* t) { delete reinterpret_cast<Tensor*>(t); }

CComputationGraph* new_ComputationGraph() { return reinterpret_cast<CComputationGraph*>(new ComputationGraph()); }
void init_ComputationGraph(CComputationGraph* g) { new (g) ComputationGraph(); }
unsigned size_of_ComputationGraph() { return sizeof(ComputationGraph); }
void delete_ComputationGraph(CComputationGraph* g) { delete reinterpret_cast<ComputationGraph*>(g); }
void ComputationGraph_print_graphviz(CComputationGraph* g) { reinterpret_cast<ComputationGraph*>(g)->print_graphviz(); }
const CTensor* ComputationGraph_forward(CComputationGraph* g, CExpression* expr) {
    const Tensor* res = &reinterpret_cast<ComputationGraph*>(g)->forward(*reinterpret_cast<Expression*>(expr));
    return reinterpret_cast<const CTensor*>(res);
}
void ComputationGraph_backward(CComputationGraph* g, CExpression* expr) {
    reinterpret_cast<ComputationGraph*>(g)->backward(*reinterpret_cast<Expression*>(expr));
}

float c_as_scalar(const CTensor* t) { return as_scalar(*reinterpret_cast<const Tensor*>(t)); }
void Trainer_update(CTrainer* t, float s) { reinterpret_cast<Trainer*>(t)->update(s); }

CTrainer* new_SimpleSGDTrainer(CModel* m, float e0, float edecay) { return reinterpret_cast<CTrainer*>(new SimpleSGDTrainer(*reinterpret_cast<Model*>(m), e0, edecay)); }
void init_SimpleSGDTrainer(CTrainer* t, CModel* m, float e0, float edecay) { new (t) SimpleSGDTrainer(*reinterpret_cast<Model*>(m), e0, edecay); }
unsigned size_of_SimpleSGDTrainer() { return sizeof(SimpleSGDTrainer); }
void delete_SimpleSGDTrainer(CTrainer* t) { delete reinterpret_cast<SimpleSGDTrainer*>(t); }
unsigned size_of_Trainer() { return sizeof(SimpleSGDTrainer); }
void delete_Trainer(CTrainer* t) { delete reinterpret_cast<SimpleSGDTrainer*>(t); }
