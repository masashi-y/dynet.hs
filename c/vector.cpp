#include <iostream>
#include "vector.h"

#define implement_vector_instance(VECTOR_NAME, TYPE)                                                                          \
    int size_of_##VECTOR_NAME() { return sizeof(std::vector<TYPE>); }                                                         \
    VECTOR_NAME* new_##VECTOR_NAME##_int(int size) { return new std::vector<TYPE>(size); }                                    \
    VECTOR_NAME* new_##VECTOR_NAME##_intp_intp(TYPE* from, TYPE* to) { return new std::vector<TYPE>(from, to); }              \
    VECTOR_NAME* new_##VECTOR_NAME##_intp_int(TYPE* from, int size) { return new std::vector<TYPE>(from, from + size); }      \
    void init_##VECTOR_NAME##_int(VECTOR_NAME* u, int size) { new(u) std::vector<TYPE>(size); }                               \
    void init_##VECTOR_NAME##_intp_intp(VECTOR_NAME* u, TYPE* from, TYPE* to) { new (u) std::vector<TYPE>(from, to); }        \
    void init_##VECTOR_NAME##_intp_int(VECTOR_NAME* v, TYPE* from, int size) { new(v) std::vector<TYPE>(from, from + size); } \
    void delete_##VECTOR_NAME(VECTOR_NAME* v) { delete v; }                                                                   \
    int VECTOR_NAME##_size(VECTOR_NAME* v) { return v->size(); }                                                              \
    TYPE VECTOR_NAME##_get(VECTOR_NAME* v, int i) { return (*v)[i]; }                                                         \
    void VECTOR_NAME##_set(VECTOR_NAME* v, int i, TYPE val) { (*v)[i] = val; }                                                \
    void VECTOR_NAME##_push_back(VECTOR_NAME* v, TYPE i) { v->push_back(i); }                                                 \
    void VECTOR_NAME##_show(VECTOR_NAME* v) {                                                                                 \
      std::cerr << "[";                                                                                                       \
       for (unsigned i = 0; i < v->size(); i++) { if (i > 0) std::cerr << ", "; std::cerr << (*v)[i]; }                       \
      std::cerr << "]" << std::endl;                                                                                          \
    }                                                                                                                         \
    TYPE* VECTOR_NAME##_copy(VECTOR_NAME* v, TYPE* out) { std::copy(v->begin(), v->end(), out); return out; }

implement_vector_instance(IntVector, int);
implement_vector_instance(LongVector, long);
implement_vector_instance(FloatVector, float);
implement_vector_instance(UIntVector, unsigned);

int size_of_ExpressionVector() { return sizeof(std::vector<CExpression>); }

ExpressionVector* new_ExpressionVector_int(int size) { return new std::vector<CExpression>(size); }

ExpressionVector* new_ExpressionVector_intp_intp(CExpression* from, CExpression* to) {
    std::cout << sizeof(from) << std::endl;
    return new std::vector<CExpression>(from, to); }

ExpressionVector* new_ExpressionVector_intp_int(CExpression* from, int size) {
    return new std::vector<CExpression>(from, from + size); }

void init_ExpressionVector_int(ExpressionVector* u, int size) { new(u) std::vector<CExpression>(size); }

void init_ExpressionVector_intp_intp(ExpressionVector* u, CExpression* from, CExpression* to) { new (u) std::vector<CExpression>(from, to); }

void init_ExpressionVector_intp_int(ExpressionVector* u, CExpression** from, int size) {
    new (u) std::vector<CExpression>(size);
    for (int i=0; i < size; i++)
        (*u)[i] = *(from[i]);
}

void delete_ExpressionVector(ExpressionVector* v) { delete v; }

int ExpressionVector_size(ExpressionVector* v) { return v->size(); }

CExpression* ExpressionVector_get(ExpressionVector* v, int i) { return &(*v)[i]; }

void ExpressionVector_set(ExpressionVector* v, int i, CExpression* val) { (*v)[i] = *val; }

void ExpressionVector_push_back(ExpressionVector* v, CExpression* i) { v->push_back(*i); }

void ExpressionVector_show(ExpressionVector* v) {
    std::cerr << "[";
    for (unsigned i = 0; i < v->size(); i++) { if (i > 0) std::cerr << ", "; std::cerr << &(*v)[i]; }
    std::cerr << "]" << std::endl;
}

CExpression** ExpressionVector_copy(ExpressionVector* v, CExpression** out) {
    for (auto itr = v->begin(); itr != v->end(); ++itr)
        *out++ = new CExpression(*itr);
    return out;
}
