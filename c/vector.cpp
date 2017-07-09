#include <iostream>
#include "vector.h"

#define implement_vector_instance(VECTOR_NAME, TYPE)                                                                     \
    int size_of_##VECTOR_NAME() { return sizeof(std::vector<TYPE>); }                                                    \
    VECTOR_NAME* new_##VECTOR_NAME##_int(int size) { return new std::vector<TYPE>(size); }                               \
    VECTOR_NAME* new_##VECTOR_NAME##_int_int(int size, int v) { return new std::vector<TYPE>(size, v); }                 \
    VECTOR_NAME* new_##VECTOR_NAME##_intp_intp(TYPE* from, TYPE* to) { return new std::vector<TYPE>(from, to); }         \
    VECTOR_NAME* new_##VECTOR_NAME##_intp_int(TYPE* from, int size) { return new std::vector<TYPE>(from, from + size); } \
    void init_##VECTOR_NAME##_int(VECTOR_NAME* u, int size) { new(u) std::vector<TYPE>(size); }                          \
    void init_##VECTOR_NAME##_int_int(VECTOR_NAME* u, int size, int v) { new (u) std::vector<TYPE>(size, v); }           \
    void init_##VECTOR_NAME##_intp_intp(VECTOR_NAME* u, TYPE* from, TYPE* to) { new (u) std::vector<TYPE>(from, to); }   \
    void VECTOR_NAME##_delete(VECTOR_NAME* v) { delete v; }                                                              \
    int VECTOR_NAME##_size(VECTOR_NAME* v) { return v->size(); }                                                         \
    TYPE VECTOR_NAME##_get(VECTOR_NAME* v, int i) { return (*v)[i]; }                                                    \
    void VECTOR_NAME##_set(VECTOR_NAME* v, int i, TYPE val) { (*v)[i] = val; }                                           \
    void VECTOR_NAME##_push_back(VECTOR_NAME* v, TYPE i) { v->push_back(i); }                                            \
    void VECTOR_NAME##_show(VECTOR_NAME* v) {                                                                            \
      std::cerr << "[";                                                                                                  \
       for (unsigned i = 0; i < v->size(); i++) { if (i > 0) std::cerr << ", "; std::cerr << (*v)[i]; }                  \
      std::cerr << "]" << std::endl;                                                                                     \
    }                                                                                                                    \
    void VECTOR_NAME##_copy(VECTOR_NAME* v, TYPE* out) { std::copy(v->begin(), v->end(), out); }

implement_vector_instance(IntVector, int);
implement_vector_instance(LongVector, long);
implement_vector_instance(FloatVector, float);
implement_vector_instance(UIntVector, unsigned);

// int size_of_ExpressionVector() { return sizeof(std::vector<dynet::Expression>); }

// ExpressionVector* new_ExpressionVector_int(int size) { return new std::vector<dynet::Expression>(size); }
// 
// ExpressionVector* new_ExpressionVector_int_int(int size, int v) { return new std::vector<dynet::Expression>(size, v); }
// 
// ExpressionVector* new_ExpressionVector_intp_intp(dynet::Expression* from, dynet::Expression* to) { return new std::vector<dynet::Expression>(from, to); }
// 
// ExpressionVector* new_ExpressionVector_intp_int(dynet::Expression* from, int size) { return new std::vector<dynet::Expression>(from, from + size); }
// 
// void init_ExpressionVector_int(ExpressionVector* u, int size) { new(u) std::vector<dynet::Expression>(size); }
// 
// void init_ExpressionVector_int_int(ExpressionVector* u, int size, int v) { new (u) std::vector<dynet::Expression>(size, v); }
// 
// void init_ExpressionVector_intp_intp(ExpressionVector* u, dynet::Expression* from, dynet::Expression* to) { new (u) std::vector<dynet::Expression>(from, to); }
// 
// void ExpressionVector_delete(ExpressionVector* v) { delete v; }
// int ExpressionVector_size(ExpressionVector* v) { return v->size(); }
// dynet::Expression ExpressionVector_get(ExpressionVector* v, int i) { return (*v)[i]; }
// void ExpressionVector_set(ExpressionVector* v, int i, dynet::Expression val) { (*v)[i] = val; }
// void ExpressionVector_push_back(ExpressionVector* v, dynet::Expression i) { v->push_back(i); }
// void ExpressionVector_show(ExpressionVector* v) {
//     std::cerr << "[";
//     for (unsigned i = 0; i < v->size(); i++) { if (i > 0) std::cerr << ", "; std::cerr << (*v)[i]; }
//     std::cerr << "]" << std::endl;
// }
// void ExpressionVector_copy(ExpressionVector* v, dynet::Expression* out) { std::copy(v->begin(), v->end(), out); }
