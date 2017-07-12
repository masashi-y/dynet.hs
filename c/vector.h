
#ifdef __cplusplus
#include <vector>
#endif

#ifdef __cplusplus
typedef std::vector<int> IntVector;
typedef std::vector<long> LongVector;
typedef std::vector<float> FloatVector;
typedef std::vector<unsigned> UIntVector;
#else
struct IntVector;
typedef struct IntVector IntVector;
struct LongVector;
typedef struct LongVector LongVector;
struct FloatVector;
typedef struct FloatVector FloatVector;
struct UIntVector;
typedef struct UIntVector UIntVector;
#endif

#ifdef __cplusplus
#include "dynet/expr.h"
typedef std::vector<dynet::Expression> ExpressionVector;
typedef dynet::Expression CExpression;
#else
struct CExpression;
typedef struct CExpression CExpression;
struct ExpressionVector;
typedef struct ExpressionVector ExpressionVector;
#endif

#define declare_vector_instance(VECTOR_NAME, TYPE)                              \
    int size_of_##VECTOR_NAME();                                                \
    VECTOR_NAME* new_##VECTOR_NAME##_int(int size);                             \
    VECTOR_NAME* new_##VECTOR_NAME##_intp_intp(TYPE* from, TYPE* to);           \
    VECTOR_NAME* new_##VECTOR_NAME##_intp_int(TYPE* from, int size);            \
    void init_##VECTOR_NAME##_int(VECTOR_NAME* u, int size);                    \
    void init_##VECTOR_NAME##_intp_intp(VECTOR_NAME* u, TYPE* from, TYPE* to);  \
    void init_##VECTOR_NAME##_intp_int(VECTOR_NAME*, TYPE* from, int size);     \
    void delete_##VECTOR_NAME(VECTOR_NAME* v);                                  \
    int VECTOR_NAME##_size(VECTOR_NAME* v);                                     \
    TYPE VECTOR_NAME##_get(VECTOR_NAME* v, int i);                              \
    void VECTOR_NAME##_set(VECTOR_NAME* v, int i, TYPE val);                    \
    void VECTOR_NAME##_push_back(VECTOR_NAME* v, TYPE i);                       \
    void VECTOR_NAME##_show(VECTOR_NAME* v);                                    \
    TYPE* VECTOR_NAME##_copy(VECTOR_NAME* v, TYPE* out);

#ifdef __cplusplus
extern "C" {
#endif

declare_vector_instance(IntVector, int);
declare_vector_instance(LongVector, long);
declare_vector_instance(FloatVector, float);
declare_vector_instance(UIntVector, unsigned);


int size_of_ExpressionVector();
ExpressionVector* new_ExpressionVector_int(int size);
ExpressionVector* new_ExpressionVector_intp_intp(CExpression* from, CExpression* to);
ExpressionVector* new_ExpressionVector_intp_int(CExpression* from, int size);
void init_ExpressionVector_int(ExpressionVector* u, int size);
void init_ExpressionVector_intp_intp(ExpressionVector* u, CExpression* from, CExpression* to);
void init_ExpressionVector_intp_int(ExpressionVector*, CExpression** from, int size);
void delete_ExpressionVector(ExpressionVector* v);
int ExpressionVector_size(ExpressionVector* v);
CExpression* ExpressionVector_get(ExpressionVector* v, int i);
void ExpressionVector_set(ExpressionVector* v, int i, CExpression* val);
void ExpressionVector_push_back(ExpressionVector* v, CExpression* i);
void ExpressionVector_show(ExpressionVector* v);
CExpression** ExpressionVector_copy(ExpressionVector* v, CExpression** out);

#ifdef __cplusplus
}
#endif
