
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
namespace dynet { struct Expression; }
typedef std::vector<dynet::Expression> ExpressionVector;
#else
struct ExpressionVector;
typedef struct ExpressionVector ExpressionVector;
#endif

#define declare_vector_instance(VECTOR_NAME, TYPE)                              \
    int size_of_##VECTOR_NAME();                                                \
    VECTOR_NAME* new_##VECTOR_NAME##_int(int size);                             \
    VECTOR_NAME* new_##VECTOR_NAME##_int_int(int size, int v);                  \
    VECTOR_NAME* new_##VECTOR_NAME##_intp_intp(TYPE* from, TYPE* to);           \
    VECTOR_NAME* new_##VECTOR_NAME##_intp_int(TYPE* from, int size);            \
    void init_##VECTOR_NAME##_int(VECTOR_NAME* u, int size);                    \
    void init_##VECTOR_NAME##_int_int(VECTOR_NAME* u, int size, int v);         \
    void init_##VECTOR_NAME##_intp_intp(VECTOR_NAME* u, TYPE* from, TYPE* to);  \
    void VECTOR_NAME##_delete(VECTOR_NAME* v);                                  \
    int VECTOR_NAME##_size(VECTOR_NAME* v);                                     \
    TYPE VECTOR_NAME##_get(VECTOR_NAME* v, int i);                              \
    void VECTOR_NAME##_set(VECTOR_NAME* v, int i, TYPE val);                    \
    void VECTOR_NAME##_push_back(VECTOR_NAME* v, TYPE i);                       \
    void VECTOR_NAME##_show(VECTOR_NAME* v);                                    \
    void VECTOR_NAME##_copy(VECTOR_NAME* v, TYPE* out);

#ifdef __cplusplus
extern "C" {
#endif

declare_vector_instance(IntVector, int);
declare_vector_instance(LongVector, long);
declare_vector_instance(FloatVector, float);
declare_vector_instance(UIntVector, unsigned);
// declare_vector_instance(UIntVector, Expression);

#ifdef __cplusplus
}
#endif
