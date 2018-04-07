
#ifndef __INCUDE_C_TRAINER_H
#define __INCUDE_C_TRAINER_H

#include "dynet.h"

struct CTrainer;
typedef struct CTrainer CTrainer;

struct CSimpleSGDTrainer;
typedef struct CSimpleSGDTrainer CSimpleSGDTrainer;

struct CCyclicalSGDTrainer;
typedef struct CCyclicalSGDTrainer CCyclicalSGDTrainer;

struct CMomentumSGDTrainer;
typedef struct CMomentumSGDTrainer CMomentumSGDTrainer;

struct CAdagradTrainer;
typedef struct CAdagradTrainer CAdagradTrainer;

struct CAdadeltaTrainer;
typedef struct CAdadeltaTrainer CAdadeltaTrainer;

struct CRMSPropTrainer;
typedef struct CRMSPropTrainer CRMSPropTrainer;

struct CAdamTrainer;
typedef struct CAdamTrainer CAdamTrainer;

#ifdef __cplusplus
extern "C" {
#endif


void Trainer_update_subset(CTrainer* t, UIntVector* updated_params, UIntVector* updated_lookup_params, float scale);
void Trainer_update(CTrainer* t);
void Trainer_update_epoch(CTrainer* t, float r);
float Trainer_clip_gradients(CTrainer* t);
void Trainer_rescale_and_reset_weight_decay(CTrainer* t);
void Trainer_status(CTrainer* t);


void init_SimpleSGDTrainer(CSimpleSGDTrainer* t, CModel* m, float learning_rate);
void init_CyclicalSGDTrainer(CCyclicalSGDTrainer* t, CModel* m, float e0_min, float e0_max, float step_size, float gamma, float edecay);
void init_MomentumSGDTrainer(CMomentumSGDTrainer* t, CModel* m, float learning_rate, float mom);
void init_AdagradTrainer(CAdagradTrainer* t, CModel* m, float learning_rate, float eps);
void init_AdadeltaTrainer(CAdadeltaTrainer* t, CModel* m, float eps, float rho);
void init_RMSPropTrainer(CRMSPropTrainer* t, CModel* m, float learning_rate, float eps, float rho);
void init_AdamTrainer(CAdamTrainer* t, CModel* m, float learning_rate, float beta_1, float beta_2, float eps);

unsigned size_of_SimpleSGDTrainer();
unsigned size_of_CyclicalSGDTrainer();
unsigned size_of_MomentumSGDTrainer();
unsigned size_of_AdagradTrainer();
unsigned size_of_AdadeltaTrainer();
unsigned size_of_RMSPropTrainer();
unsigned size_of_AdamTrainer();

void delete_SimpleSGDTrainer(CSimpleSGDTrainer* t);
void delete_CyclicalSGDTrainer(CCyclicalSGDTrainer* t);
void delete_MomentumSGDTrainer(CMomentumSGDTrainer* t);
void delete_AdagradTrainer(CAdagradTrainer* t);
void delete_AdadeltaTrainer(CAdadeltaTrainer* t);
void delete_RMSPropTrainer(CRMSPropTrainer* t);
void delete_AdamTrainer(CAdamTrainer* t);

#ifdef __cplusplus
}
#endif

#endif
