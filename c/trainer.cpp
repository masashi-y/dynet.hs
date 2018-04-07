
#include <vector>
#include <iostream>
#include "trainer.h"

#include "dynet/training.h"
#include "dynet/model.h"

using namespace dynet;
using namespace std;

// void Trainer_update_subset(CTrainer* t, UIntVector* updated_params, UIntVector* updated_lookup_params, float scale) {
//     reinterpret_cast<Trainer*>(t)->
//         update(*reinterpret_cast<const std::vector<unsigned>*>(updated_params),
//    *reinterpret_cast<const std::vector<unsigned>*>(updated_lookup_params), scale);
// }

void Trainer_update(CTrainer* t) {
    reinterpret_cast<Trainer*>(t)->update();
}

void Trainer_update_epoch(CTrainer* t, float r) {
    reinterpret_cast<Trainer*>(t)->update_epoch(r);
}

float Trainer_clip_gradients(CTrainer* t) {
    return reinterpret_cast<Trainer*>(t)->clip_gradients();
}

void Trainer_rescale_and_reset_weight_decay(CTrainer* t) {
    reinterpret_cast<Trainer*>(t)->rescale_and_reset_weight_decay();
}

void Trainer_status(CTrainer* t) {
    reinterpret_cast<Trainer*>(t)->status();
}


void init_SimpleSGDTrainer(CSimpleSGDTrainer* t, CModel* m, float learning_rate) {
    new (t) SimpleSGDTrainer(*reinterpret_cast<ParameterCollection*>(m),
                              learning_rate);
}

void init_CyclicalSGDTrainer(CCyclicalSGDTrainer* t, CModel* m, float e0_min, float e0_max, float step_size, float gamma, float edecay) {
    new (t) CyclicalSGDTrainer(*reinterpret_cast<ParameterCollection*>(m),
                              e0_min, e0_max, step_size, gamma, edecay);
}

void init_MomentumSGDTrainer(CMomentumSGDTrainer* t, CModel* m, float learning_rate, float mom) {
    new (t) MomentumSGDTrainer(*reinterpret_cast<ParameterCollection*>(m),
                              learning_rate, mom);
}

void init_AdagradTrainer(CAdagradTrainer* t, CModel* m, float learning_rate, float eps) {
    new (t) AdagradTrainer(*reinterpret_cast<ParameterCollection*>(m),
                            learning_rate, eps);
}

void init_AdadeltaTrainer(CAdadeltaTrainer* t, CModel* m, float eps, float rho) {
    new (t) AdadeltaTrainer(*reinterpret_cast<ParameterCollection*>(m),
                            eps, rho);
}

void init_RMSPropTrainer(CRMSPropTrainer* t, CModel* m, float learning_rate, float eps, float rho) {
    new (t) RMSPropTrainer(*reinterpret_cast<ParameterCollection*>(m),
                           learning_rate, eps, rho);
}

void init_AdamTrainer(CAdamTrainer* t, CModel* m, float learning_rate, float beta_1, float beta_2, float eps) {
    new (t) AdamTrainer(*reinterpret_cast<ParameterCollection*>(m),
                        learning_rate, beta_1, beta_2, eps);
}

unsigned size_of_SimpleSGDTrainer() {
    return sizeof(SimpleSGDTrainer);
}

unsigned size_of_CyclicalSGDTrainer() {
    return sizeof(CyclicalSGDTrainer);
}

unsigned size_of_MomentumSGDTrainer() {
    return sizeof(MomentumSGDTrainer);
}

unsigned size_of_AdagradTrainer() {
    return sizeof(AdagradTrainer);
}

unsigned size_of_AdadeltaTrainer() {
    return sizeof(AdadeltaTrainer);
}

unsigned size_of_RMSPropTrainer() {
    return sizeof(RMSPropTrainer);
}

unsigned size_of_AdamTrainer() {
    return sizeof(AdamTrainer);
}

void delete_SimpleSGDTrainer(CSimpleSGDTrainer* t) {
    reinterpret_cast<SimpleSGDTrainer*>(t)->~SimpleSGDTrainer();
}

void delete_CyclicalSGDTrainer(CCyclicalSGDTrainer* t) {
    reinterpret_cast<CyclicalSGDTrainer*>(t)->~CyclicalSGDTrainer();
}

void delete_MomentumSGDTrainer(CMomentumSGDTrainer* t) {
    reinterpret_cast<MomentumSGDTrainer*>(t)->~MomentumSGDTrainer();
}

void delete_AdagradTrainer(CAdagradTrainer* t) {
    reinterpret_cast<AdagradTrainer*>(t)->~AdagradTrainer();
}

void delete_AdadeltaTrainer(CAdadeltaTrainer* t) {
    reinterpret_cast<AdadeltaTrainer*>(t)->~AdadeltaTrainer();
}

void delete_RMSPropTrainer(CRMSPropTrainer* t) {
    reinterpret_cast<RMSPropTrainer*>(t)->~RMSPropTrainer();
}

void delete_AdamTrainer(CAdamTrainer* t) {
    reinterpret_cast<AdamTrainer*>(t)->~AdamTrainer();
}


