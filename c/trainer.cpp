
#include <vector>
#include "trainer.h"

#include "dynet/training.h"
#include "dynet/model.h"

using namespace dynet;

// void Trainer_update_subset(CTrainer* t, UIntVector* updated_params, UIntVector* updated_lookup_params, float scale) {
//     reinterpret_cast<Trainer*>(t)->
//         update(*reinterpret_cast<const std::vector<unsigned>*>(updated_params),
//    *reinterpret_cast<const std::vector<unsigned>*>(updated_lookup_params), scale);
// }
void Trainer_update(CTrainer* t, float scale) {
    reinterpret_cast<Trainer*>(t)->update(scale);
}

void Trainer_update_epoch(CTrainer* t, float r) {
    reinterpret_cast<Trainer*>(t)->update_epoch(r);
}

float Trainer_clip_gradients(CTrainer* t, float scale) {
    return reinterpret_cast<Trainer*>(t)->clip_gradients(scale);
}

void Trainer_rescale_and_reset_weight_decay(CTrainer* t) {
    reinterpret_cast<Trainer*>(t)->rescale_and_reset_weight_decay();
}



void init_SimpleSGDTrainer(CSimpleSGDTrainer* t, CModel* m, float e0, float edecay) {
    new (t) SimpleSGDTrainer(*reinterpret_cast<ParameterCollection*>(m),
                              e0, edecay);
}

void init_CyclicalSGDTrainer(CCyclicalSGDTrainer* t, CModel* m, float e0_min, float e0_max, float step_size, float gamma, float edecay) {
    new (t) CyclicalSGDTrainer(*reinterpret_cast<ParameterCollection*>(m),
                              e0_min, e0_max, step_size, gamma, edecay);
}

void init_MomentumSGDTrainer(CMomentumSGDTrainer* t, CModel* m, float e0, float mom, float edecay) {
    new (t) MomentumSGDTrainer(*reinterpret_cast<ParameterCollection*>(m),
                              e0, mom, edecay);
}

void init_AdagradTrainer(CAdagradTrainer* t, CModel* m, float e0, float eps, float edecay) {
    new (t) AdagradTrainer(*reinterpret_cast<ParameterCollection*>(m),
                            e0, eps, edecay);
}

void init_AdadeltaTrainer(CAdadeltaTrainer* t, CModel* m, float eps, float rho, float edecay) {
    new (t) AdadeltaTrainer(*reinterpret_cast<ParameterCollection*>(m),
                            eps, rho, edecay);
}

void init_RMSPropTrainer(CRMSPropTrainer* t, CModel* m, float e0, float eps, float rho, float edecay) {
    new (t) RMSPropTrainer(*reinterpret_cast<ParameterCollection*>(m),
                           e0, eps, rho, edecay);
}

void init_AdamTrainer(CAdamTrainer* t, CModel* m, float e0, float beta_1, float beta_2, float eps, float edecay) {
    new (t) AdamTrainer(*reinterpret_cast<ParameterCollection*>(m),
                        e0, beta_1, beta_2, eps, edecay);
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
    delete reinterpret_cast<SimpleSGDTrainer*>(t);
}

void delete_CyclicalSGDTrainer(CCyclicalSGDTrainer* t) {
    delete reinterpret_cast<CyclicalSGDTrainer*>(t);
}

void delete_MomentumSGDTrainer(CMomentumSGDTrainer* t) {
    delete reinterpret_cast<MomentumSGDTrainer*>(t);
}

void delete_AdagradTrainer(CAdagradTrainer* t) {
    delete reinterpret_cast<AdagradTrainer*>(t);
}

void delete_AdadeltaTrainer(CAdadeltaTrainer* t) {
    delete reinterpret_cast<AdadeltaTrainer*>(t);
}

void delete_RMSPropTrainer(CRMSPropTrainer* t) {
    delete reinterpret_cast<RMSPropTrainer*>(t);
}

void delete_AdamTrainer(CAdamTrainer* t) {
    delete reinterpret_cast<AdamTrainer*>(t);
}


