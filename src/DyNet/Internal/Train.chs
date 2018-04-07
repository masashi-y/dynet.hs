
module DyNet.Internal.Train where

{#import DyNet.Internal.Core #}

import Foreign.C.Types ( CInt )
import Foreign.Ptr
import Foreign.Storable


#include "trainer.h"

{#pointer *CTrainer as CTrainer foreign newtype #}

{#pointer *CSimpleSGDTrainer as SimpleSGDTrainer
    foreign finalizer delete_SimpleSGDTrainer newtype #}

{#pointer *CCyclicalSGDTrainer as CyclicalSGDTrainer
    foreign finalizer delete_CyclicalSGDTrainer newtype #}

{#pointer *CMomentumSGDTrainer as MomentumSGDTrainer
    foreign finalizer delete_MomentumSGDTrainer newtype #}

{#pointer *CAdagradTrainer as AdagradTrainer
    foreign finalizer delete_AdagradTrainer newtype #}

{#pointer *CAdadeltaTrainer as AdadeltaTrainer
    foreign finalizer delete_AdadeltaTrainer newtype #}

{#pointer *CRMSPropTrainer as RMSPropTrainer
    foreign finalizer delete_RMSPropTrainer newtype #}

{#pointer *CAdamTrainer as AdamTrainer
    foreign finalizer delete_AdamTrainer newtype #}


class Trainer a where
    withTrainer :: a -> (Ptr a -> IO b) -> IO b
    castTrainer :: a -> (Ptr CTrainer -> IO b) -> IO b
    castTrainer t f = withTrainer t (f . castPtr)

instance Trainer SimpleSGDTrainer where
    withTrainer = withSimpleSGDTrainer

instance Trainer CyclicalSGDTrainer where
    withTrainer = withCyclicalSGDTrainer

instance Trainer MomentumSGDTrainer where
    withTrainer = withMomentumSGDTrainer

instance Trainer AdagradTrainer where
    withTrainer = withAdagradTrainer

instance Trainer AdadeltaTrainer where
    withTrainer = withAdadeltaTrainer

instance Trainer RMSPropTrainer where
    withTrainer = withRMSPropTrainer

instance Trainer AdamTrainer where
    withTrainer = withAdamTrainer


-- {#fun Trainer_update_subset as ^
--     {`CTrainer', `UIntVector', `UIntVector', `Float'} -> `()' #}

{#fun Trainer_update as update
    `Trainer t' =>
    {castTrainer* `t'} -> `()' #}

{#fun Trainer_update_epoch as updateEpoch
    `Trainer t' =>
    {castTrainer* `t', `Float'} -> `()' #}

{#fun Trainer_clip_gradients as clipGradient
    `Trainer t' =>
    {castTrainer* `t'} -> `Float' #}

{#fun Trainer_rescale_and_reset_weight_decay as rescaleAndResetWeightDecay
    `Trainer t' =>
    {castTrainer* `t'} -> `()' #}

{#fun Trainer_status as status
    `Trainer t' =>
    {castTrainer* `t'} -> `()' #}


{-|
[@brief@] Stochastic gradient descent trainer

[@details@] This trainer performs stochastic gradient descent, the goto optimization procedure for neural networks.
In the standard setting, the learning rate at epoch \(t\) is \(\eta_t=\frac{\eta_0}{1+\eta_{\mathrm{decay}}t}\)

Reference : [reference needed](ref.need.ed)


@createSimpleSGDTrainer m learning_rate@ where:

[@parameters@]

    * @m@ ParameterCollection to be trained
    * @learning_rate@ Initial learning rate
-}
{#fun init_SimpleSGDTrainer as createSimpleSGDTrainer
    {+S, `Model', `Float'} -> `SimpleSGDTrainer' #}

{-|
[@brief@] Cyclical learning rate SGD
[@details@] This trainer performs stochastic gradient descent with a cyclical learning rate as proposed in [Smith, 2015](https://arxiv.org/abs/1506.01186).

This uses a triangular function with optional exponential decay.

More specifically, at each update, the learning rate \(\eta\) is updated according to :

\[
\begin{split}
\text{cycle} &= \left\lfloor 1 + \frac{\texttt{it}}{2 \times\texttt{step_size}} \right\rfloor\\
x &= \left\vert \frac{\texttt{it}}{\texttt{step_size}} - 2 \times \text{cycle} + 1\right\vert\\
\eta &= \eta_{\text{min}} + (\eta_{\text{max}} - \eta_{\text{min}}) \times \max(0, 1 - x) \times \gamma^{\texttt{it}}\\
\end{split}
\]

Reference : [Cyclical Learning Rates for Training Neural Networks](https://arxiv.org/abs/1506.01186)

[@parameters@]

    * @m@ ParameterCollection to be trained
    * @learning_rate_min@ Lower learning rate
    * @learning_rate_max@ Upper learning rate
    * @step_size@ Period of the triangular function in number of iterations (__not__ epochs). According to the original paper, this should be set around (2-8) x (training iterations in epoch)
    * @gamma@ Learning rate upper bound decay parameter
    * @edecay@ Learning rate decay parameter. Ideally you shouldn't use this with cyclical learning rate since decay is already handled by \(\gamma\)
-}
{#fun init_CyclicalSGDTrainer as createCyclicalSGDTrainer
    {+S, `Model', `Float', `Float', `Float', `Float', `Float'} -> `CyclicalSGDTrainer' #}

{-|
[@brief@] Stochastic gradient descent with momentum
[@details@] This is a modified version of the SGD algorithm with momentum to stablize the gradient trajectory.
The modified gradient is \(\theta_{t+1}=\mu\theta_{t}+\nabla_{t+1}\) where \(\mu\) is the momentum.

Reference : [reference needed](ref.need.ed)

[@parameters@]

    * @m@ ParameterCollection to be trained
    * @learning_rate@ Initial learning rate
    * @mom@ Momentum
-}
{#fun init_MomentumSGDTrainer as createMomentumSGDTrainer
    {+S, `Model', `Float', `Float'} -> `MomentumSGDTrainer' #}

{-|
[@brief@] Adagrad optimizer
[@details@] The adagrad algorithm assigns a different learning rate to each parameter according to the following formula :
\(\delta_\theta^{(t)}=-\frac{\eta_0}{\epsilon+\sum_{i=0}^{t-1}(\nabla_\theta^{(i)})^2}\nabla_\theta^{(t)}\)

Reference : [Duchi et al., 2011](http://www.jmlr.org/papers/volume12/duchi11a/duchi11a.pdf)

[@parameters@]

    * @m@ ParameterCollection to be trained
    * @learning_rate@ Initial learning rate
    * @eps@ Bias parameter \(\epsilon\) in the adagrad formula
-}
{#fun init_AdagradTrainer as createAdagradTrainer
    {+S, `Model', `Float', `Float'} -> `AdagradTrainer' #}

{-|
[@brief@] AdaDelta optimizer
[@details@] The AdaDelta optimizer is a variant of Adagrad where \(\frac{\eta_0}{\sqrt{\epsilon+\sum_{i=0}^{t-1}(\nabla_\theta^{(i)})^2}}\) is replaced by \(\frac{\sqrt{\epsilon+\sum_{i=0}^{t-1}\rho^{t-i-1}(1-\rho)(\delta_\theta^{(i)})^2}}{\sqrt{\epsilon+\sum_{i=0}^{t-1}(\nabla_\theta^{(i)})^2}}\),
hence eliminating the need for an initial learning rate.

Reference : [ADADELTA: An Adaptive Learning Rate Method](https://arxiv.org/pdf/1212.5701v1)

[@parameters@]

    * @m@ ParameterCollection to be trained
    * @eps@ Bias parameter \(\epsilon\) in the adagrad formula
    * @rho@ Update parameter for the moving average of updates in the numerator
-}
{#fun init_AdadeltaTrainer as createAdadeltaTrainer
    {+S, `Model', `Float', `Float'} -> `AdadeltaTrainer' #}

{-|
[@brief@] RMSProp optimizer
[@details@] The RMSProp optimizer is a variant of Adagrad where the squared sum of previous gradients is replaced with a moving average with parameter \(\rho\).

Reference : [reference needed](ref.need.ed)

[@parameters@]

    * @m@ ParameterCollection to be trained
    * @learning_rate@ Initial learning rate
    * @eps@ Bias parameter \(\epsilon\) in the adagrad formula
    * @rho@ Update parameter for the moving average (`rho = 0` is equivalent to using Adagrad)
-}
{#fun init_RMSPropTrainer as createRMSPropTrainer
    {+S, `Model', `Float', `Float', `Float'} -> `RMSPropTrainer' #}

{-|
[@brief@] Adam optimizer
[@details@] The Adam optimizer is similar to RMSProp but uses unbiased estimates
of the first and second moments of the gradient

Reference : [Adam: A Method for Stochastic Optimization](https://arxiv.org/pdf/1412.6980v8)

[@parameters@]

    * @m@ ParameterCollection to be trained
    * @learning_rate@ Initial learning rate
    * @beta_1@ Moving average parameter for the mean
    * @beta_2@ Moving average parameter for the variance
    * @eps@ Bias parameter \(\epsilon\)
-}
{#fun init_AdamTrainer as createAdamTrainer
    {+S, `Model', `Float', `Float', `Float', `Float'} -> `AdamTrainer' #}



instance Storable SimpleSGDTrainer where
    sizeOf _ = sizeOfSimpleSGDTrainer
    alignment _ = 4
    peek = undefined
    poke = undefined

instance Storable CyclicalSGDTrainer where
    sizeOf _ = sizeOfCyclicalSGDTrainer
    alignment _ = 4
    peek = undefined
    poke = undefined

instance Storable MomentumSGDTrainer where
    sizeOf _ = sizeOfMomentumSGDTrainer
    alignment _ = 4
    peek = undefined
    poke = undefined

instance Storable AdagradTrainer where
    sizeOf _ = sizeOfAdagradTrainer
    alignment _ = 4
    peek = undefined
    poke = undefined

instance Storable AdadeltaTrainer where
    sizeOf _ = sizeOfAdadeltaTrainer
    alignment _ = 4
    peek = undefined
    poke = undefined

instance Storable RMSPropTrainer where
    sizeOf _ = sizeOfRMSPropTrainer
    alignment _ = 4
    peek = undefined
    poke = undefined

instance Storable AdamTrainer where
    sizeOf _ = sizeOfAdamTrainer
    alignment _ = 4
    peek = undefined
    poke = undefined

{#fun pure size_of_SimpleSGDTrainer as ^ {} -> `Int' #}
{#fun pure size_of_CyclicalSGDTrainer as ^ {} -> `Int' #}
{#fun pure size_of_MomentumSGDTrainer as ^ {} -> `Int' #}
{#fun pure size_of_AdagradTrainer as ^ {} -> `Int' #}
{#fun pure size_of_AdadeltaTrainer as ^ {} -> `Int' #}
{#fun pure size_of_RMSPropTrainer as ^ {} -> `Int' #}
{#fun pure size_of_AdamTrainer as ^ {} -> `Int' #}
