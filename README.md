# DyNet.hs

Haskell binding for [DyNet](https://github.com/clab/dynet).

## Build

#### Requirements
- C++ build environment for DyNet
- [haskell-stack](https://www.haskellstack.org)

To build C++ DyNet, please refer to https://dynet.readthedocs.io.

```shell
  git clone https://github.com/clab/dynet
  # Build this snapshot of DyNet following the instruction
  cd dynet && git checkout 2204e64fcfad6ad14425ab17317e30eaf915268d
  # clone dynet.hs to some other place
  git clone https://github.com/masashi-y/dynet.hs
  cd dynet.hs
  DYNET=/path/to/dynet EIGEN3_INCLUDE_DIR=/path/to/eigen stack build
  stack exec train-xor # You may need to export LD_LIBRARY_PATH=c:/path/to/dynet
  # Hopefully xor example runs!
```

I have checked the build completes successfully in:
- Ubuntu 16.04
- macOS Sierra 10.12.5

## Usage

You can write in almost the same manner as in C++ and Python versions.

Example code:
```haskell
import DyNet.Core
import DyNet.Trainer -- SGD, AdaGrad, Adam etc.
import DyNet.Expr -- operations: input, mul, logistic
import qualified DyNet.Vector as V -- wrapper for C++ STL vector

main = do
    -- Initialization function in DyNet
    initialize' ["--dynet-seed", "1"] -- command line args
    m <- createModel
    -- Functions with default args are marked with prime (')
    -- createSimpleSGDTrainer' m = createSimpleSGDTrainer m 0.1 0.0
    sgd <- createSimpleSGDTrainer' m
    -- Use withNewComputationGraph function to
    -- ensure that there is only one CG at a time
    -- (which is required by DyNet).
    withNewComputationGraph $ \cg -> do
        -- parameter matrix with 1 x 3 shape
        pW <- addParameters m [1, 3]
        _W <- parameter cg pW

        -- input vector
        x_values <- V.fromList [0,0,0]
        x <- input [3] cg x_values

        -- It is also OK to write:
        -- x <- input [3] cg [0,0,0]

        -- label
        y_values <- V.fromList [0]
        y <- input [1] cg y_values

        -- print computation graph structure
        printGraphviz cg

        -- prediction and loss
        y_pred <- logistic (_W `mul` x)
        l <- binaryLogLoss y_pred y

        loss <- asScalar =<< forward cg l -- loss as Haskell Float type

        -- parameter update
        backward cg
        update sgd 1.0
```

Other example programs are available in `examples` directory.
