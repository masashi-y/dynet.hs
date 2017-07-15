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
  cd dynet && git checkout  a77c0e6917528e5fad83a4e03f98ea7727f103c8
  # clone dynet.hs to some other place
  git clone https://github.com/masashi-y/dynet.hs
  cd dynet.hs
  DYNET=/path/to/dynet EIGEN3_INCLUDE_DIR=/path/to/eigen stack build
  stack exec train-xor # Hopefully the example program runs

```

## Usage

You can write in almost in the same manner as in C++ and Python versions.

Example code:
```haskell
import DyNet.Core
import DyNet.Trainer -- SGD, AdaGrad, Adam etc.
import DyNet.Expr -- operations: input, mul, logistic
import qualified DyNet.Vector as V -- wrapper for stl vector

-- All functions are IO actions
main = do
    -- Initialization function in DyNet
    -- Functions with default args are marked with prime (')
    initialize' ["--dynet-seed", "1"] -- command line args
    m <- createModel
    sgd <- createSimpleSGDTrainer' m  -- SGD
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
