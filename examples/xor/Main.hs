
import System.Environment
import Control.Monad ( forM )
import qualified DyNet.Core as D
import qualified DyNet.Expr as D
import qualified DyNet.Vector as V

main = do
    let hiddenSize = 8
        iteration = 30
    argv <- getArgs
    D.initialize argv True
    m <- D.createModel
    trainer <- D.createSimpleSGDTrainer m 0.1 0.0

    p_W <- D.addParameters m [hiddenSize, 2]
    p_b <- D.addParameters m [hiddenSize]
    p_V <- D.addParameters m [1, hiddenSize]
    p_a <- D.addParameters m [1]

    cg <- D.createComputationGraph

    mW <- D.parameter cg p_W
    b <- D.parameter cg p_b
    mV <- D.parameter cg p_V
    a <- D.parameter cg p_a

    xValues <- V.fromList [0, 0]
    x <- D.input cg [2] xValues

    yValue <- V.fromList [0]
    y <- D.input cg [1] yValue

    h <- D.tanh =<< (b `D.add`) =<< mW `D.mul` x  -- tanh (W * x + b)
    y_pred <- (a `D.add`) =<< mV `D.mul` h
    lossExp <- D.squaredDistance y_pred y

    D.printGaphviz cg

    let encode v = if v then 1 else -1
    forM [1..iteration] $ \iter -> do
        loss' <- forM [0..4] $ \mi -> do
            let x1 = (mi `mod` 2) == 1
                x2 = ((mi `div` 2) `mod` 2) == 1
            V.insert xValues 0 (encode x1)
            V.insert xValues 1 (encode x2)
            V.insert yValue 0 (encode $ x1 == x2)
            loss <- D.asScalar =<< D.forward cg lossExp
            D.backward cg lossExp
            D.update trainer 1.0
            return loss
        putStrLn $ "E = " ++ show ((sum loss') / (realToFrac $ length loss'))

