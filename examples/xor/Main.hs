
import System.Environment
import Control.Monad ( forM, when )
import qualified DyNet.Core as D
import qualified DyNet.Expr as D
import qualified DyNet.Train as D
import qualified DyNet.IO as D
import qualified DyNet.Vector as V

main = do
    let hiddenSize = 8
        iteration = 30
    argv <- D.initialize' =<< getArgs
    m <- D.createModel
    trainer <- D.createSimpleSGDTrainer m 0.1 0.0

    p_W <- D.addParameters m [hiddenSize, 2]
    p_b <- D.addParameters m [hiddenSize]
    p_V <- D.addParameters m [1, hiddenSize]
    p_a <- D.addParameters m [1]

    when (not $ null argv) $ do
        let (path:_) = argv
        loader <- D.createLoader path
        D.populateModel' loader m
        putStrLn $ "Model loaded from: " ++ path

    D.withNewComputationGraph $ \cg -> do
        _W <- D.parameter cg p_W
        b <- D.parameter cg p_b
        _V <- D.parameter cg p_V
        a <- D.parameter cg p_a

        xValues <- V.fromList ([0, 0] :: [Float])
        x <- D.input cg [2] xValues

        yValue <- V.fromList ([0] :: [Float])
        y <- D.input cg [1] yValue

        h <- D.tanh $ _W `D.mul` x `D.add` b
        y_pred <- _V `D.mul` h `D.add` a
        lossExp <- D.squaredDistance y_pred y

        D.printGraphviz cg

        let encode v = if v then 1 else -1
        forM [1..iteration] $ \iter -> do
            loss' <- forM [0..3] $ \mi -> do
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

        saver <- D.createSaver' "/tmp/xor.model"
        D.saveModel' saver m

