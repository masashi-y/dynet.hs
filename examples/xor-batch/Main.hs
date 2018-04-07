
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
    trainer <- D.createSimpleSGDTrainer m 0.1

    p_W <- D.addParameters' m [hiddenSize, 2]
    p_b <- D.addParameters' m [hiddenSize]
    p_V <- D.addParameters' m [1, hiddenSize]
    p_a <- D.addParameters' m [1]

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

        xDim <- D.dim [2] 4
        D.print_ xDim
        xValues <- V.fromList ([1.0,  1.0,
                                1.0, -1.0,
                               -1.0,  1.0,
                               -1.0, -1.0] :: [Float])
        x <- D.input cg xDim xValues

        -- it is also possible to pass args directly
        y <- D.input cg (D.dim [1] 4) [-1.0, 1.0, 1.0, -1.0]

        h <- D.tanh $ _W `D.mul` x `D.add` b
        y_pred <- _V `D.mul` h `D.add` a
        loss <- D.squaredDistance y_pred y
        sumLoss <- D.sumBatches loss

        forM [1..iteration] $ \iter -> do
            loss' <- D.asScalar =<< D.forward cg sumLoss
            D.backward cg sumLoss
            D.update trainer
            putStrLn $ "E = " ++ show (loss' / 4)

        saver <- D.createSaver' "/tmp/xor.model"
        D.saveModel' saver m

