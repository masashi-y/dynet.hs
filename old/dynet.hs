{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module DyNet (
    main
) where

import Prelude  hiding ( tanh )
import Foreign.C.Types       ( CInt(..), CChar(..), CFloat(..), CLong(..), CUInt(..) )
import Foreign.Ptr           ( Ptr(..), FunPtr(..) )
import Foreign.ForeignPtr    ( ForeignPtr(..), newForeignPtr, withForeignPtr, FinalizerPtr )
import Foreign.C.String
import Foreign.Marshal
import Foreign.Storable
import Control.Monad (forM)

import System.Environment

import qualified Vector as V

data HLookupParameter
data HDim
type family CType a = b | b -> a

#define import_struct(STRUCT)                                             \
    data H/**/STRUCT;                                                     \
    newtype STRUCT = STRUCT { get/**/STRUCT :: ForeignPtr H/**/STRUCT };  \
    type instance CType STRUCT = H/**/STRUCT;                             \
    foreign import ccall "size_of_/**/STRUCT"                             \
        sizeof/**/STRUCT :: CInt;                                         \
    foreign import ccall "&delete_/**/STRUCT"                             \
        delete/**/STRUCT :: FunPtr (Ptr H/**/STRUCT -> IO ());            \
    instance Storable H/**/STRUCT where                                   \
        sizeOf _ = fromIntegral sizeof/**/STRUCT;                         \
        alignment _ = 4;                                                  \
        peek = undefined;                                                 \
        poke = undefined;

import_struct(Model)
import_struct(Parameter)
import_struct(Expression)
import_struct(ComputationGraph)
import_struct(Tensor)
import_struct(Trainer)


class Storable (CType a) => Alloca a where
    constructor :: (ForeignPtr (CType a) -> a)
    deleter :: FinalizerPtr (CType a)

    allocateWith :: (Ptr (CType a) -> IO ()) -> IO a
    allocateWith f = do
        ptr <- malloc
        f ptr
        ptr' <- newForeignPtr deleter ptr
        return $ constructor ptr'

instance Alloca ComputationGraph where
    constructor = ComputationGraph
    deleter = deleteComputationGraph

instance Alloca Model where
    constructor = Model
    deleter = deleteModel

instance Alloca Expression where
    constructor = Expression
    deleter = deleteExpression

instance Alloca Parameter where
    constructor = Parameter
    deleter = deleteParameter

instance Alloca Tensor where
    constructor = Tensor
    deleter = deleteTensor

instance Alloca Trainer where
    constructor = Trainer
    deleter = deleteTrainer

type Dim = [V.Int64]
-- CDim* new_Dim_v(LongVector* ds) { return reinterpret_cast<CDim*>(new Dim(*ds)); }
foreign import ccall "new_Dim_v"
    c_newDim :: Ptr (V.Vec V.Int64) -> Ptr HDim

newDim :: [V.Int64] -> IO (Ptr HDim)
newDim dim = pure (c_newDim . V.getPtr) <*> V.fromList dim

foreign import ccall "dynet_initialize"
    dynet_initialize :: CInt -> Ptr CString -> Bool -> IO ()

foreign import ccall "init_Model"
    initModel :: Ptr HModel -> IO ()

foreign import ccall "init_ComputationGraph"
    initComputationGraph :: Ptr HComputationGraph -> IO ()

createComputationGraph :: IO ComputationGraph
createComputationGraph = allocateWith initComputationGraph

createModel :: IO Model
createModel = allocateWith initModel

foreign import ccall "init_SimpleSGDTrainer"
    initSimpleSGDTrainer :: Ptr HTrainer -> Ptr HModel -> CFloat -> CFloat -> IO ()
foreign import ccall "Trainer_update"
    c_trainer_update :: Ptr HTrainer -> CFloat -> IO ()

createSimpleSGDTrainer :: Model -> Float -> Float -> IO Trainer
createSimpleSGDTrainer (Model m) e0 edecay = allocateWith $ \ptr ->
    withForeignPtr m $ \m' ->
    initSimpleSGDTrainer ptr m' (realToFrac e0) (realToFrac edecay)

update :: Trainer -> Float -> IO ()
update (Trainer t) s = withForeignPtr t $ \t' -> c_trainer_update t' (realToFrac s)

tanh :: Expression -> IO Expression
tanh (Expression x) = allocateWith (\ptr -> withForeignPtr x (c_tanh ptr))

input :: ComputationGraph -> Dim -> V.Vector Float -> IO Expression
input (ComputationGraph cg) dim (V.Vec v) = allocateWith $ \ptr ->
      withForeignPtr cg $ \cg' -> do
          dim' <- newDim dim
          c_input ptr cg' dim' v

parameter :: ComputationGraph -> Parameter -> IO Expression
parameter (ComputationGraph cg) (Parameter p) = allocateWith $ \ptr ->
    withForeignPtr cg $ \cg' ->
    withForeignPtr p $ \p' ->
    c_parameter ptr cg' p'

binOp :: (Ptr HExpression -> Ptr HExpression -> Ptr HExpression -> IO ())
    -> (Expression -> Expression -> IO Expression)
binOp cOp = \ (Expression x) (Expression y) ->
    allocateWith $ \ptr ->
    withForeignPtr x $ \x' ->
    withForeignPtr y $ \y' ->
    cOp ptr x' y'

add = binOp c_op_add
mul = binOp c_op_mul
squaredDistance = binOp c_squared_distance

printGraphviz :: ComputationGraph -> IO ()
printGraphviz (ComputationGraph cg) = withForeignPtr cg c_printGraphviz

foreign import ccall "Model_add_parameters"
    modelAddParameters :: Ptr HModel -> Ptr HParameter -> Ptr HDim -> IO ()
foreign import ccall "Model_add_lookup_parameters"
    modelAddLookupParameters :: Ptr HModel -> Ptr HLookupParameter -> CUInt -> Ptr HDim -> IO ()

addParameter :: Model -> Dim -> IO Parameter
addParameter (Model m) dim = allocateWith $ \ptr ->
    withForeignPtr m $ \m' -> do
        dim' <- newDim dim
        modelAddParameters m' ptr dim'

forward :: ComputationGraph -> Expression -> IO Tensor
forward (ComputationGraph cg) (Expression x) =
    withForeignPtr cg $ \cg' ->
    withForeignPtr x  $ \x' ->
    Tensor <$> newForeignPtr deleteNothing (cg_forward cg' x')

backward :: ComputationGraph -> Expression -> IO ()
backward (ComputationGraph cg) (Expression x) =
    withForeignPtr cg $ \cg' ->
    withForeignPtr x  $ \x' ->
    cg_backward cg' x'

asScalar :: Tensor -> IO Float
asScalar (Tensor t) = withForeignPtr t (return . c_as_scalar)

-- longVector :: [Long] -> IO (V.Vector Long)
-- longVector list = do
--     let size = length list
--     res <- allocaArray size $ \ptr -> do
--         pokeArray ptr $ map fromIntegral list
--         return $ newLongVector ptr (fromIntegral size)
--     return $ Vec res


initialize :: [String] -> Bool -> IO ()
initialize argv shared_parameters = do
    let argc = fromIntegral $ length argv
    argv' <- newCString $ unwords argv
    alloca $ \ptr -> do
        poke ptr argv'
        dynet_initialize argc ptr shared_parameters



main = do
    let hiddenSize = 8
    argv <- getArgs
    initialize argv True
    m <- createModel
    trainer <- createSimpleSGDTrainer m 0.1 0.0
    p_W <- addParameter m [hiddenSize, 2]
    p_b <- addParameter m [hiddenSize]
    p_V <- addParameter m [1, hiddenSize]
    p_a <- addParameter m [1]

    cg <- createComputationGraph

    mW <- parameter cg p_W
    b <- parameter cg p_b
    mV <- parameter cg p_V
    a <- parameter cg p_a

    xValues <- V.fromList [0, 0]
    x <- input cg [2] xValues

    yValue <- V.fromList [0]
    y <- (input cg [1]) yValue

    h <- tanh =<< (b `add`) =<< mW `mul` x  -- tanh (W * x + b)
    y_pred <- (a `add`) =<< mV `mul` h
    lossExp <- squaredDistance y_pred y

    printGraphviz cg

    let encode v = if v then 1 else -1
    forM [1..30] $ \iter -> do
        lossThisIter <- forM [0..4] $ \mi -> do
            let x1 = (mi `mod` 2) == 1
                x2 = ((mi `div` 2) `mod` 2) == 1
            V.insert xValues 0 (encode x1)
            V.insert xValues 1 (encode x2)
            V.insert yValue 0 (encode $ x1 == x2)
            loss <- asScalar =<< forward cg lossExp
            backward cg lossExp
            update trainer 1.0
            return loss
        putStrLn $ "E = " ++ show ((sum lossThisIter) / (realToFrac $ length lossThisIter))
    -- print lossV

    return ()

test = do
    v <- V.fromList [1,2,3]
    V.debug (v :: V.Vector Int)
    print $ V.size v
    V.pushBack v 10
    print $ V.size v
    V.debug v
    V.insert v 0 1000
    V.debug v
    print $ v V.! 3

-- foreign import ccall "c_input" (CExpression* out, CComputationGraph* g, float *ps);
foreign import ccall "c_input_1"
    c_input :: Ptr HExpression -> Ptr HComputationGraph -> Ptr HDim -> Ptr (V.Vec Float) -> IO ()
foreign import ccall "c_tanh"
    c_tanh :: Ptr HExpression -> Ptr HExpression -> IO ()
foreign import ccall "c_op_add"
    c_op_add :: Ptr HExpression -> Ptr HExpression -> Ptr HExpression -> IO ()
foreign import ccall "c_op_mul"
    c_op_mul :: Ptr HExpression -> Ptr HExpression -> Ptr HExpression -> IO ()
foreign import ccall "c_squared_distance"
    c_squared_distance :: Ptr HExpression -> Ptr HExpression -> Ptr HExpression -> IO ()
foreign import ccall "c_parameter"
    c_parameter :: Ptr HExpression -> Ptr HComputationGraph -> Ptr HParameter -> IO ()

foreign import ccall "ComputationGraph_print_graphviz"
    c_printGraphviz :: Ptr HComputationGraph -> IO ()

foreign import ccall "ComputationGraph_forward"
    cg_forward :: Ptr HComputationGraph -> Ptr HExpression -> Ptr HTensor
foreign import ccall "ComputationGraph_backward"
    cg_backward :: Ptr HComputationGraph -> Ptr HExpression -> IO ()

foreign import ccall "c_as_scalar"
    c_as_scalar :: Ptr HTensor -> Float

foreign import ccall "&doNothing"
    deleteNothing :: FunPtr (Ptr HTensor -> IO ());
