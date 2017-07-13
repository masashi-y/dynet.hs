
module DyNet.Internal.Core where

{#import DyNet.Internal.Vector #}

import Foreign.C.Types ( CInt(..), CChar(..), CFloat(..), CLong(..), CUInt(..) )
import Foreign.Ptr     ( Ptr(..), FunPtr(..), castPtr )
import Foreign.Storable ( Storable(..) )
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.C.String
import Foreign.ForeignPtr ( newForeignPtr )
import Control.Monad (forM)

import Prelude  hiding ( tanh )
import Data.Int ( Int64(..) )
import DyNet.Vector


#include "dynet.h"

-- type Dim = [Int64]
withDim d f = dim d >>= flip withHDim f

{#pointer *CModel as Model
    foreign finalizer delete_Model newtype #}

{#pointer *CParameter as Parameter
    foreign finalizer delete_Parameter newtype #}

{#pointer *CLookupParameter as LookupParameter
    foreign finalizer delete_LookupParameter newtype #}

{#pointer *CExpression as Expression
    foreign finalizer delete_Expression newtype #}

{#pointer *CTensor as Tensor
    foreign finalizer delete_Tensor newtype #}

{#pointer *CComputationGraph as ComputationGraph
    foreign finalizer doNothing newtype #}

{#pointer *CDim as HDim
    foreign finalizer delete_Dim newtype #}

{#fun init_Model as createModel
    {+S} -> `Model' #}

{#fun init_ComputationGraph as createComputationGraph
    {+S} -> `ComputationGraph' #}

{#fun c_as_scalar as asScalar {`Tensor'} -> `Float' #}

{#fun c_as_vector as asVector {+S, `Tensor'} -> `FloatVector' #}

{#fun Model_add_parameters as addParameters
    `Integral d' =>
    {`Model', +S, withDim* `[d]'} -> `Parameter' #}

{#fun Model_add_lookup_parameters as addLookupParameters
    `Integral d' =>
    {`Model', +S, `Int', withDim* `[d]'} -> `LookupParameter' #}

{#fun ComputationGraph_print_graphviz as printGraphviz
    {`ComputationGraph'} -> `()' #} 

{#fun delete_ComputationGraph as ^
    {`ComputationGraph'} -> `()' #} 

-- Tensor returned by "forward" is owned by DyNet
-- and should not finalize it using finalizer
foreign import ccall "&doNothing"
    deleteNothing :: FunPtr (Ptr Tensor -> IO ());

notDelete x = newForeignPtr deleteNothing x >>= (return . Tensor)

{#fun ComputationGraph_forward as forward
    {`ComputationGraph',
     `Expression'} -> `Tensor' notDelete* #} 

{#fun ComputationGraph_backward as backward
    {`ComputationGraph',
     `Expression'} -> `()' #} 

instance Storable Model where
    sizeOf _ = fromIntegral $ sizeOfModel
    alignment _ = 4
    peek = undefined
    poke = undefined

instance Storable ComputationGraph where
    sizeOf _ = fromIntegral $ sizeOfComputationGraph
    alignment _ = 4
    peek = undefined
    poke = undefined

instance Storable Parameter where
    sizeOf _ = fromIntegral $ sizeOfParameter
    alignment _ = 4
    peek = undefined
    poke = undefined

instance Storable LookupParameter where
    sizeOf _ = fromIntegral $ sizeOfLookupParameter
    alignment _ = 4
    peek = undefined
    poke = undefined

instance Storable Expression where
    sizeOf _ = fromIntegral $ sizeOfExpression
    alignment _ = 4
    peek = undefined
    poke = undefined

{#fun new_Dim_v as ^ {`LongVector'} -> `HDim' #}
{#fun Dim_size as ^ {`HDim'} -> `Int' #}

dim :: Integral a => [a] -> IO HDim
dim list = fromList (map fromIntegral list) >>= newDimV


initialize :: [String] -> Bool -> IO [String]
initialize argv shared_parameters = do
    let argv' = "":argv
        len = length argv'
    cargv <- mapM newCString argv'
    cargv' <- allocaArray len $ \ptr ->
        alloca $ \ptr' -> do
        pokeArray ptr cargv
        poke ptr' (fromIntegral len)
        dynetInitialize ptr' ptr shared_parameters
        len' <- peek ptr'
        peekArray (fromIntegral len') ptr
    (_:res) <- mapM peekCString cargv'
    mapM free cargv
    return res

foreign import ccall "dynet_initialize"
    dynetInitialize :: Ptr CInt -> Ptr (Ptr CChar) -> Bool -> IO ()
foreign import ccall "size_of_Model"
    sizeOfModel :: CInt
foreign import ccall "size_of_ComputationGraph"
    sizeOfComputationGraph :: CInt
foreign import ccall "size_of_Parameter"
    sizeOfParameter :: CInt
foreign import ccall "size_of_LookupParameter"
    sizeOfLookupParameter :: CInt
foreign import ccall "size_of_Expression"
    sizeOfExpression :: CInt

