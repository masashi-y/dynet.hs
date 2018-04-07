{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE IncoherentInstances #-}

module DyNet.Internal.Core where

{#import DyNet.Internal.Vector #}

import Foreign.C.Types ( CInt(..), CChar(..), CFloat(..), CLong(..), CUInt(..) )
import Foreign.Ptr     ( Ptr(..), FunPtr(..), castPtr, castFunPtr )
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

{#pointer *CModel as Model
    foreign finalizer delete_Model newtype #}

{#pointer *CParameter as Parameter
    foreign finalizer delete_Parameter newtype #}

{#pointer *CLookupParameter as LookupParameter
    foreign finalizer delete_LookupParameter newtype #}

{#pointer *CExpression as Expression
    foreign finalizer delete_Expression newtype #}

{#pointer *CParameterInit as ParameterInit
    foreign finalizer delete_ParameterInit newtype #}


{#fun new_ParameterInitNormal as initNormal
    {`Float', `Float'} -> `ParameterInit' #}

{#fun new_ParameterInitUniform as initUniform
    {`Float'} -> `ParameterInit' #}

{#fun new_ParameterInitConst as initConst
    {`Float'} -> `ParameterInit' #}

{#fun new_ParameterInitIdentity as initIdentity
    {} -> `ParameterInit' #}

{#fun new_ParameterInitGlorot as initGlorot
    {`Bool', `Float'} -> `ParameterInit' #}

{#fun new_ParameterInitSaxe as initSaxe
    {`Float'} -> `ParameterInit' #}

{#fun new_ParameterInitFromFile as initFromFile
    {`String'} -> `ParameterInit' #}

{#fun new_ParameterInitFromVector as initFromVector
    `Sequence Float s' =>
    {withSequence* `s'} -> `ParameterInit' #}


{#pointer *CTensor as Tensor
    foreign finalizer delete_Tensor newtype #}

{#pointer *CComputationGraph as ComputationGraph newtype #}

{#pointer *CDim as Dim
    foreign finalizer delete_Dim newtype #}

{#fun init_Model as createModel
    {+S} -> `Model' #}

{#fun new_ComputationGraph as createComputationGraph
    {} -> `ComputationGraph' #}

{#fun c_as_scalar as asScalar {`Tensor'} -> `Float' #}

{#fun c_as_vector as asVector {+S, `Tensor'} -> `FloatVector' #}

{#fun Model_add_parameters as addParameters
    `Dimension d' =>
    {`Model', +S, withDimension* `d', `String'} -> `Parameter' #}

{#fun Model_add_parameters_1 as addParametersWith
    `Dimension d' =>
    {`Model', +S, withDimension* `d', `ParameterInit', `String'} -> `Parameter' #}

{#fun Model_add_lookup_parameters as addLookupParameters
    `Dimension d' =>
    {`Model', +S, `Int', withDimension* `d', `String'} -> `LookupParameter' #}

{#fun Model_add_lookup_parameters_1 as addLookupParametersWith
    `Dimension d' =>
    {`Model', +S, `Int', withDimension* `d', `ParameterInit', `String'} -> `LookupParameter' #}


{#fun ComputationGraph_print_graphviz as printGraphviz
    {`ComputationGraph'} -> `()' #} 

{#fun delete_ComputationGraph as ^
    {`ComputationGraph'} -> `()' #} 

-- Tensor returned by "forward" is owned by DyNet
-- and should not finalize it using finalizer
foreign import ccall "&doNothing"
    deleteNothing :: FunPtr (Ptr () -> IO ());

notDeleteTensor x = newForeignPtr (castFunPtr deleteNothing) x >>= (return . Tensor)
notDeleteDim x = newForeignPtr (castFunPtr deleteNothing) x >>= (return . Dim)

{#fun ComputationGraph_forward as forward
    {`ComputationGraph',
     `Expression'} -> `Tensor' notDeleteTensor* #} 

{#fun ComputationGraph_backward as backward
    {`ComputationGraph',
     `Expression'} -> `()' #} 

{#fun ComputationGraph_incremental_forward as incrementalForward
    {`ComputationGraph',
     `Expression'} -> `Tensor' notDeleteTensor* #} 

{#fun ComputationGraph_invalidate as invalidate
    {`ComputationGraph'} -> `()' #}

{#fun ComputationGraph_clear as clear
    {`ComputationGraph'} -> `()' #}

{#fun ComputationGraph_checkpoint as checkpoint
    {`ComputationGraph'} -> `()' #}

{#fun ComputationGraph_revert as revert
    {`ComputationGraph'} -> `()' #}

-- {#fun ComputationGraph_get_dimension as getDimension
--     {`ComputationGraph', +S, `Int'} -> `Dim' notDeleteDim* #}

{#fun Expression_value as getValue
    {`Expression'} -> `Tensor' notDeleteTensor* #} 

{#fun Expression_gradient as getGradient
    {`Expression'} -> `Tensor' notDeleteTensor* #} 

{#fun Expression_dim as getDim
    {`Expression'} -> `Dim' notDeleteDim* #} 

instance Storable Model where
    sizeOf _ = sizeOfModel
    alignment _ = 4
    peek = undefined
    poke = undefined

instance Storable ComputationGraph where
    sizeOf _ = sizeOfComputationGraph
    alignment _ = 4
    peek = undefined
    poke = undefined

instance Storable Parameter where
    sizeOf _ = sizeOfParameter
    alignment _ = 4
    peek = undefined
    poke = undefined

instance Storable LookupParameter where
    sizeOf _ = sizeOfLookupParameter
    alignment _ = 4
    peek = undefined
    poke = undefined

instance Storable Expression where
    sizeOf _ = sizeOfExpression
    alignment _ = 4
    peek = undefined
    poke = undefined

instance Storable Dim where
    sizeOf _ = sizeOfDim
    alignment _ = 4
    peek = undefined
    poke = undefined



{#fun Tensor_debug as ^ {`Tensor'} -> `()' #}


class IsExpr a where
    withExpr :: a -> (C2HSImp.Ptr Expression -> IO b) -> IO b

instance IsExpr Expression where
    withExpr = withExpression

instance IsExpr (IO Expression) where
    withExpr exp g = exp >>= (\x -> withExpression x g)


class Dimension d where
    withDimension :: d -> (Ptr Dim -> IO a) -> IO a

instance Dimension Dim where
    withDimension = withDim

instance Dimension (IO Dim) where
    withDimension d f = d >>= (\d' -> withDim d' f)

instance Sequence Int64 d => Dimension d where
    withDimension d f = initDimV d >>= (\d' -> withDim d' f)

{#fun init_Dim_v as ^
    `Sequence Int64 s' =>
    {+S, withSequence* `s'} -> `Dim' #}

{#fun init_Dim_v_int as dim
    `Sequence Int64 s' =>
    {+S, withSequence* `s', `Int'} -> `Dim' #}

{#fun Dim_size as ^ {`Dim'} -> `Int' #}

{#fun Dim_batch_elems as ^
    {`Dim'} -> `()' #}

{#fun Dim_sum_dims as ^
    {`Dim'} -> `Int' #}

{#fun Dim_truncate as ^
    {`Dim', +S} -> `Dim' #}

{#fun Dim_resize as ^
    `Integral a' =>
    {`Dim', fromIntegral `a'} -> `()' #}

{#fun Dim_ndims as ^
    {`Dim'} -> `Int' #}

{#fun Dim_rows as ^
    {`Dim'} -> `Int' #}

{#fun Dim_cols as ^
    {`Dim'} -> `Int' #}

{#fun Dim_at as ^
    `Integral a' =>
    {`Dim', fromIntegral `a'} -> `Int' #}

{#fun Dim_set as ^
    `(Integral a1, Integral a2)' =>
    {`Dim', fromIntegral `a1', fromIntegral `a2'} -> `()' #}

{#fun Dim_transpose as ^
    {`Dim', +S} -> `Dim' #}

{#fun Dim_debug as ^
    {`Dim'} -> `()' #}

{-|
    @initialize argv shared_parameters@ parses argv until @"--"@, and initializes DyNet.
    return the rest of @argv@ in @['String']@.

    Parameters:

        * @--dynet-mem@ or @--dynet_mem@
        the memory, in megabytes, to reserve
        * @--dynet-weight-decay@ or @--dynet_weight_decay@
        the weight decay per update
        * @--dynet-seed@ or @--dynet_seed@
        the random number seed
        * @--dynet-autobatch@ or @--dynet_autobatch@
        0 for none 1 for on
        * @--dynet-profiling@ or @--dynet_profiling@
        0 for none 1 for on
        * @--dynet-gpus@ or @--dynet_gpus@
        number of GPUs to use
        * @--dynet-devices@ or @--dynet_devices@
        comma separated list of CPU and physical GPU ids to use (e.g. CPU:1,GPU:3)
-}
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

{#fun pure size_of_Model as ^ {} -> `Int' #}
{#fun pure size_of_ComputationGraph as ^ {} -> `Int' #}
{#fun pure size_of_Parameter as ^ {} -> `Int' #}
{#fun pure size_of_LookupParameter as ^ {} -> `Int' #}
{#fun pure size_of_Expression as ^ {} -> `Int' #}
{#fun pure size_of_Dim as ^ {} -> `Int' #}

