{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE IncoherentInstances #-}

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
    {`Model', +S, withDimension* `d'} -> `Parameter' #}

{#fun Model_add_lookup_parameters as addLookupParameters
    `Dimension d' =>
    {`Model', +S, `Int', withDimension* `d'} -> `LookupParameter' #}

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

{#fun ComputationGraph_incremental_forward as incrementalForward
    {`ComputationGraph',
     `Expression'} -> `Tensor' notDelete* #} 

{#fun ComputationGraph_invalidate as invalidate
    {`ComputationGraph'} -> `()' #}

{#fun ComputationGraph_clear as clear
    {`ComputationGraph'} -> `()' #}

{#fun ComputationGraph_checkpoint as checkpoint
    {`ComputationGraph'} -> `()' #}

{#fun ComputationGraph_revert as revert
    {`ComputationGraph'} -> `()' #}

-- {#fun ComputationGraph_get_dimension as getDimension
--     {`ComputationGraph', +S, `Int'} -> `Dim' notDelete* #}

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

