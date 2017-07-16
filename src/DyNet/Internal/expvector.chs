{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module DyNet.Internal.ExpVector where

{#import DyNet.Internal.Vector #}
{#import DyNet.Internal.Core #}

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Storable
import Data.List ( reverse )

#include "vector.h"

-- ######################################################
-- ################# Expression Vector ##################
-- ######################################################

type instance Vector Expression = ExpressionVector

instance Vectorizable Expression where
    constructor = newExpressionVector
    size = expressionVectorSize
    pushBack = expressionVectorPushBack
    debug = expressionVectorShow
    toList = expressionVectorCopy
    insert = expressionVectorSet
    (!) = expressionVectorGet

instance Storable ExpressionVector where
    sizeOf _ = fromIntegral sizeOfExpressionVector
    alignment _ = 4
    poke = undefined
    peek = undefined

instance Sequence Expression ExpressionVector where
    withSequence = withExpressionVector

instance Sequence Expression [Expression] where
    withSequence s f = fromList s >>= (\s' -> withExpressionVector s' f)

foreign import ccall safe "ExpressionVector_copy"
  expressionVectorCopy'_ :: Ptr ExpressionVector -> Ptr (Ptr Expression) -> IO (Ptr (Ptr Expression))

expressionVectorCopy :: ExpressionVector -> IO [Expression]
expressionVectorCopy v =
  withExpressionVector v $ \v' -> do
      len <- size v
      allocaArray len $ \out -> do
          res <- peekArray len =<< expressionVectorCopy'_ v' out
          mapM (\x -> newForeignPtr_ x >>= return . Expression) res


initExpList :: [Expression] -> (Ptr (Ptr Expression) -> IO a) -> IO a
initExpList xs f = initExpList' xs f []
    where initExpList' (x:xs) f ps = withExpression x (\p -> initExpList' xs f (p:ps))
          initExpList' [] f ps =
              allocaArray ((length ps) * sizeOf (undefined :: Ptr ())) $ \ptr -> do
              pokeArray ptr (reverse ps)
              f ptr

{#pointer *ExpressionVector as ExpressionVector
    foreign finalizer delete_ExpressionVector newtype #}

{#fun init_ExpressionVector_intp_int as newExpressionVector
    {+S, initExpList* `[Expression]', `Int'} -> `ExpressionVector' #}

{#fun ExpressionVector_size as ^
    {`ExpressionVector'} -> `Int' #}

{#fun ExpressionVector_get as ^
    {`ExpressionVector', `Int'} -> `Expression' #}

{#fun ExpressionVector_set as ^
    {`ExpressionVector', `Int', `Expression'} -> `()' #}

{#fun ExpressionVector_push_back as ^
    {`ExpressionVector', `Expression'} -> `()' #}

{#fun ExpressionVector_show as ^
    {`ExpressionVector'} -> `()' #}

foreign import ccall "size_of_ExpressionVector"
    sizeOfExpressionVector :: CInt
