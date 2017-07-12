{-# LANGUAGE TypeFamilies #-}

module DyNet.Internal.ExpVector where

{#import DyNet.Internal.Vector #}
{#import DyNet.Internal.Core #}

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Data.List ( reverse )

#include "vector.h"

-- ######################################################
-- ###################### Int Vector ####################
-- ######################################################

type instance Vector Expression = ExpressionVector

instance Vectorizable Expression where
    constructor = newExpressionVector
    size = expressionVectorSize
    pushBack = expressionVectorPushBack
    debug = expressionVectorShow
    -- cCopy = longVectorCopy
    insert = expressionVectorSet
    (!) = expressionVectorGet


initExpList :: [Expression] -> (Ptr (Ptr Expression) -> IO a) -> IO a
initExpList xs f = initExpList' xs f []
    where initExpList' (x:xs) f ps = withExpression x (\p -> initExpList' xs f (p:ps))
          initExpList' [] f ps =
              allocaArray ((length ps) * 8) $ \ptr -> do
              pokeArray ptr (reverse ps)
              f ptr

{#pointer *ExpressionVector as ExpressionVector
    foreign finalizer delete_ExpressionVector newtype #}

{#fun init_ExpressionVector_intp_int as newExpressionVector
    {+, initExpList* `[Expression]', `Int'} -> `ExpressionVector' #}

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

