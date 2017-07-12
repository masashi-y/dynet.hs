{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}


module DyNet.Internal.Vector where

import Foreign.Ptr
import Data.Int  ( Int64 )
import Foreign.Storable
import Foreign.Marshal
import Foreign.C.Types

#include "vector.h"


allocaArrayWith :: Storable b => (a -> b) -> [a] -> (Ptr b -> IO c) -> IO c
allocaArrayWith f list g = do
    allocaArray (length list) $ \ptr -> do
        pokeArray ptr (map f list)
        g ptr

allocaIntArray :: (Storable b, Integral a, Num b) =>
                  [a] -> (Ptr b -> IO c) -> IO c 
allocaIntArray = allocaArrayWith fromIntegral

allocaFloatArray :: (Storable b, Real a, Fractional b) =>
                  [a] -> (Ptr b -> IO c) -> IO c 
allocaFloatArray = allocaArrayWith realToFrac

-- allocaArr list f = do
--     allocaArray (length list) $ \ptr ->
--         f ptr
--         peekArray (length list) ptr

type family Vector a = b | b -> a
type instance Vector Float = FloatVector
type instance Vector Int = IntVector
type instance Vector Int64 = LongVector
type instance Vector Word = UIntVector

class Vectorizable a where
    (!) :: Vector a -> Int -> IO a
    constructor :: [a] -> Int -> IO (Vector a)
    pushBack :: Vector a -> a -> IO ()
    size :: Vector a -> IO Int
    debug :: Vector a -> IO ()
    insert :: Vector a -> Int -> a -> IO ()

    fromList :: [a] -> IO (Vector a)
    fromList list = constructor list (length list)

-- ######################################################
-- ###################### Int Vector ####################
-- ######################################################

instance Vectorizable Int where
    constructor = newIntVector
    size = intVectorSize
    pushBack = intVectorPushBack
    debug = intVectorShow
    -- cCopy = intVectorCopy
    insert = intVectorSet
    (!) = intVectorGet


{#pointer *IntVector as IntVector
    foreign finalizer delete_IntVector newtype #}

{#fun init_IntVector_intp_int as newIntVector
    {+, allocaIntArray* `[Int]', `Int'} -> `IntVector' #}

{#fun IntVector_size as ^
    {`IntVector'} -> `Int' #}

{#fun IntVector_get as ^
    {`IntVector', `Int'} -> `Int' #}

{#fun IntVector_set as ^
    {`IntVector', `Int', `Int'} -> `()' #}

{#fun IntVector_push_back as ^
    {`IntVector', `Int'} -> `()' #}

{#fun IntVector_show as ^
    {`IntVector'} -> `()' #}

-- {#fun IntVector_copy as ^
--     {`IntVector', allocaIntArray* `[Int]' id} -> `[Int]' peekArray* #}

-- ######################################################
-- ##################### Float Vector ###################
-- ######################################################

instance Vectorizable Float where
    (!) = floatVectorGet
    constructor = newFloatVector
    insert = floatVectorSet
    size = floatVectorSize
    pushBack = floatVectorPushBack
    debug = floatVectorShow
    -- cCopy = floatVectorCopy


{#pointer *FloatVector as FloatVector
    foreign finalizer delete_FloatVector newtype #}

{#fun init_FloatVector_intp_int as newFloatVector
    {+, allocaFloatArray* `[Float]', `Int'} -> `FloatVector' #}

{#fun FloatVector_size as ^
    {`FloatVector'} -> `Int' #}

{#fun FloatVector_get as ^
    {`FloatVector', `Int'} -> `Float' #}

{#fun FloatVector_set as ^
    {`FloatVector', `Int', `Float'} -> `()' #}

{#fun FloatVector_push_back as ^
    {`FloatVector', `Float'} -> `()' #}

{#fun FloatVector_show as ^
    {`FloatVector'} -> `()' #}

-- {#fun FloatVector_copy as ^
--     {`FloatVector', allocaFloatArray* `[Float]'} -> `[Float]' #}

-- ######################################################
-- ##################### Long Vector ####################
-- ######################################################

instance Vectorizable Int64 where
    constructor = newLongVector
    size = longVectorSize
    pushBack = longVectorPushBack
    debug = longVectorShow
    -- cCopy = longVectorCopy
    insert = longVectorSet
    (!) = longVectorGet


{#pointer *LongVector as LongVector
    foreign finalizer delete_LongVector newtype #}

{#fun init_LongVector_intp_int as newLongVector
    {+, allocaIntArray* `[Int64]', `Int'} -> `LongVector' #}

{#fun LongVector_size as ^
    {`LongVector'} -> `Int' #}

{#fun LongVector_get as ^
    {`LongVector', `Int'} -> `Int64' #}

{#fun LongVector_set as ^
    {`LongVector', `Int', `Int64'} -> `()' #}

{#fun LongVector_push_back as ^
    {`LongVector', `Int64'} -> `()' #}

{#fun LongVector_show as ^
    {`LongVector'} -> `()' #}

-- {#fun IntVector_copy as ^
--     {`IntVector', allocaArr* +} -> `[Int]' #}

-- ######################################################
-- ###################### UInt Vector ###################
-- ######################################################

instance Vectorizable Word where
    constructor = newUIntVector
    size = uIntVectorSize
    pushBack = uIntVectorPushBack
    debug = uIntVectorShow
    -- cCopy = uIntVectorCopy
    insert = uIntVectorSet
    (!) = uIntVectorGet


{#pointer *UIntVector as UIntVector
    foreign finalizer delete_UIntVector newtype #}

{#fun init_UIntVector_intp_int as newUIntVector
    {+, allocaIntArray* `[Word]', `Int'} -> `UIntVector' #}

{#fun UIntVector_size as ^
    {`UIntVector'} -> `Int' #}

{#fun UIntVector_get as ^
    {`UIntVector', `Int'} -> `Word' fromIntegral #}

{#fun UIntVector_set as ^
    {`UIntVector', `Int', fromIntegral `Word'} -> `()' #}

{#fun UIntVector_push_back as ^
    {`UIntVector', fromIntegral `Word'} -> `()' #}

{#fun UIntVector_show as ^
    {`UIntVector'} -> `()' #}

-- -- {#fun UIntVector_copy as ^
-- --     {`UIntVector', allocaIntArray* `[Int]' id} -> `[Int]' peekArray* #}
-- 
