{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}


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
    toList :: Vector a -> IO [a]

    fromList :: [a] -> IO (Vector a)
    fromList list = constructor list (length list)

-- This class enables any sequence type (e.g. [Int] IntVector)
-- to be passed as the std::vector<int> argument of dynet operations
class Sequence t s where
    withSequence :: s -> (C2HSImp.Ptr (Vector t) -> IO b) -> IO b

instance Real a => Sequence Float [a] where
    withSequence s f = fromList (map realToFrac s) >>= (\s' -> withFloatVector s' f)

instance Sequence Float FloatVector where
    withSequence = withFloatVector

instance Sequence Word UIntVector where
    withSequence = withUIntVector

instance Integral a => Sequence Word [a] where
     withSequence s f = fromList (map fromIntegral s) >>= (\s' -> withUIntVector s' f)

instance Sequence Int64 LongVector where
    withSequence = withLongVector

instance Integral a => Sequence Int64 [a] where
    withSequence s f = fromList (map fromIntegral s) >>= (\s' -> withLongVector s' f)


-- ######################################################
-- ###################### Int Vector ####################
-- ######################################################

instance Vectorizable Int where
    constructor = newIntVector
    size = intVectorSize
    pushBack = intVectorPushBack
    debug = intVectorShow
    toList = intVectorCopy
    insert = intVectorSet
    (!) = intVectorGet

instance Storable IntVector where
    sizeOf _ = fromIntegral sizeOfIntVector
    alignment _ = 4
    poke = undefined
    peek = undefined

{#pointer *IntVector as IntVector
    foreign finalizer delete_IntVector newtype #}

{#fun init_IntVector_intp_int as newIntVector
    {+S, allocaIntArray* `[Int]', `Int'} -> `IntVector' #}

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

-- TODO I don't know how to tell peekArray the length of the output
-- {#fun IntVector_copy as ^
--      {`IntVector', allocaIntArray* `[Int]'} -> `[Int]' peekArray* #}

foreign import ccall safe "IntVector_copy"
  intVectorCopy'_ :: Ptr IntVector -> Ptr CInt -> IO (Ptr CInt)

intVectorCopy :: IntVector -> IO [Int]
intVectorCopy v =
  withIntVector v $ \v' -> do
      len <- size v
      allocaArray len $ \out -> do
          res <- peekArray len =<< intVectorCopy'_ v' out
          return $ map fromIntegral res

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
    toList = floatVectorCopy

instance Storable FloatVector where
    sizeOf _ = fromIntegral sizeOfFloatVector
    alignment _ = 4
    poke = undefined
    peek = undefined

{#pointer *FloatVector as FloatVector
    foreign finalizer delete_FloatVector newtype #}

{#fun init_FloatVector_intp_int as newFloatVector
    {+S, allocaFloatArray* `[Float]', `Int'} -> `FloatVector' #}

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

foreign import ccall safe "FloatVector_copy"
  floatVectorCopy'_ :: Ptr FloatVector -> Ptr CFloat -> IO (Ptr CFloat)

floatVectorCopy :: FloatVector -> IO [Float]
floatVectorCopy v =
  withFloatVector v $ \v' -> do
      len <- size v
      allocaArray len $ \out -> do
          res <- peekArray len =<< floatVectorCopy'_ v' out
          return $ map realToFrac res

-- ######################################################
-- ##################### Long Vector ####################
-- ######################################################

instance Vectorizable Int64 where
    constructor = newLongVector
    size = longVectorSize
    pushBack = longVectorPushBack
    debug = longVectorShow
    insert = longVectorSet
    (!) = longVectorGet
    toList = longVectorCopy

instance Storable LongVector where
    sizeOf _ = fromIntegral sizeOfLongVector
    alignment _ = 4
    poke = undefined
    peek = undefined

{#pointer *LongVector as LongVector
    foreign finalizer delete_LongVector newtype #}

{#fun init_LongVector_intp_int as newLongVector
    {+S, allocaIntArray* `[Int64]', `Int'} -> `LongVector' #}

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

foreign import ccall safe "LongVector_copy"
  longVectorCopy'_ :: Ptr LongVector -> Ptr CLong -> IO (Ptr CLong)

longVectorCopy :: LongVector -> IO [Int64]
longVectorCopy v =
  withLongVector v $ \v' -> do
      len <- size v
      allocaArray len $ \out -> do
          res <- peekArray len =<< longVectorCopy'_ v' out
          return $ map fromIntegral res

-- ######################################################
-- ###################### UInt Vector ###################
-- ######################################################

instance Vectorizable Word where
    constructor = newUIntVector
    size = uIntVectorSize
    pushBack = uIntVectorPushBack
    debug = uIntVectorShow
    toList = uIntVectorCopy
    insert = uIntVectorSet
    (!) = uIntVectorGet

instance Storable UIntVector where
    sizeOf _ = fromIntegral sizeOfUIntVector
    alignment _ = 4
    poke = undefined
    peek = undefined

{#pointer *UIntVector as UIntVector
    foreign finalizer delete_UIntVector newtype #}

{#fun init_UIntVector_intp_int as newUIntVector
    {+S, allocaIntArray* `[Word]', `Int'} -> `UIntVector' #}

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

foreign import ccall safe "UIntVector_copy"
  uIntVectorCopy'_ :: Ptr UIntVector -> Ptr CUInt -> IO (Ptr CUInt)

uIntVectorCopy :: UIntVector -> IO [Word]
uIntVectorCopy v =
  withUIntVector v $ \v' -> do
      len <- size v
      allocaArray len $ \out -> do
          res <- peekArray len =<< uIntVectorCopy'_ v' out
          return $ map fromIntegral res

foreign import ccall "size_of_IntVector"
    sizeOfIntVector :: CInt
foreign import ccall "size_of_FloatVector"
    sizeOfFloatVector :: CInt
foreign import ccall "size_of_LongVector"
    sizeOfLongVector :: CInt
foreign import ccall "size_of_UIntVector"
    sizeOfUIntVector :: CInt
