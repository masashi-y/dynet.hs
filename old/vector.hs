{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Vector (
  Int64,
  Word,
  Vec(..),
  Vector(..),
  Vectorizable(..)
) where

import Foreign.C.Types       ( CInt(..), CChar(..), CFloat(..), CLong(..), CUInt(..) )
import Foreign.Ptr           ( Ptr(..) )
import Foreign.C.String
import Foreign.Marshal
import Foreign.Storable
import Data.Int
import Data.Word


data Vec a
data Vector a = Vec { getPtr :: Ptr (Vec a) }

class Storable (CType a) => Vectorizable a where
    type CType a :: *
    toCType     :: a -> CType a
    fromCType   :: CType a -> a
    constructor :: Ptr (CType a) -> CInt -> Ptr (Vec a)
    destructor  :: Ptr (Vec a) -> IO ()
    cDebug      :: Ptr (Vec a) -> IO ()
    cCopy       :: Ptr (Vec a) -> Ptr (CType a) -> IO ()
    cSize       :: Ptr (Vec a) -> CInt
    cPushBack   :: Ptr (Vec a) -> CType a -> IO ()
    cSet        :: Ptr (Vec a) -> CInt -> CType a -> IO ()
    cGet        :: Ptr (Vec a) -> CInt -> CType a

    (!) :: Vector a -> Int -> a
    (Vec ptr) ! i = fromCType $ cGet ptr (fromIntegral i)

    insert :: Vector a -> Int -> a -> IO ()
    insert (Vec ptr) i v = cSet ptr (fromIntegral i) (toCType v)

    pushBack :: Vector a -> a -> IO ()
    pushBack (Vec ptr) v = cPushBack ptr (toCType v)

    fromList :: [a] -> IO (Vector a)
    fromList list = do
        let size = length list
        res <- allocaArray size $ \ptr -> do
            pokeArray ptr $ map toCType list
            return $ constructor ptr (fromIntegral size)
        return $ Vec res

    size :: Vector a -> Int
    size (Vec ptr) = fromIntegral $ cSize ptr

    toList :: Vector a -> IO [a]
    toList v@(Vec ptr) = do
        allocaArray (size v) $ \ptr' -> do
            cCopy ptr ptr'
            res <- peekArray (size v) ptr'
            return $ map fromCType res

    debug :: Vector a -> IO ()
    debug (Vec ptr) = cDebug ptr

instance Vectorizable Int64 where
    type CType Int64 = CLong
    toCType = fromIntegral
    fromCType = fromIntegral
    constructor = newLongVector
    destructor = longVectorDelete
    cDebug = longVectorShow
    cCopy = longVectorCopy
    cSize = longVectorSize
    cPushBack = longVectorPushBack
    cSet = longVectorSet
    cGet = longVectorGet

instance Vectorizable Int where
    type CType Int = CInt
    toCType = fromIntegral
    fromCType = fromIntegral
    constructor = newIntVector
    destructor = intVectorDelete
    cDebug = intVectorShow
    cCopy = intVectorCopy
    cSize = intVectorSize
    cPushBack = intVectorPushBack
    cSet = intVectorSet
    cGet = intVectorGet

instance Vectorizable Float where
    type CType Float = CFloat
    toCType = realToFrac
    fromCType = realToFrac
    constructor = newFloatVector
    destructor = floatVectorDelete
    cDebug = floatVectorShow
    cCopy = floatVectorCopy
    cSize = floatVectorSize
    cPushBack = floatVectorPushBack
    cSet = floatVectorSet
    cGet = floatVectorGet

instance Vectorizable Word where
    type CType Word = CUInt
    toCType = fromIntegral
    fromCType = fromIntegral
    constructor = newUIntVector
    destructor = uintVectorDelete
    cDebug = uintVectorShow
    cCopy = uintVectorCopy
    cSize = uintVectorSize
    cPushBack = uintVectorPushBack
    cSet = uintVectorSet
    cGet = uintVectorGet


#define declare_vector_instance(CNAME, HSNAME, CTYPE, HTYPE)       \
    foreign import ccall "new_/**/CNAME/**/_intp_int"              \
        new/**/CNAME :: Ptr CTYPE -> CInt -> Ptr (Vec HTYPE);      \
    foreign import ccall "CNAME/**/_delete"                        \
        HSNAME/**/Delete :: Ptr (Vec HTYPE) -> IO ();              \
    foreign import ccall "CNAME/**/_size"                          \
        HSNAME/**/Size :: Ptr (Vec HTYPE) -> CInt;                 \
    foreign import ccall "CNAME/**/_get"                           \
        HSNAME/**/Get :: Ptr (Vec HTYPE) -> CInt -> CTYPE;         \
    foreign import ccall "CNAME/**/_push_back"                     \
        HSNAME/**/PushBack :: Ptr (Vec HTYPE) -> CTYPE -> IO ();   \
    foreign import ccall "CNAME/**/_show"                          \
        HSNAME/**/Show :: Ptr (Vec HTYPE) -> IO ();                \
    foreign import ccall "CNAME/**/_set"                           \
        HSNAME/**/Set :: Ptr (Vec HTYPE) -> CInt -> CTYPE -> IO ();\
    foreign import ccall "CNAME/**/_copy"                          \
        HSNAME/**/Copy :: Ptr (Vec HTYPE) -> Ptr CTYPE -> IO ()

declare_vector_instance(IntVector,intVector,CInt, Int)
declare_vector_instance(LongVector,longVector,CLong, Int64)
declare_vector_instance(FloatVector,floatVector,CFloat, Float)
declare_vector_instance(UIntVector,uintVector,CUInt, Word)
