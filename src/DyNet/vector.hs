{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module DyNet.Vector (
  Int64,
  -- Word,
  -- Vec(..),
  Vector(..),
  (!),
  constructor,
  pushBack,
  size,
  debug,
  insert,
  fromList
) where

import DyNet.Internal.Vector
import Data.Int ( Int64 )



    --
    --
    -- toList :: Vector a -> IO [a]
    -- toList v@(Vec ptr) = do
    --     allocaArray (size v) $ \ptr' -> do
    --         cCopy ptr ptr'
    --         res <- peekArray (size v) ptr'
    --         return $ map fromCType res


-- import Foreign.C.Types       ( CInt(..), CChar(..), CFloat(..), CLong(..), CUInt(..) )
-- import Foreign.Ptr           ( Ptr(..) )
-- import Foreign.C.String
-- import Foreign.Marshal
-- import Foreign.Storable
-- import Data.Int
-- import Data.Word
-- 
-- 
-- data Expression
-- data Vec a
-- data Vector a = Vec { getPtr :: Ptr (Vec a) }
-- 
-- 
-- instance Vectorizable Int64 where
--     type CType Int64 = CLong
--     toCType = fromIntegral
--     fromCType = fromIntegral
--     constructor = newLongVector
--     destructor = longVectorDelete
--     cDebug = longVectorShow
--     cCopy = longVectorCopy
--     cSize = longVectorSize
--     cPushBack = longVectorPushBack
--     cSet = longVectorSet
--     cGet = longVectorGet
-- 
-- 
-- instance Vectorizable Float where
--     type CType Float = CFloat
--     toCType = realToFrac
--     fromCType = realToFrac
--     constructor = newFloatVector
--     destructor = floatVectorDelete
--     cDebug = floatVectorShow
--     cCopy = floatVectorCopy
--     cSize = floatVectorSize
--     cPushBack = floatVectorPushBack
--     cSet = floatVectorSet
--     cGet = floatVectorGet
-- 
-- instance Vectorizable Word where
--     type CType Word = CUInt
--     toCType = fromIntegral
--     fromCType = fromIntegral
--     constructor = newUIntVector
--     destructor = uintVectorDelete
--     cDebug = uintVectorShow
--     cCopy = uintVectorCopy
--     cSize = uintVectorSize
--     cPushBack = uintVectorPushBack
--     cSet = uintVectorSet
--     cGet = uintVectorGet
-- 
-- 
-- 
-- 
-- #define declare_vector_instance(CNAME, HSNAME, CTYPE, HTYPE)       \
--     foreign import ccall "new_/**/CNAME/**/_intp_int"              \
--         new/**/CNAME :: Ptr CTYPE -> CInt -> Ptr (Vec HTYPE);      \
--     foreign import ccall "delete_/**/CNAME"                        \
--         HSNAME/**/Delete :: Ptr (Vec HTYPE) -> IO ();              \
--     foreign import ccall "CNAME/**/_size"                          \
--         HSNAME/**/Size :: Ptr (Vec HTYPE) -> CInt;                 \
--     foreign import ccall "CNAME/**/_get"                           \
--         HSNAME/**/Get :: Ptr (Vec HTYPE) -> CInt -> CTYPE;         \
--     foreign import ccall "CNAME/**/_push_back"                     \
--         HSNAME/**/PushBack :: Ptr (Vec HTYPE) -> CTYPE -> IO ();   \
--     foreign import ccall "CNAME/**/_show"                          \
--         HSNAME/**/Show :: Ptr (Vec HTYPE) -> IO ();                \
--     foreign import ccall "CNAME/**/_set"                           \
--         HSNAME/**/Set :: Ptr (Vec HTYPE) -> CInt -> CTYPE -> IO ();\
--     foreign import ccall "CNAME/**/_copy"                          \
--         HSNAME/**/Copy :: Ptr (Vec HTYPE) -> Ptr CTYPE -> IO ()
-- 
-- declare_vector_instance(IntVector,intVector,CInt, Int)
-- declare_vector_instance(LongVector,longVector,CLong, Int64)
-- declare_vector_instance(FloatVector,floatVector,CFloat, Float)
-- declare_vector_instance(UIntVector,uintVector,CUInt, Word)
-- -- declare_vector_instance(ExpressionVector,exprVector,Ptr (), Expression)
