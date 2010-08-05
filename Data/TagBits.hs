{-# LANGUAGE CPP, BangPatterns, MagicHash, UnboxedTuples #-}
module Data.TagBits
    (
    -- * Inspecting the tag bits of a closure
      unsafeGetTagBits
    -- * Determining if a closure is evaluated
    , unsafeIsEvaluated
    ) where

#if __GLASGOW_HASKELL__ >= 608
-- dynamic pointer tagging is present on this platform
#define HAS_TAGS
-- and we have an info table we can plunder for information
#define HAS_INFO_TABLE

import Data.Bits ((.&.))
import Data.Functor ((<$>))
import Unsafe.Coerce (unsafeCoerce)
import System.IO.Unsafe (unsafePerformIO)
import Foreign (plusPtr, castPtr, peek)
import GHC.Prim
import GHC.Exts
import Data.Word

#include "ghcconfig.h"

#if SIZEOF_VOID_P == 8
type HalfWord = Word32
#else 
type HalfWord = Word16
#endif

#if i386_BUILD_ARCH
#define TABLES_NEXT_TO_CODE
#define CLOSURE_TYPE_OFFSET SIZEOF_INT
#else
#define CLOSURE_TYPE_OFFSET (SIZEOF_INT * 2)
#endif

getInfoTablePtr :: a -> Ptr HalfWord
getInfoTablePtr i = case unpackClosure# i of
    (# p, _, _ #) -> Ptr p

newtype ClosureType = ClosureType HalfWord

getClosureType :: a -> IO ClosureType
getClosureType a = ClosureType <$> 
    peek (castPtr (getInfoTablePtr a) `plusPtr` CLOSURE_TYPE_OFFSET)

isConstructor :: ClosureType -> Bool
isConstructor (ClosureType i) = i >= 1 && i <= 8

isIndirection :: ClosureType -> Bool 
isIndirection (ClosureType i) = i >= 28 && i <= 32

unsafeIsConstructorOrIndirection :: a -> Bool
unsafeIsConstructorOrIndirection a = unsafePerformIO $ do
    ty <- getClosureType a
    return (isConstructor ty || isIndirection ty)

#endif 

-- | Inspect the dynamic pointer tagging bits of a closure. This is an impure function that relies on GHC internals and may falsely return 0, but should never give the wrong tag number if it returns a non-0 value.
unsafeGetTagBits :: a -> Word
{-# INLINE unsafeGetTagBits #-}
#ifdef HAS_TAGS
unsafeGetTagBits a = unsafeCoerce (Box a) .&. (SIZEOF_VOID_P - 1)
data Box a = Box a
#else
unsafeGetTagBits _ = 0
#endif

-- | Returns a guess as to whether or not a value has been evaluated. This is an impure function that relies on GHC internals and will return false negatives, but no false positives. This is unsafe as the value of this function will vary (from False to True) over the course of otherwise pure invocations! This first attempts to check the tag bits of the pointer, and then falls back on inspecting the info table for the closure type.
unsafeIsEvaluated :: a -> Bool
{-# INLINE unsafeIsEvaluated #-}
unsafeIsEvaluated a = unsafeGetTagBits a > 0 
#ifdef HAS_INFO_TABLE
                   || unsafeIsConstructorOrIndirection a
#endif
