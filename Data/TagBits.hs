{-# LANGUAGE CPP, BangPatterns, MagicHash #-}
module Data.TagBits
    (
    -- * Determining if a closure is evaluated
      unsafeGetTagBits
    , unsafeIsEvaluated
    ) where

#if __GLASGOW_HASKELL__ >= 608
-- import Data.Bits ((.&.))
-- import Unsafe.Coerce (unsafeCoerce)
import Foreign (sizeOf)
import GHC.Prim
import GHC.Types
import GHC.Word
-- dynamic pointer tagging is present on this platform
#define TAGGED
#endif

-- | Inspect the dynamic pointer tagging bits of a closure. This is an impure function that relies on GHC internals and may falsely return 0, but should never give the wrong tag number if it returns a non-0 value.
unsafeGetTagBits :: a -> Word
{-# INLINE unsafeGetTagBits #-}
#ifndef TAGGED
unsafeGetTagBits _ = 0
#else
unsafeGetTagBits a = W# (and# (unsafeCoerce# a) (int2Word# mask#))
    where 
        !(I# mask#) = sizeOf (undefined :: Int) - 1

-- unsafeGetTagBits a = unsafeCoerce (Box a) .&. (sizeOf (undefined :: Word) - 1)
-- data Box a = Box a
#endif

-- | Returns a guess as to whether or not a value has been evaluated. This is an impure function that relies on GHC internals and will return false negatives, but no false positives. This is unsafe as the value of this function will vary (from False to True) over the course of otherwise pure invocations!
unsafeIsEvaluated :: a -> Bool
unsafeIsEvaluated a = and# (unsafeCoerce# a) (int2Word# mask#) `gtWord#` int2Word# 0#
    where 
        !(I# mask#) = sizeOf (undefined :: Int) - 1
{-# INLINE unsafeIsEvaluated #-}
