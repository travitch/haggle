module Data.Graph.Haggle.Internal.BitSet (
  BitSet,
  newBitSet,
  setBit,
  testBit,
  setBitUnsafe,
  testBitUnsafe
  ) where

import Control.Monad.ST
import qualified Data.Bits as B
import Data.Vector.Unboxed.Mutable ( STVector )
import qualified Data.Vector.Unboxed.Mutable as V
import Data.Word ( Word64 )

-- Note that the implementation here assumes thaththe bit numbers are all
-- unsigned.  A proper implementation would perhaps use 'Natural' instead of
-- 'Int', but that would require gratuitous fromEnum/toEnum conversions from all
-- the other API's that just use 'Int', which has about a 33% performance impact
-- when measured.
--
-- The 'setBit' and 'testBit' operations use V.unsafeRead instead of V.read
-- (where the latter is roughly 25% slower) because this is an internal module
-- that is generally always used with a positive 'Int' value, and the value is
-- also checked against 'sz' (which is also probably superfluous).  In other
-- words, this module prioritizes performance over robustness and should only be
-- used when the caller can guarantee positive Int values and otherwise good
-- behavior.


data BitSet s = BS (STVector s Word64) {-# UNPACK #-} !Int

bitsPerWord :: Int
bitsPerWord = 64

-- | Allocate a new 'BitSet' with @n@ bits.  Bits are all
-- initialized to zero.
--
-- > bs <- newBitSet n
newBitSet :: Int -> ST s (BitSet s)
newBitSet n = do
  let nWords = (n `div` bitsPerWord) + 1
  v <- V.replicate nWords 0
  return $ BS v n

-- | Set a bit in the bitset.  Out of range has no effect.
setBit :: BitSet s -> Int -> ST s ()
setBit b@(BS _ sz) bitIx
  | bitIx >= sz = return ()
  | bitIx < 0 = return ()
  | otherwise = setBitUnsafe b bitIx

-- |  Set a bit in the bitset.  The specified bit must be in range.
setBitUnsafe :: BitSet s -> Int -> ST s ()
setBitUnsafe (BS v _) bitIx = do
    let wordIx = bitIx `div` bitsPerWord
        bitPos = bitIx `mod` bitsPerWord
    oldWord <- V.unsafeRead v wordIx
    let newWord = B.setBit oldWord bitPos
    V.write v wordIx newWord

-- | Return True if the bit is set.  Out of range will return False.
testBit :: BitSet s -> Int -> ST s Bool
testBit b@(BS _ sz) bitIx
  | bitIx >= sz = return False
  | bitIx < 0 = return False
  | otherwise = testBitUnsafe b bitIx

-- | Return True if the bit is set.  The specified bit must be in range.
testBitUnsafe :: BitSet s -> Int -> ST s Bool
testBitUnsafe (BS v _) bitIx = do
    let wordIx = bitIx `div` bitsPerWord
        bitPos = bitIx `mod` bitsPerWord
    w <- V.unsafeRead v wordIx
    return $ B.testBit w bitPos
