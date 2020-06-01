module Util
  ( splitOpcode
  , mergeOpcode
  , chunksOf
  ) where

import           Data.Bits (rotateL, shiftL, (.&.), (.|.))
import           Data.Word (Word16, Word8)

-- | Split a two-byte opcode into four parts.
splitOpcode :: Word16 -> (Word8, Word8, Word8, Word8)
splitOpcode x = (extract 0, extract 1, extract 2, extract 3)
  where extract n = fromIntegral $ rotateL x ((n + 1) * 4) .&. 0xf

-- | Merge two bytes into an opcode.
mergeOpcode :: [Word8] -> Word16
mergeOpcode [h, l] = shiftL (fromIntegral h) 8 .|. fromIntegral l
-- TODO: Other cases may be better treated as real errors.
mergeOpcode [h]    = shiftL (fromIntegral h) 8
mergeOpcode _      = 0xffff

-- | Divide a list into a list of small chunks of the given size.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (y, xs') = splitAt n xs
                in y : chunksOf n xs'
