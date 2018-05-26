module Data.VarInt(getVarInt, putVarInt) where

import           Data.Bits      (Bits (..))
import           Data.Serialize

getVarInt :: Get Int
getVarInt = do
  x <- fromIntegral <$> getWord8
  if x .&. 0x80 == 0
    then pure x
    else do
      rest <- getVarInt
      pure $ (rest `shiftL` 7) .|. (x .&. 0x7F)

putVarInt :: Int -> Put
putVarInt x
  | x < 0     = error "Negative integers not yet supported"
  | x < 0x80  = putWord8 $ fromIntegral x
  | otherwise = do
      putWord8 . fromIntegral $ 0x80 .|. (x .&. 0x7F)
      putVarInt (x `shiftR` 7)
