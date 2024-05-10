{-# LANGUAGE MagicHash #-}
module Data.Bson.Size (sizeOfDocument) where

import Data.Bson
import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.Text.Unsafe (lengthWord8)
import GHC.Num.Integer (integerSizeInBase#)
import GHC.Word (Word (..))

-- | Size in bytes of the BSON document when serialized to binary format.
sizeOfDocument :: Document -> Int
sizeOfDocument doc = sum (map sizeOfField doc) + 5
-- Additional 5 bytes = 4 bytes (size) + 1 byte (null terminator)

sizeOfField :: Field -> Int
sizeOfField (k := v) = sizeOfLabel k + sizeOfValue v + 1
-- Additional 1 byte = element type

sizeOfLabel :: Text -> Int
sizeOfLabel = sizeOfCString

sizeOfValue :: Value -> Int
sizeOfValue v = case v of
  Float _ -> 8 -- 64-bit binary floating point
  String x -> sizeOfString x
  Doc x -> sizeOfDocument x
  Array x -> sizeOfArray x
  Bin (Binary x) -> sizeOfBinary x
  Fun (Function x) -> sizeOfBinary x
  Uuid (UUID x) -> sizeOfBinary x
  Md5 (MD5 x) -> sizeOfBinary x
  UserDef (UserDefined x) -> sizeOfBinary x
  ObjId _ -> 12
  Bool _ -> 1
  UTC _ -> 8
  Null -> 0
  RegEx (Regex pattern opts) -> sizeOfCString pattern + sizeOfCString opts
  JavaScr (Javascript [] code) -> sizeOfString code
  JavaScr (Javascript env code) -> sizeOfClosure env code -- "code with scope"
  Sym (Symbol x) -> sizeOfString x
  Int32 _ -> 4
  Int64 _ -> 8
  Stamp _ -> 8
  MinMax _ -> 0

sizeOfCString :: Text -> Int
sizeOfCString t = lengthWord8 t + 1
-- Additional 1 byte = null terminator

sizeOfString :: Text -> Int
sizeOfString t = sizeOfCString t + 4
-- Additional 4 bytes = size of string

sizeOfClosure :: Document -> Text -> Int
sizeOfClosure env code = 4 + sizeOfDocument env + sizeOfString code

sizeOfBinary :: BS.ByteString -> Int
sizeOfBinary x = 5 + BS.length x
-- Additional 5 bytes = 4 bytes (size) + 1 byte (subtype)

sizeOfArray :: [Value] -> Int
sizeOfArray vs = 5 + sum (zipWith go [0..] vs)
-- Additional 5 bytes = 4 bytes (size) + 1 byte (null terminator)
  where
    go i v = 2 + intLen i + sizeOfValue v
    -- Additional 2 bytes = 1 byte (element type) + 1 byte (null terminator of key name)

    -- Length of the string representing the integer in base 10
    intLen 0 = 1
    intLen i = fromIntegral $ W# (integerSizeInBase# 10## i)
