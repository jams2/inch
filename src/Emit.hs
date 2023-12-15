{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Emit
  ( Emit (..),
    emit,
    indent,
  )
where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import System.IO

class Emit a where
  toText :: a -> T.Text

emit :: (Emit a) => Handle -> a -> IO ()
emit h x = BS.hPut h (toByteString x <> "\n")
  where
    toByteString = encode . toText

indent :: T.Text -> T.Text
indent = (<>) "    "

encode :: T.Text -> BS.ByteString
encode = TE.encodeUtf8
