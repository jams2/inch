{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Emit (Emit (..), emit, indent) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import System.IO

class Emit a where
  toByteString :: a -> BS.ByteString

emit :: (Emit a) => Handle -> a -> IO ()
emit h x = BS.hPut h (toByteString x) >> BS.hPut h "\n"

indent :: T.Text -> T.Text
indent = (<>) "    "
