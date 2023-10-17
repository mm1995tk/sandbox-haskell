{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Examples.ConduitExample where

import Conduit
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C8
import Data.Conduit.Combinators qualified as C
import System.IO (stdin, stdout)

inputToOutput :: ConduitT () Void IO ()
inputToOutput = input .| C.takeWhile (\c -> stripTrailingWhitespace c /= "quit") .| conduit .| output

conduit :: ConduitT ByteString ByteString IO ()
conduit = do
  mInput <- await
  case mInput of
    Nothing -> return ()
    Just v -> do
      yield $ "input: " <> v
      conduit

exec :: IO ()
exec = do
  runConduit inputToOutput

input :: ConduitT () ByteString IO ()
input = sourceHandle stdin

output :: ConduitT ByteString Void IO ()
output = sinkHandle stdout

stripTrailingWhitespace :: ByteString -> ByteString
stripTrailingWhitespace = C8.dropWhileEnd (`elem` [' ', '\t', '\n', '\r'])
