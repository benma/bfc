{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

import Parser(parseString)
import CodeGenerator(compile, optimizeOutput)
import BFS(bf, parseBfDSL)
import BfIR(BfChar(..), toString)
import qualified Data.Sequence as S

import Control.Applicative((<$>))

import qualified Data.ByteString.Char8 as BC

import ShortBytes(getCachedShortByteParams)

main :: IO ()
main = do
  shortByteParams <- getCachedShortByteParams
  (toString . compile shortByteParams . parseString) <$> getContents >>= BC.putStrLn

-- main = print $ optimizeOutput [bf|[++<--++>---+-][-]|]
-- main :: IO ()
-- main = (show . parseString) <$> getContents >>= putStrLn
