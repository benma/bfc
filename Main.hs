{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

import Parser(parseString)
import CodeGenerator(compile)
import BfIR(toString)

import Control.Applicative((<$>))

import qualified Data.ByteString.Lazy.Char8 as BLC

import ShortBytes(getCachedShortByteParams)

main :: IO ()
main = do
  shortByteParams <- getCachedShortByteParams
  (toString . compile shortByteParams . parseString) <$> getContents >>= BLC.putStrLn

--main = print $ length $ optimizeOutput (replicate 100000000 $ BfDot) 
-- main :: IO ()
-- main = (show . parseString) <$> getContents >>= putStrLn
