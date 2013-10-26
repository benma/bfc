{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

import Parser(parseString)
import CodeGenerator(compile)
import BFS(bf, parseBfDSL)
import BfIR(BfChar(..), toString)
import qualified Data.Sequence as S
import Data.Sequence(ViewR(..), ViewL(..), (|>))
import Control.Applicative((<$>))

import qualified Data.ByteString.Char8 as BC

import ShortBytes(getCachedShortByteParams)

optimizeOutput :: S.Seq BfChar -> S.Seq BfChar
optimizeOutput = go S.empty
  where
    go stack (S.null -> True) = stack
    -- [-][-] => [-]
    go stack@(stripPostfix [bf|[-]|] -> Just _) (stripPrefix [bf|[-]|] -> Just xs) = go stack xs
    -- remove all balanced <> and >< and -+ and +-
    go (S.viewr -> ss :> s) (S.viewl -> x :< xs)
      | any (\(l,r) -> (s == l && x == r)) [(BfMoveLeft, BfMoveRight),(BfMoveRight,BfMoveLeft),(BfInc,BfDec),(BfDec,BfInc)] = go ss xs
    go stack (S.viewl -> x :< xs) = go (stack |> x) xs
    go _ _ = error "impossible"
    stripPrefix cs xs = let (left, right) = S.splitAt (S.length cs) xs
                                        in if left == cs then Just right else Nothing
    stripPostfix cs xs = let (left, right) = S.splitAt (S.length xs - S.length cs) xs
                                         in if right == cs then Just left else Nothing

main :: IO ()
main = do
  shortByteParams <- getCachedShortByteParams
  (toString . optimizeOutput . compile shortByteParams . parseString) <$> getContents >>= BC.putStrLn

-- main :: IO ()
-- main = (show . parseString) <$> getContents >>= putStrLn
