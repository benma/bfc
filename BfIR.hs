{-# LANGUAGE DeriveDataTypeable #-}
module BfIR(BfChar(..), BfS, BfIR(..), toString, fromString) where

import Types(PositionRefOffset(..))
import Data.Data(Typeable, Data)
import qualified Data.Foldable as F
import qualified Data.Sequence as S
import qualified Data.ByteString.Lazy.Char8 as BLC
import Control.Applicative(pure)
import Data.Maybe(fromJust)

data BfChar = BfDot
            | BfComma
            | BfMoveLeft
            | BfMoveRight
            | BfInc
            | BfDec
            | BfStartLoop
            | BfEndLoop
            deriving (Eq, Show, Typeable, Data)

type BfS = S.Seq BfChar

data BfIR = AtPos PositionRefOffset BfS


charToBfS :: Char -> Maybe BfChar
charToBfS '.' = Just BfDot
charToBfS ',' = Just BfComma
charToBfS '<' = Just BfMoveLeft
charToBfS '>' = Just BfMoveRight
charToBfS '+' = Just BfInc
charToBfS '-' = Just BfDec
charToBfS '[' = Just BfStartLoop
charToBfS ']' = Just BfEndLoop
charToBfS _ = Nothing -- error ("Could not parse `" ++ [a] ++ "`")
 
bfsToChar :: BfChar -> Char
bfsToChar BfDot = '.' 
bfsToChar BfComma = ',' 
bfsToChar BfMoveLeft = '<' 
bfsToChar BfMoveRight = '>' 
bfsToChar BfInc = '+' 
bfsToChar BfDec = '-' 
bfsToChar BfStartLoop = '[' 
bfsToChar BfEndLoop = ']' 


toString :: (F.Foldable t) => t BfChar -> BLC.ByteString
toString = BLC.pack . F.foldMap (pure . bfsToChar)

fromString :: String -> [BfChar]
--fromString = F.foldMap (S.singleton . fromJust . charToBfS)
fromString = map (fromJust . charToBfS)
