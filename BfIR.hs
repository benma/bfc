{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
module BfIR
       (
         BfChar(..)
       , BfS
       , BfIR(..)
       , toString
       , fromString
       ) where

import Language.Haskell.TH.Syntax (Lift(..))

import Types (PositionRefOffset(..))
import Data.Data (Typeable, Data)
import qualified Data.Foldable as F
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Maybe (fromJust)

data BfChar = BfDot
            | BfComma
            | BfMoveLeft
            | BfMoveRight
            | BfInc
            | BfDec
            | BfStartLoop
            | BfEndLoop
            deriving (Eq, Show, Typeable, Data)

instance Lift BfChar where
  lift BfDot = [|BfDot|]
  lift BfComma = [|BfComma|]
  lift BfMoveLeft = [|BfMoveLeft|]
  lift BfMoveRight = [|BfMoveRight|]
  lift BfStartLoop = [|BfStartLoop|]
  lift BfEndLoop = [|BfEndLoop|]
  lift BfInc = [|BfInc|]
  lift BfDec = [|BfDec|]

type BfS = [BfChar]

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
toString = F.foldMap (BLC.singleton . bfsToChar)

fromString :: String -> BfS
fromString = map (fromJust . charToBfS)
