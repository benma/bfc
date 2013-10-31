{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types
       (
         PositionRef(..)
       , makePositionRef
       , PositionRefOffset(..)
       , (+:)
       , Position(..)
       , Size
       ) where

import qualified Data.Data as Data

import Data.Hashable

-- This type stands for an unknown but unique position on the tape.
-- The type itself does not matter as long as it hashable and unique values can be produced easily.
newtype PositionRef = PositionRef { _position' :: Int }
                  deriving (Eq, Data.Typeable, Data.Data)

instance Show PositionRef where
  show = show . _position'
  
makePositionRef :: Int -> PositionRef
makePositionRef = PositionRef

instance Hashable PositionRef where
  hashWithSalt salt (PositionRef pos) = salt `hashWithSalt` pos

-- This type is used to reference an unknown but unique position on the tape with an offset.
data PositionRefOffset = PositionRefOffset { _position :: PositionRef
                                           , _offset :: Int
                                           , _size :: Size
                                           } deriving (Show, Eq)

infixl 6 +:
(+:) :: PositionRefOffset -> Int -> PositionRefOffset
(PositionRefOffset pos offset size) +: offset' = PositionRefOffset pos (offset + offset') size

type Size = Int

-- This type represents an actual position on the tape.
newtype Position = Position Int
                 deriving (Eq, Num, Hashable)
