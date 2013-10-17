{-# LANGUAGE TemplateHaskell #-}
module TapeAllocation(TapeAccessSequence(..),Allocation,findAllocation) where
import Data.Monoid
import Control.Lens
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Data.Hashable(Hashable)

import Data.List((\\))
import qualified Data.Sequence as S
import qualified Data.Foldable as F

import Control.Monad.Tardis

import Types(Size, Position)

data TapeAccessSequence a = Start a Size | End a | StartLoop | EndLoop
 
type Allocation a = Map.HashMap a Position

data AnnotateEndState a = AnnotateEndState { _loopLevel :: Int
                                           , _firstOccurrences :: Map.HashMap a Int
                                           , _lingering :: Map.HashMap Int (Set.HashSet a)
                                           }
             
makeLenses ''AnnotateEndState

annotateEnd :: (Eq a, Hashable a) => S.Seq (TapeAccessSequence a) -> S.Seq (TapeAccessSequence a)
annotateEnd xs = flip evalTardis (Set.empty, ()) . execWriterT . evalStateT go $ AnnotateEndState 0 Map.empty Map.empty
  where
    go = F.for_ xs $ \el -> 
      case el of
        StartLoop -> loopLevel += 1
        EndLoop -> do { loopLevel' <- loopLevel <-= 1
                      ; lingering' <- gets (^? lingering . ix loopLevel')
                      ; case lingering' of
                        Nothing -> return ()
                        Just xs' -> do { F.for_ xs' tellEnd
                                       ; lingering %= Map.delete loopLevel'
                                       }
                      }
        Start a _ -> do { tell' $ S.singleton el
                        ; liftTardis $ modifyBackwards (Set.insert a)
                        ; loopLevel' <- use loopLevel
                        ; firstOccurrences %= Map.insertWith (const id) a loopLevel'
                        ; firstOccurrenceLevel <- uses firstOccurrences (^?! ix a)
                        ; if firstOccurrenceLevel == loopLevel'
                          then tellEnd a
                          else lingering %= (Map.insertWith mappend firstOccurrenceLevel $ Set.singleton a)
                        }
        End _ -> error "annotateEnd was passed list already containing an End"
      where
        tell' = lift . tell
        liftTardis = lift . lift
        tellEnd a = do isLastOccurrence <- liftTardis $ getsFuture (not . Set.member a)
                       tell' $ if isLastOccurrence then S.singleton (End a) else mempty

data AllocationState a = AllocationState { _allocation :: Allocation a
                                         , _liveVars :: Map.HashMap a [Position]
                                         }
makeLenses ''AllocationState

findAllocation :: (Hashable a, Eq a) => S.Seq (TapeAccessSequence a) -> Allocation a
findAllocation tapeAccessSequence = flip evalState initialState $ findAllocation' (F.toList $ annotateEnd tapeAccessSequence)
  where
    initialState = AllocationState Map.empty Map.empty
    findAllocation' [] = use allocation
    findAllocation' (x:xs) = do
      liveVars' <- use liveVars
      allocation' <- use allocation
      case x of
        Start v size -> case Map.lookup v allocation' of
          Just pos -> return ()
          Nothing -> let freePositions = [0..] \\ (concat $ Map.elems liveVars')
                         pos = findSeqOfLength freePositions size
                     in do { allocation %= Map.insert v pos
                           ; liveVars %= Map.insert v [pos..pos+size-1]
                           }
        End v -> liveVars %= Map.delete v
      findAllocation' xs

    findSeqOfLength xs size = go xs Nothing size size
      where 
        go _ (Just r) _ 0 = r
        go (x1:x2:xs) result size size' | size == 1 || x1 + 1 == x2 =
          case result of
            Nothing -> go (x2:xs) (Just x1) size (size' - 1)
            a -> go (x2:xs) a size (size' - 1)
                                        | otherwise = go (x2:xs) Nothing size size
    


