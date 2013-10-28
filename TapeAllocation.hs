{-# LANGUAGE TemplateHaskell #-}
module TapeAllocation(TapeAccessSequence(..), Allocation, findAllocation) where
import Control.Lens
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Data.Hashable(Hashable)

import Data.List((\\))
import qualified Data.DList as D
import qualified Data.Foldable as F

import Control.Monad.Tardis

import Types(Size, Position(..))

data TapeAccessSequence a = Access a Size | StartLoop | EndLoop
data TapeAccessSequenceNoLoops a = Start' a Size | End' a
 
type Allocation a = Map.HashMap a Position

data AnnotateEndState a = AnnotateEndState { _loopLevel :: Int
                                           , _firstOccurrences :: Map.HashMap a Int
                                           , _lingering :: Map.HashMap Int (Set.HashSet a)
                                           }
             
makeLenses ''AnnotateEndState

annotateEnd :: (Eq a, Hashable a, F.Foldable t) => t (TapeAccessSequence a) -> [TapeAccessSequenceNoLoops a]
annotateEnd xs = D.toList . flip evalTardis initialTardis . execWriterT . flip evalStateT initialState $ go
  where
    initialTardis = (Set.empty, ())
    initialState = AnnotateEndState 0 Map.empty Map.empty
    go = F.for_ xs $ \el -> 
      case el of
        StartLoop -> loopLevel += 1
        EndLoop -> do { loopLevel' <- loopLevel <-= 1
                      ; lingering' <- use lingering
                      ; lingering' ^! at loopLevel' . _Just . folded . act tellEnd
                      ; lingering %= Map.delete loopLevel'
                      }
        Access v size -> do { tell' $ D.singleton (Start' v size)
                            ; liftTardis $ modifyBackwards (Set.insert v)
                            ; loopLevel' <- use loopLevel
                            ; firstOccurrences %= Map.insertWith (const id) v loopLevel'
                            ; firstOccurrenceLevel <- uses firstOccurrences (^?! ix v)
                            ; if firstOccurrenceLevel == loopLevel'
                              then tellEnd v
                              else lingering %= (Map.insertWith mappend firstOccurrenceLevel $ Set.singleton v)
                            }
      where
        tell' = lift . tell
        liftTardis = lift . lift
        tellEnd a = do isLastOccurrence <- liftTardis $ getsFuture (not . Set.member a)
                       tell' $ if isLastOccurrence then D.singleton (End' a) else mempty

findAllocation :: (Hashable a, Eq a, F.Foldable t) => t (TapeAccessSequence a) -> Allocation a
findAllocation tapeAccessSequence = findAllocation' (F.toList $ annotateEnd tapeAccessSequence) Map.empty Map.empty
  where
    findAllocation' [] allocation _ = allocation
    findAllocation' (x:xs) allocation liveVars = 
      case x of
        Start' v size -> case allocation ^. at v of
          Nothing -> let pos = findSeqOfLength size $ [0..] \\ (concat $ Map.elems liveVars)
                     in findAllocation' xs (Map.insert v (Position pos) allocation) (Map.insert v [pos..pos+size-1] liveVars)
          _ -> findAllocation' xs allocation liveVars
        End' v -> findAllocation' xs allocation (Map.delete v liveVars)

    findSeqOfLength size xs = go xs Nothing size
      where 
        go _ (Just r) 0 = r
        go (x1:x2:xs') result size' = if size == 1 || x1 + 1 == x2
                                      then case result of
                                        Nothing -> go (x2:xs') (Just x1) (size' - 1)
                                        just -> go (x2:xs') just (size' - 1)
                                      else go (x2:xs') Nothing size
        go _ _ _ = undefined -- should never happen on an infinite list
