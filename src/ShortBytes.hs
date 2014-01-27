module ShortBytes
       (
         ShortByteParams
       , getCachedShortByteParams
       ) where

import qualified Data.Binary as Bin
import System.IO (stderr, hPutStrLn)
import System.Directory (doesFileExist)
import Control.Monad (guard, when)
import Control.Applicative ((<$>))
import Data.Ord (comparing)
import Data.List (minimumBy)
import Math.NumberTheory.Moduli (invertMod)
import qualified Data.ByteString.Lazy as BL

type ShortByteParams = (Int, Int, Int, Int, Int)

scoreParams :: ShortByteParams -> Int
scoreParams (a, b, c, d, diff) = if b == 1
                                 then scoreVal d + abs diff
                                 else scoreVal a + scoreVal c + scoreVal d + abs diff



findMin :: Int -> ShortByteParams
findMin 0 = (0, 1, 0, 0, 0)
findMin s = minimumBy (comparing scoreParams) $ do
  loopIterations <- [1..255]
  d <- filter (>= loopIterations) $ modularSolve loopIterations s 256
  c <- [1..255]
  let a = 256 - loopIterations*c `rem` 256
  guard $ a /= 256 -- if a % 256 == 0, the loop would never be executed.
  -- check that there is no loopIterations' < loopIterations which would satisfy (a + loopIterations' * c) mod 256 == 0
  -- if there was one, the current solution couldn't work because the loop would exit after loopIterations' iterations.
  -- the check is equivalent to and faster than
  -- guard $ not $ any id [(a+loopIterations'*c) `rem` 256 == 0 | loopIterations' <- [1..loopIterations-1]]
  guard $ not $ any (< loopIterations) $ drop 1 $ modularSolve c 0 256
  -- this holds: (a + loopIterations * c) mod 256 == 0 && (loopIterations * d) mod 256 == s
  return (a, loopIterations, c, d, 0)

-- returns all solutions to b*x == s (rem m)
modularSolve :: Int -> Int -> Int -> [Int]
modularSolve b s m = let g = gcd b s
                         b' = b `quot` g
                         s' = s `quot` g
                         inv = invertMod (fromIntegral b') (fromIntegral m)
                     in case inv of
                       Just inv' -> let oneSolution = (s' * fromIntegral inv') `rem` m
                                        step = m `quot` gcd b m
                                        smallestSolution = oneSolution - (oneSolution `quot` step) * step
                                    in takeWhile (<m) $ iterate (+step) smallestSolution
                       Nothing -> []


scoreVal :: Int -> Int
scoreVal a = min a (256 - a)

optimizeShortByteParams :: [ShortByteParams] -> [ShortByteParams] 
optimizeShortByteParams params = [go i i_params | (i, i_params) <- zip [0..] params]
  where 
    go :: Int -> ShortByteParams -> ShortByteParams
    go i i_params = let score = scoreParams i_params
            in snd $ minimumBy (comparing fst) $ do
              (p, p_params) <- zip [0..] params
              let diff = i-p
                  newScore = scoreParams p_params + abs diff
              guard $ newScore <= score
              let (a,b,c,d,diff') = p_params
              return (newScore, (a,b,c,d,diff+diff'))



computeShortByteParams :: [ShortByteParams]
computeShortByteParams = map findMin [0..255]

getCachedShortByteParams :: IO [ShortByteParams]
getCachedShortByteParams = do
  exists <- doesFileExist filename
  when (not exists) $ do
    hPutStrLn stderr "Please wait; precomputing short byte parameters. This can take a couple of minutes."
    BL.writeFile filename (Bin.encode $ optimizeShortByteParams computeShortByteParams)
  Bin.decode <$> BL.readFile filename
  where
    filename = ".shortbytes"
