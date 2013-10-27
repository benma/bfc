{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module BFS(parseBfDSL, bf, bfe) where

import BfIR(BfChar(..), fromString)
import qualified Data.Foldable as F
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Text.ParserCombinators.Parsec
import qualified Data.Sequence as S
import Control.Applicative((<$>))

parseBfDSL :: String -> [(String, [BfChar])]
parseBfDSL = either (error . show) id . parse p ""
  where
    p = between spaces eof (many atpos)
    atpos = do
      _ <- char '@'
      spaces
      var <- between (optional $ char '(') (optional $ char ')') (many1 alphaNum)
      spaces
      bfchars <- fromString <$> many (oneOf ".,<>+-[]")
      spaces
      return (var, bfchars)

instance Lift BfChar where
  lift BfDot = [|BfDot|]
  lift BfComma = [|BfComma|]
  lift BfMoveLeft = [|BfMoveLeft|]
  lift BfMoveRight = [|BfMoveRight|]
  lift BfStartLoop = [|BfStartLoop|]
  lift BfEndLoop = [|BfEndLoop|]
  lift BfInc = [|BfInc|]
  lift BfDec = [|BfDec|]

bfe :: QuasiQuoter
bfe = QuasiQuoter { quoteExp = \s -> let result = [ [|$(dyn "atPos") $(dyn p) $ $(dyn "write") $ S.fromList bfR|] | (p, bfR) <- parseBfDSL s]
                                     in F.foldr (\a b -> [| $a >> $b |]) [|return ()|] result
                 , quotePat = \_ -> fail "illegal string QuasiQuote (allowed as expression only, used as a pattern)"
                 , quoteType = \_ -> fail "illegal string QuasiQuote (allowed as expression only, used as a type)"
                 , quoteDec  = \_ -> fail "illegal string QuasiQuote (allowed as expression only, used as a dec)"
                 }



bf :: QuasiQuoter
bf = QuasiQuoter { quoteExp = \s -> let result = fromString s -- compile time
                                    in [|S.fromList result|] -- runtime
                 , quotePat = dataToPatQ (const Nothing) . fromString
                 , quoteType = \_ -> fail "illegal string QuasiQuote (allowed as expression and pattern only, used as a type)"
                 , quoteDec  = \_ -> fail "illegal string QuasiQuote (allowed as expression and pattern only, used as a dec)"
                 }

