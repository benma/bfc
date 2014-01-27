{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module BfQQ
       (
         parseBfDSL
       , bf
       , bfe
       ) where

import BfIR (BfS, fromString)
import qualified Data.Foldable as F
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.ParserCombinators.Parsec
import Control.Applicative((<$>))

parseBfDSL :: String -> [(String, BfS)]
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

bfe :: QuasiQuoter
bfe = QuasiQuoter { quoteExp = \s -> let result = [ [|$(dyn "atPos") $(dyn p) $ $(dyn "write") $ bfR|] | (p, bfR) <- parseBfDSL s]
                                     in F.foldr (\a b -> [| $a >> $b |]) [|return ()|] result
                 , quotePat = \_ -> fail "illegal string QuasiQuote (allowed as expression only, used as a pattern)"
                 , quoteType = \_ -> fail "illegal string QuasiQuote (allowed as expression only, used as a type)"
                 , quoteDec  = \_ -> fail "illegal string QuasiQuote (allowed as expression only, used as a dec)"
                 }



bf :: QuasiQuoter
bf = QuasiQuoter { quoteExp = \s -> let result = fromString s -- compile time
                                    in [|result|] -- runtime
                 , quotePat = dataToPatQ (const Nothing) . fromString
                 , quoteType = \_ -> fail "illegal string QuasiQuote (allowed as expression and pattern only, used as a type)"
                 , quoteDec  = \_ -> fail "illegal string QuasiQuote (allowed as expression and pattern only, used as a dec)"
                 }

