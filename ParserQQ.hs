{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module ParserQQ
       (
         ast
       ) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Parser (parseString, AST(..))
import Types (PositionRefOffset(..))

import Data.Generics (extQ)

ast :: QuasiQuoter
ast = QuasiQuoter { quoteExp = dataToExpQ (const Nothing `extQ` antiQuote) . parseString
                  , quotePat = \_ -> fail "illegal string QuasiQuote (allowed as expression only, used as a pattern)"
                  , quoteType = \_ -> fail "illegal string QuasiQuote (allowed as expression only, used as a type)"
                  , quoteDec  = \_ -> fail "illegal string QuasiQuote (allowed as expression only, used as a dec)"
                  }
  where
    antiQuote :: AST -> Maybe (Q Exp)
    antiQuote (AMetaVariable v) = Just [|APosRef $ _position $(dyn v)|]
    antiQuote _ = Nothing
