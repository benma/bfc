{-# LANGUAGE OverloadedStrings #-}

import Parser(parseString)
import CodeGenerator(compile)

import qualified Data.Attoparsec as A
import qualified Data.Attoparsec.Char8 as AC
import Control.Monad
import Control.Applicative((<$>), (<|>), (<*), (*>))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
  

-- remove all balanced <> and >< and -+ and +-
optimizeOutput :: BC.ByteString -> BC.ByteString
optimizeOutput = (parse'
                  $ liftM BS.concat
                  $ A.many'
                  $ A.try (A.many1 (AC.char '[' >> (A.option "" skipNested) >> AC.char '-' >> (A.option "" skipNested) >> AC.char ']' >> (A.option "" skipNested)) >> return "[-]")
                  <|> skipNested
--                  <|> A.try skipTrailing
                  <|> BC.singleton <$> AC.anyChar
                 )
  where
    parse' p = either (error . show) id . A.parseOnly p 
    skipNested = A.try $ A.choice $ map balanced [('<','>'),('>','<'),('+','-'),('-','+')]
    balanced (a,b) = between (AC.char a) (AC.char b) $ A.option "" skipNested
    between left right f = left *> f <* right
                       -- all code after last '.' or ',' is irrelevant
--    skipTrailing = A.many1 (A.choice $ map AC.char "<>+-") >> A.endOfInput >> return ""
    
main :: IO ()
main = (optimizeOutput . compile . parseString) <$> getContents >>= BC.putStrLn

-- main :: IO ()
-- main = (show . parseString) <$> getContents >>= putStrLn
