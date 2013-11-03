{-# LANGUAGE DeriveDataTypeable #-}
module Parser
       (
         AST(..)
       , Op(..)
       , parseString
       ) where

import qualified Data.Data as Data

import qualified Control.Lens as L
import Data.Data.Lens (uniplate)
import Text.ParserCombinators.Parsec hiding ((<|>))
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (emptyDef, identLetter)

import Control.Monad
import Control.Applicative

import Types(PositionRef, makePositionRef)


data Op = OpPlus
        | OpMinus
        | OpMul
        | OpAssignment
        | OpInc
        | OpDec
        | OpPlusEq
        | OpMinusEq 
        | OpNeg
        | OpAnd
        | OpOr
        | OpEq
        | OpIntegerEq
        | OpNeq
        | OpLe
        | OpLeq
        | OpGe
        | OpGeq
        deriving (Show, Data.Typeable, Data.Data)

type VarName = String
data AST = AProgram [AST]
         | AVariable VarName
         | AMetaVariable VarName
         | APosRef PositionRef
         | AExpression AST
         | AInteger Integer
         | AString String
         | ABool Bool
         | AInfixOp Op AST AST
         | APostfixOp Op AST
         | APrefixOp Op AST
         | ASliceOp AST AST -- a[b] -> ASliceOp a b
         | APrint AST
         | AIf AST [AST] (Maybe [AST])
         | AWhile AST [AST]
         | ADef VarName [VarName] [AST]
         | ADefCall VarName [AST] -- defname, [AExpr]
         deriving (Show, Data.Typeable, Data.Data)

-- instance Lift AST where
--   lift _ = error "HU"

instance L.Plated AST where
  plate = uniplate

parseString :: String -> AST
parseString = either (error . show) id . parse init' "" 
  where
    init' = between _whiteSpace eof program
    program = liftM AProgram instructions
    instructions = many1 instruction
    instruction = print' <|> stmtIf <|> stmtWhile <|> function <|> expression
    expression = buildExpressionParser table factor
      where table = [[Postfix (_brackets expression >>= (\expr -> return (\a -> AExpression $ ASliceOp a expr)))]
                    ,[postfixOp ("++", OpInc), postfixOp ("--", OpDec), prefixOp ("!", OpNeg)]
                    ,[infixOp ("*", OpMul) AssocLeft]
                    ,[infixOp ("+", OpPlus) AssocLeft, infixOp ("-", OpMinus) AssocLeft]
                    ,[infixOp ("<", OpLe) AssocLeft, infixOp ("<=", OpLeq) AssocLeft, infixOp (">", OpGe) AssocLeft, infixOp (">=", OpGeq) AssocLeft]
                    ,[infixOp ("i==", OpIntegerEq) AssocLeft, infixOp ("==", OpEq) AssocLeft, infixOp ("!=", OpNeq) AssocLeft]
                    ,[infixOp ("&&", OpAnd) AssocLeft]
                    ,[infixOp ("||", OpOr) AssocLeft]
                    ,[infixOp ("=", OpAssignment) AssocLeft, infixOp ("+=", OpPlusEq) AssocLeft, infixOp ("-=", OpMinusEq) AssocLeft]
                    ]
              where infixOp (op, opr) assoc = Infix (reservedOp op *> pure (\a b -> AExpression $ AInfixOp opr a b)) assoc
                    postfixOp (op, opr) = Postfix (reservedOp op *> pure (\a -> AExpression $ APostfixOp opr a))
                    prefixOp (op, opr) = Prefix (reservedOp op *> pure (\a -> AExpression $ APrefixOp opr a))
            factor = simple_expression <|> _parens expression
    simple_expression = AExpression <$> choice [try defCall <|> variable, try posref <|> metaVariable, integer, bool, stringLiteral]
    variable = AVariable <$> _identifier
    metaVariable = AMetaVariable <$> (string "$" *> _identifier)
    posref = (APosRef . makePositionRef . fromIntegral) <$> (string "$" *> _braces _decimal)
    integer = AInteger <$> _integer
    bool = ABool <$> choice [reserved "true" *> return True, reserved "false" *> return False]
    stringLiteral = AString <$> _stringLiteral
    defCall = ADefCall <$> _identifier <*> (_parens $ _commaSep expression)
    print' = reserved "print" *> (APrint <$> expression)
    stmtIf = reserved "if" *> (AIf <$> (_parens expression) <*> (_braces instructions) <*> stmtElse)
      where stmtElse = option Nothing $ try $ reserved "else" *> (Just <$> _braces instructions)
    
    stmtWhile = reserved "while" >> liftM2 AWhile (_parens expression) (_braces instructions)
    function = reserved "def" *> (ADef <$> _identifier <*> (_parens $ _commaSep _identifier) <*> _braces instructions)

    languageDef = emptyDef { P.commentStart = "/*" 
                           , P.commentEnd = "*/" 
                           , P.commentLine = "//" 
                           , P.reservedNames = ["int", "string", "print", "if", "else", "while", "def", "true", "false"]
                           }

    lexer = P.makeTokenParser languageDef
    _whiteSpace = P.whiteSpace lexer
    reservedOp = P.reservedOp lexer
    _integer = P.integer lexer
    _commaSep = P.commaSep lexer
    _decimal = P.decimal lexer
    _stringLiteral = P.stringLiteral lexer
    _identifier = P.identifier lexer
    _braces = P.braces lexer
    _brackets = P.brackets lexer
    _parens = P.parens lexer
    _lexeme = P.lexeme lexer
    _char = _lexeme . char
    reserved' p = _lexeme $ try $ do { r <- p
                                     ; notFollowedBy $ identLetter languageDef
                                     ; return r
                                     }
    reserved = reserved' . string
    
