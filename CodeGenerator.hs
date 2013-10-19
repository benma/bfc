{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module CodeGenerator (compile) where

import TapeAllocation(TapeAccessSequence(..), findAllocation, Allocation)
import Types(PositionRef(..), makePositionRef, PositionRefOffset(..), (+:), Size)

import Prelude hiding(foldr, (.))

import ShortBytes(ShortByteParams)
import Parser(AST(..), Op(..), parseString)
import Data.Word(Word8)
import Data.Digits(digits)
import Control.Category((.))
import Control.Applicative((<*))
import Control.Exception(assert)
import Control.Applicative((<$>))
import Control.Monad
import Control.Monad.Writer.Strict
import Control.Monad.State.Strict
import Control.Monad.Tardis

import Control.Lens
import Data.Char(ord)

import Data.String.Utils(replace)

import qualified Data.Sequence as S
import qualified Data.HashMap.Strict as Map

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC

import qualified Data.Array as Array


import Data.Foldable(foldr)

data Type = TInteger
          | TString
          | TBool
          | TArray Int

-- type Knot = RWST Allocation (S.Seq IR) ()
-- tie :: Knot Identity t -> t
-- tie knot = let (a, k) = runIdentity $ evalRWST knot (findAllocation k) ()
--            in a

type Knot = Tardis (Allocation PositionRef) (S.Seq (TapeAccessSequence PositionRef))
tie :: Knot a -> a
tie knot = evalTardis (knot <* (getPast >>= sendPast . findAllocation)) (Map.empty, S.empty)

data MemInfo = MemInfo { _memType :: Maybe Type
                       , _memSize :: Size
                       }

type DefMap = Map.HashMap String AST
type VarMap = Map.HashMap String PositionRef
type MemMap = Map.HashMap PositionRef MemInfo

data EvalState = EvalState { _varMap :: VarMap
                           , _memMap :: MemMap
                           , _memPos :: PositionRef
                           , _defMap :: DefMap
                           , _shortByteParams :: Array.Array Int ShortByteParams
                           }

makeLenses ''EvalState
makeLenses ''MemInfo

data Alignment = LeftAligned | RightAligned


compile :: [ShortByteParams] -> AST -> BS.ByteString
compile shortByteParams' = tie . execWriterT . flip evalStateT initialEvalState . evalAST
  where
    initialEvalState :: EvalState
    initialEvalState = EvalState Map.empty Map.empty (makePositionRef 0) Map.empty shortByteParamsArray
      where shortByteParamsArray = Array.array (0,255) $ zip [0..] shortByteParams'

    skip = return ()

    evalAST :: AST -> StateT EvalState (WriterT (BS.ByteString) Knot) ()
    evalAST (AProgram is) = mapM_ evalAST is
    evalAST (AExpression expr) = evalExpression expr >> skip
    evalAST (APrint (AExpression (AVariable varName))) = do
      pos <- getPos varName
      size <- getSize pos
      atPos pos $ do write $ BC.concat $ replicate size ".>"
                     move (-size)
    evalAST (APrint (AExpression (AString s))) = do
      pos <- mallocByte
      let bytes = map ord s
      tmp <- mallocByte
      forM_ (zipWith (-) bytes (0:bytes)) $ \byte -> addByteShort (Just tmp) pos byte >> (atPos pos $ write ".")
    evalAST (APrint (AExpression expr)) = do
      (Just pos) <- evalExpression expr
      size <- getSize pos
      atPos pos $ do write $ BC.concat $ replicate size ".>"
                     move (-size)
    evalAST (AIf (AExpression expr) is elsePart) = do
      pos <- mallocBool
      convertToBool expr pos
      writeIfByte' False pos (forM_ is evalAST) (liftM (mapM_ evalAST) elsePart)
    evalAST (AWhile (AExpression expr) is) = do
      cond <- mallocBool
      convertToBool expr cond
      loopByte cond $ do
        tellTapeAccess StartLoop
        forM_ is evalAST
        convertToBool expr cond
        tellTapeAccess EndLoop
    evalAST ast@(ADef defName _ _) = do
      defMap %= Map.insert defName (replaceVars ast)
        where replaceVars = transform (\ast' -> case ast' of 
                                          AVariable varName -> AVariable $ rename varName
                                          ADef defName' params is -> ADef defName' (map rename params) is
                                          _ -> ast')
              rename varName = defName ++ "$" ++ varName
    evalAST _ = undefined
    evalExpression (ADefCall "array" [AExpression (AInteger size)]) = do
      let size' = fromIntegral size
      pos <- mallocT (Just $ TArray size') (size'+4)
      return $ Just pos
    evalExpression (ADefCall "read" []) = do
      pos <- mallocT (Just TString) 1
      atPos pos $ write ","
      return $ Just pos
    evalExpression (ADefCall "ord" [AExpression (AString (c:[]))]) = do
      ret <- mallocT (Just TInteger) 1
      atPos ret $ addByte $ fromEnum $ c
      return $ Just ret
    evalExpression (ADefCall "trace" [AExpression (AString s)]) = do
      write $ BC.pack s
      return Nothing
    evalExpression (ADefCall "ord" [AExpression expr]) = do
      (Just pos) <- evalExpression expr
      size <- getSize pos
      assert (size == 1) skip
      ret <- mallocT (Just TInteger) 1
      writeMove (1::Word8) pos ret
      return $ Just ret
    evalExpression (ADefCall "malloc" [AExpression (AInteger size)]) = let size' = fromIntegral size in Just <$> mallocT (Just TInteger) size'
    evalExpression (ADefCall "malloc" [AExpression (AInteger size), AExpression  (AInteger i)]) = do
      let encoded = encodeInteger i
          size' = fromIntegral size
          size'' = length encoded
      assert (size'' <= size') skip
      pos <- mallocT (Just TInteger) size'
      atPos (pos +: (size'-size'')) $ do forM_ encoded $ \byte -> addByte byte >> move 1
                                         move (-size'')
      return $ Just pos
    evalExpression (ADefCall defName args) = do
      defMap' <- use defMap
      let ADef _ params is = defMap' ^?! ix defName
      forM_ (zip params args) $ \(paramName, (AExpression arg)) -> do
        (Just pos) <- evalExpression arg
        defineV paramName pos
      ret <- evalDefAST is
      forM_ params undefineV
      return ret
      where
        evalDefAST [] = return Nothing
        evalDefAST [AExpression expr] = evalExpression expr
        evalDefAST (ins:inss) = evalAST ins >> evalDefAST inss
    evalExpression (AInfixOp OpAssignment (AExpression (APosRef refPos')) (AExpression expr)) = do
      let refPos = PositionRefOffset refPos' 0
      (Just pos) <- evalExpression expr
      refSize <- getSize refPos
      size <- getSize pos
       -- once a ref is defined, it needs to stay the same size
      assert (size == refSize) skip
      writeCopy refSize pos refPos
      return Nothing
    evalExpression (AInfixOp OpAssignment (AExpression (AVariable varName)) (AExpression expr)) = do
      (Just pos) <- evalExpression expr
      vm <- use varMap
      let exists = Map.member varName vm
      if exists
        then do varPos <- getPos varName
                varSize <- getSize varPos
                (Just exprType) <- getType pos
                -- resize in case of int
                newPos <- case exprType of
                  TInteger -> resize RightAligned pos varSize
                  _ -> return pos
                size <- getSize newPos
                 -- once a var is defined, it needs to stay the same size
                assert (size == varSize) skip
                writeCopy varSize newPos varPos
        else do defineV varName pos >> skip
      return Nothing
    evalExpression (AVariable varName) = do
      evalExpression =<< (APosRef . _position) <$> getPos varName
    evalExpression (APosRef p') = do
      let p = PositionRefOffset p' 0
      t <- getType p
      size <- getSize p
      pos <- mallocT t size
      writeCopy' size p pos
      return $ Just pos

    evalExpression (AString s) = do
      let size = length s
      pos <- mallocT (Just TString) size
      addBytesShort pos (map ord s)
      return $ Just pos
    evalExpression (ASliceOp (AExpression expr) (AExpression (AInteger index'))) = do
      (Just pos) <- evalExpression expr
      (Just t) <- getType pos
      case t of
        TString -> do
          ret <- mallocT (Just TString) 1
          copyByte' (pos+:(fromIntegral index')) ret
          return $ Just ret
        (TArray _) -> do
          ret <- mallocT (Just TString) 1
          copyByte' (pos +: (4 + fromIntegral index')) ret
          return $ Just ret
        _ -> error "not supported"
    evalExpression (AInfixOp OpAssignment (AExpression (ASliceOp (AExpression (AVariable varName)) (AExpression index'))) (AExpression right)) = do
      pos <- getPos varName
      (Just t) <- getType pos
      (Just indexPos) <- evalExpression index'
      (Just indexType) <- getType indexPos
      (Just rightPos) <- evalExpression right
      rightSize <- getSize rightPos
      case (t, indexType) of
        ((TArray _), TInteger) -> do
          erase pos
          copyByte indexPos (pos+:1)
          copyByte indexPos (pos+:2)
          copyByte (rightPos +: (rightSize-1)) (pos+:3)
          atPos pos $ write ">[>>>[-<<<<+>>>>]<[->+<]<[->+<]<[->+<]>-]>>>[-]<[->+<]<[[-<+>]<<<[->>>>+<<<<]>>-]<<"
          return Nothing
        _ -> do
          assert False skip
          return Nothing
    evalExpression (ASliceOp (AExpression expr) (AExpression index')) = do
      (Just pos) <- evalExpression expr
      (Just t) <- getType pos
      (Just indexPos) <- evalExpression index'
      (Just indexType) <- getType indexPos
      case (t, indexType) of
        ((TArray _), TInteger) -> do
          ret <- mallocT (Just TString) 1
          copyByte indexPos (pos+:1)
          copyByte indexPos (pos+:2)
          erase (pos+:3)
          atPos pos $ write ">[>>>[-<<<<+>>>>]<<[->+<]<[->+<]>-]>>>[-<+<<+>>>]<<<[->>>+<<<]>[[-<+>]>[-<+>]<<<<[->>>>+<<<<]>>-]<<"
          copyByte' (pos+:3) ret
          return $ Just ret
        _ -> error "not supported"
    evalExpression (AInteger i) = do
      let encoded = encodeInteger i
          size = length encoded
      pos <- mallocT (Just TInteger) size
      addBytesShort pos encoded
      return $ Just pos

    evalExpression (ABool b) = do
      pos <- mallocBool
      when b $ atPos pos $ addByte (1::Word8)
      return $ Just pos
    evalExpression (AInfixOp OpPlus (AExpression (AInteger left)) (AExpression (AInteger right))) = evalExpression $ AInteger (left+right) -- constant folding
    evalExpression (AInfixOp OpPlusEq (AExpression (AVariable varName)) expr) = do
      pos <- getPos varName
      evalExpression $ AInfixOp OpPlusEq (AExpression (APosRef $ _position pos)) expr
    evalExpression (AInfixOp OpPlusEq (AExpression (APosRef varPos')) (AExpression expr)) = do
      let varPos = PositionRefOffset varPos' 0
      varSize <- getSize varPos
      (Just exprPos) <- evalExpression expr
      (Just varType) <- getType varPos
      (Just exprType) <- getType exprPos
      case (varType, exprType) of
        (TInteger, TInteger) -> do
          newPos <- resize RightAligned exprPos varSize
          writeAddSubInt OpPlus varPos newPos >> skip
        _ -> error "not supported"
      return Nothing
    evalExpression (AInfixOp OpMinusEq (AExpression (AVariable varName)) (AExpression expr)) = do
      varPos <- getPos varName
      varSize <- getSize varPos
      (Just exprPos) <- evalExpression expr
      (Just varType) <- getType varPos
      (Just exprType) <- getType exprPos
      case (varType, exprType) of
        (TInteger, TInteger) -> do
          newPos <- resize RightAligned exprPos varSize
          writeAddSubInt OpMinus varPos newPos >> skip
        _ -> error "not supported"
      return Nothing
    evalExpression (AInfixOp OpPlus (AExpression left) (AExpression right)) = evalPlusMinusExpression OpPlus left right 
    evalExpression (AInfixOp OpMinus (AExpression left) (AExpression right)) = evalPlusMinusExpression OpMinus left right
    evalExpression (AInfixOp OpMul (AExpression (AInteger left)) (AExpression (AInteger right))) = evalExpression $ AInteger (left*right) -- constant folding
    evalExpression (AInfixOp OpMul (AExpression left) (AExpression right)) = do
      (Just pos1) <- evalExpression left
      (Just pos2) <- evalExpression right
      (Just t1) <- getType pos1
      (Just t2) <- getType pos2
      s1 <- getSize pos1
      s2 <- getSize pos2
      case (t1, t2) of
        (TInteger, TInteger) -> do
          let newSize = max s1 s2 + 1
          ret <- mallocT (Just TInteger) newSize
          assert (_offset pos1 == 0 && _offset pos2 == 0 && _offset ret == 0) skip
          -- hack
          evalAST $ parseString $ foldr (uncurry replace) "while(${_l}) { ${_ret} += ${_r} ${_l}-- }" $ [("_l", show $ _position' $ _position pos1), ("_r", show $ _position' $ _position pos2), ("_ret", show $ _position' $ _position ret)]
          return $ Just ret
        _ -> error "not supported"
    evalExpression (APostfixOp OpDec (AExpression (AVariable varName))) = do
      pos <- getPos varName
      evalExpression (APostfixOp OpDec (AExpression (APosRef $ _position pos)))
    evalExpression (APostfixOp OpDec (AExpression (APosRef pos'))) = do
      let pos = PositionRefOffset pos' 0
      (Just t) <- getType pos
      s <- getSize pos
      case t of
        TInteger -> do
          let handle i = if i /= 0
                         then do carry <- writeNotByte (pos+:i)
                                 atPos (pos+:i) dec
                                 writeIfByte' False carry (handle (i-1)) Nothing
                         else atPos (pos+:i) dec
          handle (s-1)
        _ -> error "not supported"
      return Nothing
    evalExpression (APostfixOp OpInc (AExpression (AVariable varName))) = do
      pos <- getPos varName
      evalExpression (APostfixOp OpInc (AExpression (APosRef $ _position pos)))
    evalExpression (APostfixOp OpInc (AExpression (APosRef pos'))) = do
      let pos = PositionRefOffset pos' 0
      (Just t) <- getType pos
      s <- getSize pos
      case t of
        TInteger -> do
          let handle i = if i /= 0
                         then do atPos (pos+:i) inc
                                 writeIfByte (pos+:i) skip $ Just (handle (i-1))
                         else atPos (pos+:i) inc
          handle (s-1)
        _ -> error "not supported"
      return Nothing
    evalExpression (APrefixOp OpNeg (AExpression expr)) = do
      pos <- mallocBool
      convertToBool expr pos
      tmp <- mallocT Nothing 1
      loopByte pos $ (atPos tmp inc) >> erase pos
      atPos pos $ inc
      loopByte tmp $ atPos tmp dec >> atPos pos dec
      return $ Just pos
    evalExpression (AInfixOp OpAnd (AExpression left) (AExpression right)) = do
      pos1 <- mallocBool
      convertToBool left pos1
      pos2 <- mallocBool
      convertToBool right pos2
      tmp0 <- mallocT Nothing 1
      tmp1 <- mallocT Nothing 1
      loopByte pos1 $ atPos tmp1 inc >> atPos pos1 dec
      loopByte tmp1 $ do
        erase tmp1
        loopByte pos2 $ atPos tmp1 inc >> atPos tmp0 inc >> atPos pos2 dec
        loopByte tmp0 $ atPos pos2 inc >> atPos tmp0 dec
        loopByte tmp1 $ atPos pos1 inc >> erase tmp1
      return $ Just pos1
    evalExpression (AInfixOp OpOr (AExpression left) (AExpression right)) = do
      pos1 <- mallocBool
      convertToBool left pos1
      pos2 <- mallocBool
      convertToBool right pos2
      tmp0 <- mallocT Nothing 1
      tmp1 <- mallocT Nothing 1
      loopByte pos1 $ atPos tmp1 inc >> atPos pos1 dec
      loopByte tmp1 $ atPos pos1 dec  >> erase tmp1
      loopByte pos2 $ do
        atPos tmp1 inc
        atPos tmp0 inc
        atPos pos2 dec
      loopByte tmp0 $ atPos pos2 inc >> atPos tmp0 dec
      loopByte tmp1 $ atPos pos1 (write "[-]-") >> erase tmp1
      return $ Just pos1
    evalExpression (AInfixOp OpEq (AExpression left) (AExpression right)) = do
      (Just pos1) <- evalExpression left
      (Just pos2) <- evalExpression right
      s1 <- getSize pos1
      s2 <- getSize pos2
      res <- mallocBool -- defaults to 0
      if s1 == s2
        then do
          let handle i = do
                isEqual (pos1+:i) (pos2+:i)
                if i == 0
                  then copyByte pos1 res
                  else writeIfByte (pos1+:i) (handle (i-1)) $ Just $ copyByte (pos1+:i) res
          handle (s1-1)
        else skip
      return $ Just res
    evalExpression (AInfixOp OpIntegerEq (AExpression left) (AExpression right)) = do
      (Just pos1) <- evalExpression left
      (Just pos2) <- evalExpression right
      s1 <- getSize pos1
      s2 <- getSize pos2
      let size = max s1 s2
      (resizedPos1, resizedPos2) <- liftM2 (,) (resize RightAligned pos1 size) (resize RightAligned pos2 size)
      res <- mallocBool
      let handle i = do
            isEqual (resizedPos1+:i) (resizedPos2+:i)
            if i == 0
              then copyByte resizedPos1 res
              else writeIfByte (resizedPos1+:i) (handle (i-1)) $ Just $ copyByte (resizedPos1+:i) res
      handle (size-1)
      return $ Just res
    evalExpression (AInfixOp OpNeq left right) = do
      evalExpression $ APrefixOp OpNeg $ AExpression $ AInfixOp OpEq left right
    evalExpression (AInfixOp OpLeq (AExpression left) (AExpression right)) = evalLeLeqExpression OpLeq left right
    evalExpression (AInfixOp OpLe (AExpression left) (AExpression right)) = evalLeLeqExpression OpLe left right
    evalExpression (AInfixOp OpGeq left right) = evalExpression $ APrefixOp OpNeg $ AExpression $ AInfixOp OpLe left right
    evalExpression (AInfixOp OpGe left right) = evalExpression $ APrefixOp OpNeg $ AExpression $ AInfixOp OpLeq left right
    evalExpression _ = undefined
    isEqual pos1 pos2 = do
      tmp0 <- mallocT Nothing 1
      tmp1 <- mallocT Nothing 1
      loopByte pos1 $ atPos tmp1 inc >> atPos pos1 dec
      atPos pos1 inc
      loopByte pos2 $ atPos tmp1 dec >> atPos tmp0 inc >> atPos pos2 dec
      loopByte tmp0 $ atPos pos2 inc >> atPos tmp0 dec
      loopByte tmp1 $ atPos pos1 dec >> erase tmp1
    evalLeLeqExpression operator left right = do
      (Just pos1) <- evalExpression left
      (Just pos2) <- evalExpression right
      _t1@(Just t1) <- getType pos1
      _t2@(Just t2) <- getType pos2
      s1 <- getSize pos1
      s2 <- getSize pos2
      case (t1,t2) of
        (TInteger, TInteger) -> do
          let size = max s1 s2
          newPos1 <- resize RightAligned pos1 size 
          newPos2 <- resize RightAligned pos2 size
          res <- mallocBool
          let handle i = do
                erase res
                let ifPos1IsZero f = writeIfByte (newPos1+:i) skip $ Just f
                loopByte (newPos2+:i) $ do
                  ifPos1IsZero $ atPos res $ writeByte (1::Word8)
                  atPos (newPos1+:i) dec
                  atPos (newPos2+:i) dec
                -- res is 1 if byte at pos1 is less than byte at pos2
                if i < size
                  then writeIfByte res skip $ Just $ ifPos1IsZero $ handle (i+1) -- if <, we are done, otherwise continue with next byte if it is <=
                  else case operator of
                            OpLeq -> ifPos1IsZero $ atPos res $ writeByte (1::Word8) -- last byte: res is already 1 if <, this checks for <=
                            OpLe -> skip
                            _ -> undefined
        
          handle 0
          return $ Just res
        _ -> error "not supported"

    evalPlusMinusExpression operator left right = do
      (Just pos1) <- evalExpression left
      (Just pos2) <- evalExpression right
      s1 <- getSize pos1
      s2 <- getSize pos2
      let size = max s1 s2
      newPos1 <- resize RightAligned pos1 size 
      newPos2 <- resize RightAligned pos2 size
      writeAddSubInt operator newPos1 newPos2

    convertToBool expr dstPos = do
      (Just pos) <- evalExpression expr
      writeToBool pos dstPos
    write = lift . tell
    atPos (PositionRefOffset pos' offset') f = do
      size <- getSize (PositionRefOffset pos' 0)
      tellTapeAccess $ Access pos' size
      p <- getPos' pos'
      move (p + offset') >> f >> move (-p - offset')
    loopByte pos f = (atPos pos $ write "[") >> f >> (atPos pos $ write "]")
    move pos = write $ if pos < 0
                       then BC.replicate (-pos) '<'
                       else BC.replicate pos '>'
    writeMove 1 srcPos dstPos = do
      erase dstPos
      loopByte srcPos $ atPos dstPos inc >> atPos srcPos dec
    writeMove _ _ _ = undefined
    erase b = atPos b $ writeByte (0::Word8)
    resize alignment pos newSize = do
      oldSize <- getSize pos
      type_ <- getType pos
      if newSize == oldSize
        then return pos
        else if newSize >= oldSize
             then do newPos <- mallocT type_ newSize
                     case alignment of
                       LeftAligned -> writeCopy' oldSize pos newPos
                       RightAligned -> writeCopy' oldSize pos (newPos +: (newSize - oldSize))
                     return newPos
             else do newPos <- mallocT type_ newSize -- todo: make smaller by simply cutting of at pos+newSize 
                     case alignment of
                       LeftAligned ->  writeCopy' newSize pos newPos
                       RightAligned -> writeCopy' newSize (pos +: (oldSize-newSize)) newPos
                     return newPos
    writeByte 0 = write "[-]"
    writeByte b = writeByte 0 >> addByte b
    writeCopy 1 srcPos dstPos = do
      tmp <- mallocByte
      -- move src to dst and tmp
      erase dstPos
      atPos srcPos $ write "["
      atPos dstPos inc
      atPos tmp inc
      atPos srcPos $ write "-]"
      writeMove (1::Word8) tmp srcPos
    writeCopy size srcPos dstPos = (forM_ [0..size-1] $ \i -> copyByte (srcPos +: i) (dstPos +: i))
    copyByte = writeCopy 1

    writeCopy' 1 srcPos dstPos = do
      tmp <- mallocByte
      -- move src to dst and tmp
      atPos srcPos $ write "["
      atPos dstPos inc
      atPos tmp inc
      atPos srcPos $ write "-]"
      writeMove (1::Word8) tmp srcPos
    writeCopy' size srcPos dstPos = (forM_ [0..size-1] $ \i -> copyByte' (srcPos +: i) (dstPos +: i))
    copyByte' = writeCopy' 1
    inc = write "+"
    dec = write "-"
    addByte b = write $ BC.replicate (fromIntegral b) '+'
    -- dstPos will contain result
    writeOrByte dstPos srcPos = do
      let setOne = atPos dstPos $ writeByte (1::Word8)
      writeIfByte dstPos setOne (Just $ writeIfByte' False srcPos setOne Nothing)

    -- -- dstPos will contain result
    -- writeAndByte dstPos srcPos = do
    --   let setOne = atPos dstPos $ writeByte 1
    --   let setZero = erase dstPos
    --   writeIfByte dstPos (writeIfByte srcPos setOne $ Just setZero) (Just setZero)

    writeToBool pos dstPos = do
      s <- getSize pos
      case s of
        1 -> copyByte pos dstPos
        _ -> do erase dstPos
                forM_ [0..s-1] $ \i -> writeOrByte dstPos (pos+:i)
    writeNotByte pos = do
      tmp <- mallocByte
      atPos tmp $ addByte (1::Word8)
      writeIfByte pos (atPos tmp dec) Nothing
      return tmp

    writeIfByte = writeIfByte' True

    writeIfByte' True pos ifPart Nothing = do
      tmp <- mallocByte
      copyByte pos tmp
      writeIfByte' False tmp ifPart Nothing
    writeIfByte' True pos ifPart (Just elsePart) = do
      tmp <- mallocByte
      copyByte pos tmp
      writeIfByte' False tmp ifPart (Just elsePart)
    writeIfByte' False pos ifPart Nothing = loopByte pos $ ifPart >> erase pos
    writeIfByte' False pos ifPart (Just elsePart) = do
      tmp2 <- mallocByte
      atPos tmp2 $ addByte (1::Word8)
      loopByte pos $ ifPart >> erase pos >> atPos tmp2 dec
      loopByte tmp2 $ elsePart >> atPos tmp2 dec

    writeAddSubInt operator dstPos srcPos = do
      (Just TInteger) <- getType dstPos
      (Just TInteger) <- getType srcPos
      s1 <- getSize dstPos
      s2 <- getSize srcPos
      assert (s1 == s2) skip

      case s1 of 
        s -> do
          carry <- mallocBool
          let handle i = do
                let opC = case operator of
                      OpPlus -> inc
                      OpMinus -> dec
                      _ -> undefined
                let writeCarry' = when (i /= 0) $ writeIfByte (dstPos+:i) skip (Just $ atPos carry $ writeByte (1::Word8))
                let writeCarry = case operator of
                      -- with plus, carry happens when dst is zero
                      OpPlus -> writeCarry'
                      -- with minus, carry happens when dst passes zero
                      OpMinus -> atPos (dstPos+:i) inc >> writeCarry' >> atPos (dstPos+:i) dec
                      _ -> undefined
                when (i /= s - 1) $ writeIfByte carry (atPos (dstPos+:i) opC >> erase carry >> writeCarry) Nothing
                loopByte (srcPos+:i) $ do atPos (dstPos+:i) opC
                                          writeCarry
                                          atPos (srcPos+:i) dec
                when (i/=0) $ handle (i-1)
          handle (s-1)
      return (Just dstPos)
      
    encodeInteger :: Integer -> [Word8]
    encodeInteger 0 = [0]
    encodeInteger i = map fromIntegral $ digits 256 (abs i)

    uniquePos oldPos = makePositionRef (_position' oldPos + 1)
    mallocT type_ size = do
      pos' <- use memPos
      let pos = PositionRefOffset pos' 0
      memPos .= uniquePos pos'
      memMap %= Map.insert pos' (MemInfo type_ size)
      forM_ [0..size-1] $ \i -> erase (pos +: i)
      return pos
    defineV varName (PositionRefOffset pos 0) = varMap %= Map.insert varName pos
    defineV _ _ = undefined
    undefineV varName = varMap %= Map.delete varName
    malloc = mallocT Nothing
    mallocByte = malloc 1
    mallocBool = mallocT (Just TBool) 1
    getSize (PositionRefOffset pos 0) = uses memMap (^?! ix pos . memSize)
    getSize _ = undefined
    getType (PositionRefOffset pos 0) = uses memMap (^?! ix pos . memType)
    getType _ = undefined
    getPos varName = uses varMap $ \m -> PositionRefOffset (m ^?! ix varName) 0
    getPos' posRef = do ~(Just p) <- lift . lift $ getsFuture $ Map.lookup posRef
                        return p
    tellTapeAccess access = lift . lift $ modifyForwards (S.|> access)

    addBytesShort pos bytes = forM_ (zip [0..] bytes) $ \(i, byte) ->
      if i < length bytes - 1
      then
        -- use unused part of allocated memory as temporary space
        addByteShort (Just $ pos +: (i+1)) (pos +: i) byte 
      else
        -- Nothing => let function allocate a new temporary byte
        addByteShort Nothing (pos +: i) byte
    -- addByteShort writes the byte `b` at position `pos` in a more clever way than just replicating `b` pluses.
    -- It outputs code of the form "a[>d<c]" where a, d, c are the optimal number of pluses or minuses to produce the byte `b`.
    --addByteShort _ pos b = let b' = if b < 0 then 256+b else b in atPos pos $ addByte b'
    addByteShort tmpPos pos b = do
      let b' = if b < 0 then 256 + b else b -- subtract b by adding (256-b)
      params <- use shortByteParams
      let (a', loopIterations, c', d') = params Array.! fromIntegral b'
      if loopIterations == 1
      then atPos pos $ writeByteShort d' -- do not output looping code if there is only one iteration
      else do { -- write (BC.pack (show (a',loopIterations, c', d', b')))
              ; tmp <- maybe mallocByte return tmpPos
              ; -- tmpPos needs to be zero at the beginning, will be zero at the end.
              ; atPos tmp $ writeByteShort a'
              ; loopByte tmp $ do { atPos pos $ writeByteShort d'
                                  ; atPos tmp $ writeByteShort c'
                                  }
              }
      where
        writeByteShort b = if b < 128
                           then write $ BC.replicate (fromIntegral b) '+'
                           else write $ BC.replicate (fromIntegral (256-b)) '-' 
