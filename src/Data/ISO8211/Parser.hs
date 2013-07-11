{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

{-
Copyright (c) 2013, Markus Barenhoff <alios@alios.org>
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the <organization> nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}

module Data.ISO8211.Parser (DataFile
               ,DataDescriptiveRecord(..)
               ,DataRecord
               ,DataDescriptiveField
               ,DataStructureCode (..)
               ,DataTypeCode (..)
               ,LexicalLevel (..)
               ,DataStructure (..)
               ,dsSingleData, dsLinearStruct, dsMultiDimStruct
               ,DataFormat (..)
               ,DataField(..)
               ,DataFieldT(..)
               ,parseDataFile
               ) where

import Data.ByteString (ByteString)

import Data.Binary
import Data.Binary.Get
import Data.Bits
import Data.Tree
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Data.Attoparsec as P
import qualified Data.Attoparsec.ByteString.Char8 as C8
import Data.Map (Map)
import qualified Data.Map as Map

--
-- external type definitions
--

type DataFile = (DataDescriptiveRecord, [DataRecord])

type DataRecord = Tree DataFieldR
type DataFieldR = (String, DataStructure)

data DataDescriptiveRecord = 
    DDR {
      ddrFileName :: !String,
      ddrFieldStructure :: ![(String, String)],
      ddrDataFormats :: Map String DataDescriptiveField -- [(String, DataDescriptiveField)]
    } deriving (Show, Eq)

data DataFieldT =
    DFString !String | DFInteger !Integer | DFReal !Double | DFByteString !ByteString
    deriving (Eq, Show)

class DataField t where
    fromDataField :: DataFieldT -> t

instance DataField String where
    fromDataField (DFString s) = s
    fromDataField v = error $ "fromDataFieldT is not a String: " ++ show v

instance DataField Integer where
    fromDataField (DFInteger i) = i
    fromDataField v = error $ "fromDataFieldT is not an Integer: " ++ show v

instance DataField Double where
    fromDataField (DFReal r) = r
    fromDataField v = error $ "fromDataFieldT is not a Double: " ++ show v

instance DataField ByteString where
    fromDataField (DFByteString bs) = bs
    fromDataField v = error $ "fromDataFieldT is not a ByteString: " ++ show v
  
type DataDescriptiveField =
    (DataStructureCode, DataTypeCode, LexicalLevel, String, [(String, DataFormat)])

data DataStructureCode = 
    SingleDataItem | LinearStructure | MultiDimStructure
    deriving (Eq, Enum, Show)

data DataTypeCode = 
    CharacterString | ImplicitPointInt | ImplicitPointReal | BinaryForm | MixedDataType
    deriving (Eq, Show)

data LexicalLevel = 
    LexicalLevel0 | LexicalLevel1 | LexicalLevel2
    deriving (Eq, Enum, Show)

data DataFormat = CharacterData (Maybe Integer)
                | ImplicitPoint (Maybe Integer)
                | ExplicitPoint (Maybe Integer)
                | BitString (Maybe Integer)
                | SubFieldLabel (Maybe Integer)
                | SignedInt Integer
                | UnsignedInt Integer
                deriving (Show, Eq)

data DataStructure = SD DataFieldT
                   | LS (Map String DataFieldT)
                   | MDS [Map String DataFieldT]
                   deriving (Show, Eq)

--
-- internal type definitions
--

type DataDescr = (DataStructureCode, [String])

type FieldControlField = 
    (DataStructureCode, DataTypeCode, LexicalLevel, String, [(String, String)])

instance Enum DataTypeCode where
    toEnum 0 = CharacterString
    toEnum 1 = ImplicitPointInt 
    toEnum 2 = ImplicitPointReal
    toEnum 5 = BinaryForm
    toEnum 6 = MixedDataType
    toEnum i = error $ "Unknown DataTypeCode" ++ show i
    fromEnum CharacterString = 0
    fromEnum ImplicitPointInt = 1
    fromEnum ImplicitPointReal = 2
    fromEnum BinaryForm = 5
    fromEnum MixedDataType = 6


dsSingleData :: DataStructure -> DataFieldT
dsSingleData (SD f) = f
dsSingleData t = error $ "not a Single Data Item: " ++ show t
dsLinearStruct :: DataStructure -> (Map String DataFieldT)
dsLinearStruct (LS fs) = fs
dsLinearStruct t = error $ "not a Linear Structure: " ++ show t
dsMultiDimStruct :: DataStructure -> [Map String DataFieldT]
dsMultiDimStruct (MDS fss) = fss 
dsMultiDimStruct t = error $ "not a Multi Dim Structure: " ++ show t



--
-- exports 
--
parseDataFile :: Parser DataFile
parseDataFile = do
  ddr <- parseDDR <?> "data descriptive record"
  drs <- manyTill (parseDR ddr) (try endOfInput) <?> "data record"
  return (ddr, drs)

parseDDR :: Parser DataDescriptiveRecord
parseDDR = fmap snd parseDDR'

parseDDR' :: Parser ((Char, Char, Char, Char, Char, String), DataDescriptiveRecord)
parseDDR' = do
  (ichglvl, lid, ext, ver, appi, fcl, extCharSet, fieldAreaLen) <- parseLeader
  (shieldTagF, ds) <- parseEntryMap
  bs <- P.take fieldAreaLen

  let ((_, rr):rs) = [(t, BS.take l $ BS.drop p bs) | (t,p,l) <- ds] 
  let (_,_,_,fname, fstruct) = 
          either error id $ parseOnly  (parseFieldControlField fcl shieldTagF) rr
  
  let rs'= [(a, either error id $ parseOnly 
             (parseDataDescriptiveField fcl) b) | (a,b) <- rs]

  let ddr =  DDR fname fstruct $ Map.fromList rs'
  let retval = ((ichglvl, lid, ext, ver, appi, extCharSet), ddr)
  return retval

parseDR :: DataDescriptiveRecord -> Parser DataRecord
parseDR ddr = fmap snd (parseDR' ddr)


parseDR' :: DataDescriptiveRecord -> Parser ((Char, Char, Char, Char, Char, String), DataRecord)
parseDR' ddr = do
  (ichglvl, lid, ext, ver, appi, _, extCharSet, fieldAreaLen) <- parseLeader
  (_, ds) <- parseEntryMap
  bs <- P.take fieldAreaLen
  let bss = [ (t, BS.take l $ BS.drop p bs) | (t,p,l) <- ds] 
  let dfs = map (\(t, lbs) -> (t, either error id $ 
                              parseOnly (ddrParserLookup ddr t) lbs)) bss
  return ((ichglvl, lid, ext, ver, appi, extCharSet), drsToTree' ddr dfs)


--
-- internal
-- 
unitTerminator, recordTerminator :: Char
unitTerminator = '\US'
recordTerminator = '\RS'

parseUT, parseRT :: Parser ()
parseUT = do _ <- C8.char unitTerminator; return ()
parseRT = do _ <- C8.char recordTerminator; return ()

ddrParserLookup :: DataDescriptiveRecord -> String -> Parser DataStructure
ddrParserLookup ddr t = do
  let (sc, _, esc, _, dfs) = maybe (error $ "unknown field: " ++ t) id 
                             $ Map.lookup t (ddrDataFormats ddr)
  dfsParser dfs esc sc

dfsParser :: 
    [(String, DataFormat)] -> LexicalLevel -> DataStructureCode 
    -> Parser DataStructure
dfsParser dfs esc sc =
    let mfp = sequence $ map fieldParser $ dfs
        fieldParser (t, p) = do
          v <- dataFormatToParser esc p
          return $ (t, v)
    in do res <- case sc of 
                  SingleDataItem -> do
                      (_, r) <- fmap head $  mfp
                      _ <- parseRT
                      return $ SD r
                  LinearStructure -> do
                      r <-  fmap (LS . Map.fromList) mfp
                      _ <- parseRT
                      return $ r
                  MultiDimStructure ->
                      fmap (MDS . (map Map.fromList) ) $ mfp `manyTill` (try $ parseRT)
          return res



dataFormatToParser :: LexicalLevel -> DataFormat  -> Parser DataFieldT
dataFormatToParser _ (CharacterData l) = fmap DFString $
    case l of
      Nothing -> C8.anyChar `manyTill` (try $ parseUT)
      Just i ->  count (fromInteger i) C8.anyChar
dataFormatToParser esc (ImplicitPoint l) = do
    (DFString s) <- dataFormatToParser esc (CharacterData l)
    return $ if (length s == 0) 
             then DFInteger 0
             else DFInteger (read s)
dataFormatToParser esc (ExplicitPoint l) = do
    (DFString s) <- dataFormatToParser esc (CharacterData l)
    return $ if (length s == 0) 
             then DFReal 0.0
             else DFReal (read s)
dataFormatToParser _ (BitString l) = fmap DFByteString $
    case l of 
      Nothing -> fmap BS.pack $ anyWord8 `manyTill` (try $ parseUT)
      Just l' -> P.take (fromInteger l' `div` 8)
dataFormatToParser _ (UnsignedInt l) = do
  bs <- P.take (fromInteger l)
  case l of
    1 -> return $ DFInteger $ toInteger $ parseUInt8  (BL.fromChunks [bs])
    2 -> return $ DFInteger $ toInteger $ parseUInt16 (BL.fromChunks [bs])
    4 -> return $ DFInteger $ toInteger$ parseUInt32 (BL.fromChunks [bs])
    i -> error $ "invalid int length: " ++ show i
dataFormatToParser _ (SignedInt l) = do
  bs <- P.take (fromInteger l)
  case l of
    1 -> return $ DFInteger $ toInteger $ parseInt8  (BL.fromChunks [bs])
    2 -> return $ DFInteger $ toInteger $ parseInt16 (BL.fromChunks [bs])
    4 -> return $ DFInteger $ toInteger $ parseInt32 (BL.fromChunks [bs])
    i -> error $ "invalid int length: " ++ show i
dataFormatToParser _ (SubFieldLabel _) = error $ "@ subfield label not implemented"


parseUInt8' :: BL.ByteString -> Word8
parseUInt8' = runGet getWord8
parseUInt16' :: BL.ByteString -> Word16
parseUInt16' = runGet getWord16le
parseUInt32' :: BL.ByteString -> Word32
parseUInt32' = runGet getWord32le

parseInt8,parseUInt8, parseInt16,parseUInt16,parseInt32,parseUInt32 :: BL.ByteString -> Integer
parseUInt8 = toInteger . parseUInt8'
parseInt8 = sintParser . parseUInt8'
parseUInt16 = toInteger . parseUInt16'
parseInt16 = sintParser . parseUInt16'
parseUInt32 = toInteger . parseUInt32'
parseInt32 = sintParser . parseUInt32'

sintParser :: (Integral a, Bits a) => a -> Integer
sintParser p =
    let ui = p
        c2 = toInteger $ 1 + (complement ui)
        msbSet = testBit ui ((bitSize ui) - 1)
    in if (msbSet) then (negate c2)  else (toInteger ui)
 
drsToTree' :: DataDescriptiveRecord -> [DataFieldR] -> Tree DataFieldR
drsToTree' ddr dfs  =
    let cs' k = ddrLookupChildFields ddr k
        cs k = filter (\(k',_) -> k' `elem` (cs' k)) dfs
    in head $ unfoldForest (\b@(k,_) -> (b, cs k)) dfs

ddrLookupChildFields :: DataDescriptiveRecord -> String -> [String]
ddrLookupChildFields ddr fn = findChildren [] fn (ddrFieldStructure ddr)
    where findChildren :: [String] -> String -> [(String,String)] -> [String]
          findChildren r _ [] = r
          findChildren r a ((b,c):bs) = findChildren (if (a == b) then (c:r) else r) a bs


tryMaybe :: Parser a -> Parser (Maybe a)
tryMaybe p = choice [ fmap Just $ try p, return Nothing]


parseDataFormatLength :: Parser (Maybe Integer)
parseDataFormatLength = tryMaybe $ do
  _ <- C8.char8 '('
  ds <- C8.digit `manyTill` C8.char8 ')'
  return $ read ds

parseDataFormat' :: Parser DataFormat
parseDataFormat' =
    let parsers =                   
            [ do _ <- C8.char 'A'
                 fl <- parseDataFormatLength
                 return $ CharacterData fl
            , do _ <- C8.char 'I'
                 fl <- parseDataFormatLength
                 return $ ImplicitPoint fl
            , do _ <- C8.char 'R'
                 fl <- parseDataFormatLength                  
                 return $ ExplicitPoint fl
            , do _ <- C8.char 'B'
                 fl <- parseDataFormatLength
                 return $ BitString fl
            , do _ <- C8.char '@'
                 fl <- parseDataFormatLength
                 return $ SubFieldLabel fl
            , do _ <- C8.char 'b'
                 t <- choice $ map try 
                      [ C8.char '1', C8.char '2']
                 l <- fmap toInteger $ parseInt 1 
                 case t of
                   '1' -> return $ UnsignedInt l
                   '2' -> return $ SignedInt l
                   i -> error $ "must be 1 (unsigned) or 2 (signed) not: " ++ show i
            ]
    in choice $ map try parsers

parseDataFormat :: Parser (Integer, DataFormat)
parseDataFormat = do 
  mul <- tryMaybe $ fmap read $ try $ many1 C8.digit
  fmt <- parseDataFormat'
  return (maybe 1 id mul, fmt)


parseDataFormats :: Parser [DataFormat]
parseDataFormats = do
  _ <- C8.char '('
  fmts <- sepBy parseDataFormat (C8.char ',')
  _ <- C8.char ')'
  return $ concat $ map (\ (c,f) -> replicate (fromInteger c) f) fmts



parseLeader ::
    Parser (Char, Char, Char, Char, Char, Int, String, Int)

parseLeader = do 
  len <- parseInt 5 <?> "record len"
  ichglvl <- C8.anyChar    <?> "interchange level"
  lid <- C8.anyChar   <?> "leader identifier"
  ext <- C8.anyChar   <?> "In line code extension indicator"
  ver <- C8.anyChar   <?> "version number"
  appi <- C8.anyChar  <?> "application indicator"
  fcl <- parseFCL <?> "field control length"
  baseAddr <- parseInt 5 <?> "field control length"
  extCharSet <- count 3 C8.anyChar <?> "Extended character set indicator"
               
  let fieldAreaLen = len - baseAddr
  return (ichglvl, lid, ext, ver, appi, fcl, extCharSet, fieldAreaLen)

parseFCL :: Parser Int
parseFCL = choice [
            try $ parseInt 2,
            do _ <- count 2 $ C8.char ' '
               return 0
           ]

parseEntryMap :: Parser (Int, [(String, Int, Int)])
parseEntryMap = do
  sFieldLengthF <- parseInt 1 <?> "size of field length field"
  sFieldPosF <- parseInt 1 <?> "size of field position field"
  _ <- C8.char8 '0'
  sFieldTagF <- parseInt 1 <?> "size of field tag field"
  let dirParser = 
          do ftag <- count sFieldTagF C8.anyChar
             flen <- parseInt sFieldLengthF
             fpos <- parseInt sFieldPosF
             return (ftag, fpos, flen)
  ds <- manyTill dirParser (try $ parseRT)
  return (sFieldTagF, ds)



parseInt :: Int -> Parser Int
parseInt l = fmap read (count l $ C8.anyChar)

parseDDFCtrl :: Parser (DataStructureCode, DataTypeCode, Char, Char, LexicalLevel)
parseDDFCtrl = do 
    s <- parseInt 1 <?> "data structure code"
    t <- parseInt 1 <?> "data type code"
    _ <- (count 2 $ C8.char '0') <?> "required 00 characters"
    f <- C8.anyChar <?> "printable field terminator"
    u <- C8.anyChar <?> "printable unit terminator"
    esc' <- count 3 C8.anyChar
    let esc = 
            if (esc' == "   ") 
            then 0 else if (esc' == "-A ") 
                        then 1 else if (esc' == "%/@") then 2 else
                                        error $ "unknown escape sequence " ++ esc'
    return (toEnum s, toEnum t, f,u, toEnum esc)

parseTagPair :: Int -> Parser (String, String)
parseTagPair sFieldTagF = do
  k <- count sFieldTagF C8.anyChar
  v <- count sFieldTagF C8.anyChar
  return (k,v)

parseFieldControlField :: Int -> Int -> Parser FieldControlField
parseFieldControlField fcl sFieldTagF = do
  fieldCtrlBS <- P.take fcl
  case (parseOnly parseDDFCtrl fieldCtrlBS) of
    Left err -> fail err
    Right (SingleDataItem, CharacterString, _ ,_,esc) -> 
        do name <-  manyTill C8.anyChar (try $ parseUT) <?> "name" 
           fieldTags <- manyTill (parseTagPair sFieldTagF) (try $ parseRT)
           return (toEnum 0, toEnum 0,esc,name, fieldTags) 
    Right _ -> fail $ "first DDR filed must have tag 0000"



parseDataDescr :: DataStructureCode -> Parser DataDescr
parseDataDescr SingleDataItem = do 
  fn <- manyTill C8.anyChar (try $ parseUT)
  return (SingleDataItem, [fn])
parseDataDescr LinearStructure = do
  fs <- (many1 $ C8.satisfy $ C8.notInClass (unitTerminator:"!")) `sepBy` (try $ C8.char '!')
  parseUT
  return (LinearStructure, fs)
parseDataDescr MultiDimStructure = do 
  _ <- C8.char '*'
  fs <- (many1 $ C8.satisfy $ C8.notInClass (unitTerminator:"!")) `sepBy` (try $ C8.char '!')
  parseUT
  return (MultiDimStructure, fs)


parseDataDescriptiveField ::
  Int -> Parser DataDescriptiveField
parseDataDescriptiveField fcl = do
  fieldCtrlBS <- P.take fcl
  case (parseOnly parseDDFCtrl fieldCtrlBS) of
    Left err -> fail err
    Right (s,t,_,_,esc) -> 
        do name <-  manyTill C8.anyChar (try $ parseUT) <?> "name" 
           label <- (do (_,ls) <- parseDataDescr s; return ls) <?> "label"
           format <- (do fs <- parseDataFormats; parseRT; return fs) <?> "format"
           return (s, t, esc,name, zip label format) 

