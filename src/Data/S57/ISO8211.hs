{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Data.S57.ISO8211 (DataFile
               ,DataDescriptiveRecord
               ,DataRecord
               ,DataDescriptiveField
               ,DataStructureCode (..)
               ,DataTypeCode (..)
               ,TruncedEscapeSequence (..)
               ,DataStructure (..)
               ,DataFormat (..)
               ,DataField(..)
               ,parseDataFile
               ,parseDDR, parseDDR'
               ,parseDR, parseDR'
               ,ddrFileName
               ,ddrFieldStructure
               ,ddrDataFormats
               ) where

import Data.ByteString (ByteString)

import Data.Binary
import Data.Binary.Get
import Data.Bits
import Data.Int
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
    fromDataFieldT :: DataFieldT -> t
    toDataFieldT  :: t -> DataFieldT

instance DataField String where
    fromDataFieldT (DFString s) = s
    fromDataFieldT v = error $ "fromDataFieldT is not a String: " ++ show v
    toDataFieldT = DFString

instance DataField Integer where
    fromDataFieldT (DFInteger i) = i
    fromDataFieldT v = error $ "fromDataFieldT is not an Integer: " ++ show v
    toDataFieldT = DFInteger

instance DataField Double where
    fromDataFieldT (DFReal r) = r
    fromDataFieldT v = error $ "fromDataFieldT is not a Double: " ++ show v
    toDataFieldT = DFReal

instance DataField ByteString where
    fromDataFieldT (DFByteString bs) = bs
    fromDataFieldT v = error $ "fromDataFieldT is not a ByteString: " ++ show v
    toDataFieldT = DFByteString
  
type DataDescriptiveField =
    (DataStructureCode, DataTypeCode, TruncedEscapeSequence, String, [(String, DataFormat)])

data DataStructureCode = 
    SingleDataItem | LinearStructure | MultiDimStructure
    deriving (Eq, Enum, Show)

data DataTypeCode = 
    CharacterString | ImplicitPointInt | ImplicitPointReal | BinaryForm | MixedDataType
    deriving (Eq, Show)

data TruncedEscapeSequence = 
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
    (DataStructureCode, DataTypeCode, TruncedEscapeSequence, String, [(String, String)])

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
  (sFieldTagF, ds) <- parseEntryMap
  bs <- P.take fieldAreaLen

  let ((_, rr):rs) = [(t, BS.take l $ BS.drop p bs) | (t,p,l) <- ds] 
  let (_,_,_,fname, fstruct) = 
          either error id $ parseOnly  (parseFieldControlField fcl sFieldTagF) rr
  
  let rs'= [(a, either error id $ parseOnly 
             (parseDataDescriptiveField fcl sFieldTagF) b) | (a,b) <- rs]

  let ddr =  DDR fname fstruct $ Map.fromList rs'
  return ((ichglvl, lid, ext, ver, appi, extCharSet), ddr)

parseDR :: DataDescriptiveRecord -> Parser DataRecord
parseDR ddr = fmap snd (parseDR' ddr)

parseDR' :: DataDescriptiveRecord -> Parser ((Char, Char, Char, Char, Char, String), DataRecord)
parseDR' ddr = do
  (ichglvl, lid, ext, ver, appi, fcl, extCharSet, fieldAreaLen) <- parseLeader
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
    [(String, DataFormat)] -> TruncedEscapeSequence -> DataStructureCode 
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



dataFormatToParser :: TruncedEscapeSequence -> DataFormat  -> Parser DataFieldT
dataFormatToParser esc (CharacterData l) = fmap DFString $
    case l of
      Nothing -> C8.anyChar `manyTill` (try $ parseUT)
      Just i ->  count (fromInteger i) C8.anyChar
dataFormatToParser esc (ImplicitPoint l) = do
    (DFString s) <- dataFormatToParser esc (CharacterData l)
    return $ DFInteger (read s)
dataFormatToParser esc (ExplicitPoint l) = do
    (DFString s) <- dataFormatToParser esc (CharacterData l)
    return $ DFReal (read s)
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
dataFormatToParser _ (SignedInt l) = do
  bs <- P.take (fromInteger l)
  case l of
    1 -> return $ DFInteger $ toInteger $ parseInt8  (BL.fromChunks [bs])
    2 -> return $ DFInteger $ toInteger $ parseInt16 (BL.fromChunks [bs])
    4 -> return $ DFInteger $ toInteger $ parseInt32 (BL.fromChunks [bs])


parseUInt8' = runGet getWord8
parseUInt8 = toInteger . parseUInt8'
parseInt8 = sintParser . parseUInt8'
parseUInt16' = runGet getWord16le
parseUInt16 = toInteger . parseUInt16'
parseInt16 = sintParser . parseUInt16'
parseUInt32' = runGet getWord32le
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
    in head $ unfoldForest (\b@(k,v) -> (b, cs k)) dfs


ddrLookupParentField :: DataDescriptiveRecord -> String -> Maybe String
ddrLookupParentField ddr fn = findParent (ddrFieldStructure ddr) fn
    where findParent [] _ = Nothing
          findParent ((b,c):bs) a = if a == c then Just b else findParent bs a

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
  fcl <- parseInt 2 <?> "field control length"
  baseAddr <- parseInt 5 <?> "field control length"
  extCharSet <- count 3 C8.anyChar <?> "Extended character set indicator"
               
  let fieldAreaLen = len - baseAddr
  return (ichglvl, lid, ext, ver, appi, fcl, extCharSet, fieldAreaLen)


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

parseDDFCtrl :: Parser (DataStructureCode, DataTypeCode, Char, Char, TruncedEscapeSequence)
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
    Right (SingleDataItem, CharacterString, f,u,esc) -> 
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
  Int -> t -> Parser DataDescriptiveField
parseDataDescriptiveField fcl sFieldTagF = do
  fieldCtrlBS <- P.take fcl
  case (parseOnly parseDDFCtrl fieldCtrlBS) of
    Left err -> fail err
    Right (s,t,f,u,esc) -> 
        do name <-  manyTill C8.anyChar (try $ parseUT) <?> "name" 
           label <- (do (_,ls) <- parseDataDescr s; return ls) <?> "label"
           format <- (do fs <- parseDataFormats; parseRT; return fs) <?> "format"
           return (s, t, esc,name, zip label format) 

