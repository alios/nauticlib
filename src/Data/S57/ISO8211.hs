module Data.S57.ISO8211 (DataFile
               ,DataDescriptiveRecord
               ,DataRecord
               ,DataDescriptiveField
               ,DataStructureCode (..)
               ,DataTypeCode (..)
               ,TruncedEscapeSequence (..)
               ,DataStructure (..)
               ,DataFormat (..)
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
import Data.Int
import Data.Tree
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Data.Attoparsec as P
import qualified Data.Attoparsec.ByteString.Char8 as C8

--
-- external type definitions
--

type DataFile = (DataDescriptiveRecord, [DataRecord])

type DataRecord = [DataField]
type DataField = (String, DataStructure)

data DataDescriptiveRecord = 
    DDR {
      ddrFileName :: !String,
      ddrFieldStructure :: ![(String, String)],
      ddrDataFormats :: ![(String, DataDescriptiveField)]
    } deriving (Show, Eq)

data DataFieldT =
    DFString !String | DFInteger !Integer | DFReal !Double | DFByteString !ByteString
    deriving (Eq, Show)
                 
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
                   | LS [(String, DataFieldT)] 
                   | MDS [[(String, DataFieldT)]]
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

  let (r@(rt, rr):rs) = [(t, BS.take l $ BS.drop p bs) | (t,p,l) <- ds] 
  let r'@(_,_,_,fname, fstruct) = 
          either error id $ parseOnly  (parseFieldControlField fcl sFieldTagF) rr
  
  let rs'= [(a, either error id $ parseOnly 
             (parseDataDescriptiveField fcl sFieldTagF) b) | (a,b) <- rs]

  let ddr =  DDR fname fstruct rs'
  return ((ichglvl, lid, ext, ver, appi, extCharSet), ddr)

parseDR :: DataDescriptiveRecord -> Parser DataRecord
parseDR ddr = fmap snd (parseDR' ddr)

parseDR' :: DataDescriptiveRecord -> Parser ((Char, Char, Char, Char, Char, String), DataRecord)
parseDR' ddr = do
  (ichglvl, lid, ext, ver, appi, fcl, extCharSet, fieldAreaLen) <- parseLeader
  (sFieldTagF, ds) <- parseEntryMap
  bs <- P.take fieldAreaLen
  let bss = [ (t, BS.take l $ BS.drop p bs) | (t,p,l) <- ds] 
  let dfs = map (\(t, bs) -> (t, either error id $ 
                              parseOnly (ddrParserLookup ddr t) bs)) bss
  return ((ichglvl, lid, ext, ver, appi, extCharSet), dfs)


--
-- internal
-- 
ddrParserLookup :: DataDescriptiveRecord -> String -> Parser DataStructure
ddrParserLookup ddr t = do
  let (sc, _, esc, _, dfs) = maybe (error $ "unknown field: " ++ t) id 
                             $ lookup t (ddrDataFormats ddr)
  dfsParser dfs esc sc

dfsParser :: 
    [(String, DataFormat)] -> TruncedEscapeSequence -> DataStructureCode 
    -> Parser DataStructure
dfsParser dfs esc sc =
    let mfp = sequence $ map (fieldParser esc) $ dfs
        fieldParser esc (t, p) = do
          v <- dataFormatToParser esc p
          return $ (t, v)
    in do res <- case sc of 
                  SingleDataItem -> do
                      (_, r) <- fmap head $  mfp
                      _ <- C8.char '\RS'
                      return $ SD r
                  LinearStructure -> do
                      r <- fmap LS mfp
                      _ <- C8.char '\RS'
                      return r
                  MultiDimStructure ->
                      fmap MDS $ mfp `manyTill` (try $ C8.char '\RS')
          return res


dataFormatToParser :: TruncedEscapeSequence -> DataFormat  -> Parser DataFieldT
dataFormatToParser esc (CharacterData l) = fmap DFString $
    case l of
      Nothing -> C8.anyChar `manyTill` (try $ C8.char '\US')
      Just i ->  count (fromInteger i) C8.anyChar
dataFormatToParser esc (ImplicitPoint l) = do
    (DFString s) <- dataFormatToParser esc (CharacterData l)
    return $ DFInteger (read s)
dataFormatToParser esc (ExplicitPoint l) = do
    (DFString s) <- dataFormatToParser esc (CharacterData l)
    return $ DFReal (read s)
dataFormatToParser _ (BitString l) = fmap DFByteString $
    case l of 
      Nothing -> fmap BS.pack $ anyWord8 `manyTill` (try $ C8.char '\US')
      Just l -> P.take (fromInteger l `div` 8)
dataFormatToParser _ (UnsignedInt l) = do
  bs <- P.take (fromInteger l)
  case l of
    1 -> return $ DFInteger $ toInteger $ runGet getWord8 (BL.fromChunks [bs])
    2 -> return $ DFInteger $ toInteger $ runGet getWord16le (BL.fromChunks [bs])
    4 -> return $ DFInteger $ toInteger $ runGet getWord32le (BL.fromChunks [bs])
dataFormatToParser _ (SignedInt l) = do
  bs <- P.take (fromInteger l)
  case l of
    1 -> return $ DFInteger $ toInteger $ runGet (get :: Get Int8) (BL.fromChunks [bs])
    2 -> return $ DFInteger $ toInteger $ runGet (get :: Get Int16) (BL.fromChunks [bs])
    4 -> return $ DFInteger $ toInteger $ runGet (get :: Get Int32) (BL.fromChunks [bs])

-- TODO
drsToTree :: DataDescriptiveRecord -> [DataField] -> Tree DataField
drsToTree ddr [] = undefined

ddrLookupParentField :: DataDescriptiveRecord -> String -> Maybe String
ddrLookupParentField ddr c = findParent (ddrFieldStructure ddr) c
    where findParent [] _ = Nothing
          findParent ((b,c):bs) a = if a == c then Just b else findParent bs a

ddrLookupChildFields :: DataDescriptiveRecord -> String -> [String]
ddrLookupChildFields ddr c = findChildren [] c (ddrFieldStructure ddr)
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
            [ do C8.char 'A'
                 fl <- parseDataFormatLength
                 return $ CharacterData fl
            , do C8.char 'I'
                 fl <- parseDataFormatLength
                 return $ ImplicitPoint fl
            , do C8.char 'R'
                 fl <- parseDataFormatLength                  
                 return $ ExplicitPoint fl
            , do C8.char 'B'
                 fl <- parseDataFormatLength
                 return $ BitString fl
            , do C8.char '@'
                 fl <- parseDataFormatLength
                 return $ SubFieldLabel fl
            , do C8.char 'b'
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
  ds <- manyTill dirParser (try $ C8.char8 '\RS')
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
        do name <-  manyTill C8.anyChar (try $ C8.char8 '\US') <?> "name" 
           fieldTags <- manyTill (parseTagPair sFieldTagF) (try $ C8.char8 '\RS')
           return (toEnum 0, toEnum 0,esc,name, fieldTags) 
    Right _ -> fail $ "first DDR filed must have tag 0000"



parseDataDescr :: DataStructureCode -> Parser DataDescr
parseDataDescr SingleDataItem = do 
  fn <- manyTill C8.anyChar (try $ C8.char '\US')
  return (SingleDataItem, [fn])
parseDataDescr LinearStructure = do
  fs <- (many1 $ C8.satisfy $ C8.notInClass "!\US") `sepBy` (try $ C8.char '!')
  C8.char '\US'
  return (LinearStructure, fs)
parseDataDescr MultiDimStructure = do 
  _ <- C8.char '*'
  fs <- (many1 $ C8.satisfy $ C8.notInClass "!\US") `sepBy` (try $ C8.char '!')
  C8.char '\US'
  return (MultiDimStructure, fs)


parseDataDescriptiveField ::
  Int -> t -> Parser DataDescriptiveField
parseDataDescriptiveField fcl sFieldTagF = do
  fieldCtrlBS <- P.take fcl
  case (parseOnly parseDDFCtrl fieldCtrlBS) of
    Left err -> fail err
    Right (s,t,f,u,esc) -> 
        do name <-  manyTill C8.anyChar (try $ C8.char8 '\US') <?> "name" 
           label <- (do (_,ls) <- parseDataDescr s; return ls) <?> "label"
           format <- (do fs <- parseDataFormats; C8.char8 '\RS'; return fs) <?> "format"
           return (s, t, esc,name, zip label format) 

