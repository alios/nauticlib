{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Data.S57 (
  -- * Record types
  -- ** Data set descriptive records
  -- *** Data set general information record (DSID)
  DSID (..), dsid,
  DSSI (..),
  -- *** Data set geographic reference record (DSPM)
  DSPM (..),
  DSPR (..),
  DSRC (..),
  -- *** Data set history record (DSHT)
  DSHT (..),
  -- *** Data set accuracy record (DSAC)
  DSAC (..),
  
  -- ** Catalog records
  -- *** Catalog directory record (CATD)
  CATD (..),
  -- *** Catalog cross reference record (CATX)
  CATX (..),

  -- ** Data dictionary records
  -- ** Feature records
  -- ** Spatial records
  VRID (..), vrid, vrids,
  SG3D (..),
  
  -- * Other data types
  PRSP (..),
  PROF (..),
  EXPP (..),
  DSTR (..),
  VRID_RCNM (..),
  RUIN (..)
) where

import Data.S57.ISO8211
import Data.Tree
import Data.List
import Data.Maybe
import Data.Time.Calendar
import Data.Time.Format
import System.Locale
import qualified Data.Map as Map
import Data.Word

type AGEN = Integer


-- | Data set Identification field structure 'DSID'
data DSID = DSID {
      dsid_rcnm :: !Word32, -- ^ Record name
      dsid_rcid :: !Integer, -- ^ Record identification number
      dsid_expp :: !EXPP, -- ^ Exchange purpose
      dsid_intu :: !Integer, -- ^ Intended usage. A numeric value indicating the inteded usage for wich the data has been compiled.
      dsid_dsnm :: !String, -- ^ Data set name
      dsid_edtn :: !String, -- ^ Edition number
      dsid_updn :: !String, -- ^ Update number
      dsid_uadt :: Maybe Day, -- ^ Update application date 
      dsid_isdt :: !Day, -- ^ Issue date
      dsid_sted :: !Double, -- ^ Edition number of S-57 (3.0, 3.1)
      dsid_prsp :: !PRSP, -- ^ Product specification
      dsid_psdn :: !String, -- ^ Product specification description. A string identifying a non standard product specification
      dsid_pred :: !String, -- ^ Product specification edition number
      dsid_prof :: !PROF, -- ^ Application profile identification
      dsid_agen :: !AGEN, -- ^ Producing agency
      dsid_comt  :: !String, -- ^ Comment
      dsid_dssi :: !DSSI -- ^ Data set structure information field
    } deriving (Eq, Show)
  
-- | Data set structure information field 'DSSI'
data DSSI = DSSI {
      dssi_dstr :: !DSTR, -- ^ Data structure
      dssi_aall :: !LexicalLevel, -- ^ 'ATTF' lexical level
      dssi_nall :: !LexicalLevel, -- ^ 'NATF' lecical level
      dssi_nomr :: !Integer, -- ^ Number of meta records
      dssi_nocr :: !Integer, -- ^ Number of cartographic records
      dssi_nogr :: !Integer, -- ^ Number of geo records
      dssi_nolr :: !Integer, -- ^ Number of collection records
      dssi_noin :: !Integer, -- ^ Number of isolated node records
      dssi_nocn :: !Integer, -- ^ Number of connected node records
      dssi_noed :: !Integer, -- ^ Number of edge records
      dssi_nofa :: !Integer  -- ^ Number of face records    
    } deriving (Eq, Show)


-- | Data set parameter field 'DSPM'
data DSPM = DSPM {
      dspm_rcnm :: Word32 -- ^ Record name
}

-- | Data set projection field (DSPR)
data DSPR = DSPR
-- | Data set registration conrol (DSRC)
data DSRC = DSRC
-- | Data set history recrord (DSHT)
data DSHT = DSHT
-- | Data set accuracy record (DSAC)
data DSAC = DSAC
-- | Catalogue directory record (CATD)
data CATD = CATD
-- | Catalogue cross refernce record (CATX)
data CATX = CATX

-- | Vector record
data VRID = VRID {
      vrid_rcnm :: !VRID_RCNM, -- ^ Record name
      vrid_rcid :: !Word32, -- ^ Record identification number 
      vrid_rver :: !Integer, -- ^ Record version
      vrid_ruin :: !RUIN, -- ^ Record update instruction
      vrid_sg3ds :: [SG3D] -- ^ -- 3-D coodriunat (Sounding Array) field
} deriving (Eq, Show)

data SG3D = SG3D {
      sg3d_ycoo :: !Integer, -- ^ Coordinat in Y axis
      sg3d_xcoo :: !Integer, -- ^ Coordinat in X axis
      sg3d_ve3d :: !Integer -- ^ 3-D (sounding) value
} deriving (Eq, Show)


--
-- data types
--

data EXPP 
    = DataSetIsNew 
    | DataSetIsRevision
    deriving (Eq, Show)

data DSTR 
    = CartographicSpaghetti
    | ChainMode
    | PlanarGraph
    | FullTopology
    | TopologyNotRelevant
    deriving (Eq, Show)

data PRSP
    = ElectronicNavigationalChart 
    | IHOObjectCatalogueDataDictionary
    deriving (Eq, Show)

data PROF 
    = ENCNew 
    | ENCRevision 
    | IHODataDictionary
    deriving (Eq, Show)


data VRID_RCNM 
    = IsolatedNode
    | ConnectedNode
    | Edge
    | Face
    deriving (Eq, Show)

data RUIN 
    = Insert
    | Delete
    | Modify
    deriving (Eq, Show)


--
-- exported functions
--

-- | get the 'DSID' from a ISO-8211 'DataFile' 
dsid :: DataFile -> DSID
dsid df = 
 let dr = findRecordByTag "DSID" df 
 in DSID {
          dsid_rcnm = sdRecordField dr "RCNM",
          dsid_rcid = sdRecordField dr "RCID",
          dsid_expp = sdRecordField dr "EXPP",
          dsid_intu = sdRecordField dr "INTU",
          dsid_dsnm = sdRecordField dr "DSNM",
          dsid_edtn = sdRecordField dr "EDTN",
          dsid_updn = sdRecordField dr "UPDN",
          dsid_uadt = sdRecordField dr "UADT",
          dsid_isdt = sdRecordField dr "ISDT",
          dsid_sted = sdRecordField dr "STED",
          dsid_prsp = sdRecordField dr "PRSP",
          dsid_psdn = sdRecordField dr "PSDN",
          dsid_pred = sdRecordField dr "PRED",
          dsid_prof = sdRecordField dr "PROF",
          dsid_agen = sdRecordField dr "AGEN",
          dsid_comt = sdRecordField dr "COMT",
          dsid_dssi = mkDSSI dr
        }
    
mkDSSI :: DataRecord -> DSSI
mkDSSI r = 
    let dssi = findSubRecord "DSSI" r
    in DSSI {
             dssi_dstr = sdRecordField dssi "DSTR",
             dssi_aall = sdRecordField dssi  "AALL",
             dssi_nall = sdRecordField dssi  "NALL",
             dssi_nomr = sdRecordField dssi  "NOMR",
             dssi_nocr = sdRecordField dssi  "NOCR",
             dssi_nogr = sdRecordField dssi  "NOGR",
             dssi_nolr = sdRecordField dssi  "NOLR",
             dssi_noin = sdRecordField dssi  "NOIN",
             dssi_nocn = sdRecordField dssi  "NOCN",
             dssi_noed = sdRecordField dssi  "NOED",
             dssi_nofa = sdRecordField dssi "NOFA"
           }


--vrids :: DataFile -> [VRID]
vrids = map vrid .  findRecordsByTag "VRID"

vrid    :: DataRecord -> VRID
vrid dr = VRID {
             vrid_rcnm = sdRecordField dr "RCNM",
             vrid_rcid = sdRecordField dr "RCID",
             vrid_rver = sdRecordField dr "RVER",
             vrid_ruin = sdRecordField dr "RUIN",
             vrid_sg3ds = sg3ds dr
           }

sg3ds :: DataRecord -> [SG3D]
sg3ds r = maybe [] (map sg3d) $ mdRecords' "SG3D" r

sg3d :: Map.Map String DataFieldT -> SG3D
sg3d m = SG3D {
      sg3d_ycoo = mdRecordField "YCOO" m,
      sg3d_xcoo = mdRecordField "XCOO" m,
      sg3d_ve3d = mdRecordField "VE3D" m
         }

--
-- helper functions
--

dropISORoot :: DataRecord -> DataRecord
dropISORoot r 
    | ((fst $ rootLabel r) == "0001") = head . subForest $ r
    | otherwise = error $ "node is not a ISO 8211 Record Identifier:" ++ show r

findRecordsByTag :: String -> DataFile -> [DataRecord]
findRecordsByTag t (_, rs) =
    filter (\n -> (fst . rootLabel $ n) == t) $ map dropISORoot rs

findRecordByTag' :: String -> DataFile -> Maybe DataRecord
findRecordByTag' t f = 
    case (findRecordsByTag t f) of
      [] -> Nothing
      (x:_) -> Just x

findRecordByTag :: String -> DataFile -> DataRecord
findRecordByTag t f = 
    maybe (error $ "unable to findRecordByTag: " ++ t)
          id $ findRecordByTag' t f

findRecordFieldLS :: String ->  DataRecord -> DataFieldT
findRecordFieldLS t dr = 
   let fs = dsLinearStruct . snd . rootLabel $ dr
   in case (t `Map.lookup` fs) of
        Nothing -> error $ "unable to find subfield: " ++ t
        Just f -> f


findSubRecords :: String -> DataRecord -> [DataRecord]
findSubRecords t dr =
    filter (\n -> (fst $ rootLabel n) == t) (subForest dr)


findSubRecord' :: String -> DataRecord -> Maybe DataRecord
findSubRecord' t r = 
    case (findSubRecords t r) of
      [] -> Nothing
      (x:_) -> Just x

findSubRecord :: String -> DataRecord -> DataRecord
findSubRecord t r = 
    maybe (error $ "unable to findSubRecordByTag: " ++ t)
          id $ findSubRecord' t r

sdRecordField :: DataField t => DataRecord -> String -> t
sdRecordField dr t = fromDataField (findRecordFieldLS t dr)

mdRecords' :: String -> DataRecord -> Maybe [Map.Map String DataFieldT]
mdRecords' t r = maybe Nothing (Just . dsMultiDimStruct . snd . rootLabel) 
                 $ findSubRecord' t r

mdRecords :: String -> DataRecord -> [Map.Map String DataFieldT]
mdRecords t r = 
    maybe (error $ "unable to find mdRecord: " ++ t) id 
              $ mdRecords' t r
                  
mdRecordField :: DataField c => String -> Map.Map String DataFieldT -> c
mdRecordField t m = 
    fromDataField . maybe (error $ "unable to find tag: " ++ t) id 
      $ t `Map.lookup` m


--
-- instance declarations
--
instance DataField DSTR where
    fromDataField (DFInteger i) = 
        case i of
          1 -> CartographicSpaghetti
          2 -> ChainMode
          3 -> PlanarGraph
          4 -> FullTopology
          5 -> TopologyNotRelevant
    fromDataField f = error $ "unable to decode DSTR from:" ++ show f


instance DataField Word32 where
    fromDataField (DFInteger i) = fromInteger i
    fromDataField v = error $ "invalid word32 " ++ show v

instance DataField EXPP where
    fromDataField (DFString s) = 
        if (s == "N") then DataSetIsNew else
            if (s == "R") then DataSetIsRevision else
                error $ "invalid EXPP: " ++ show s
    fromDataField (DFInteger i) = 
        if (i == 1) then DataSetIsNew else
            if (i == 2) then DataSetIsRevision else
                error $ "invalid EXPP: " ++ show i
    fromDataField f = error $ "unable to decode ExchangePurpose from:" ++ show f

instance DataField PRSP where
    fromDataField (DFString s) = 
        if (s == "ENC") then ElectronicNavigationalChart else
            if (s == "ODD") then IHOObjectCatalogueDataDictionary else
                error $ "invalid ProductSpec: " ++ s
    fromDataField (DFInteger i) = 
        case i of
          1 -> ElectronicNavigationalChart
          2 -> IHOObjectCatalogueDataDictionary
          i -> error $ "invalid ProductSpec: " ++ show i
    fromDataField f = error $ "unable to decode ProductSpec from:" ++ show f


instance DataField VRID_RCNM where
    fromDataField (DFString s) = 
        if (s == "VI") then IsolatedNode else
            if (s == "VC") then ConnectedNode else
                if (s == "VE") then Edge else
                    if (s == "VF") then Face else
                        error $ "invalid VRID_RCNM: " ++ s
    fromDataField (DFInteger i) = 
        case i of
          110 -> IsolatedNode
          120 -> ConnectedNode
          130 -> Edge
          140 -> Face
          i -> error $ "invalid VRID_RCNM: " ++ show i
    fromDataField f = error $ "unable to decode VRID_RCNM from:" ++ show f

instance DataField PROF where
    fromDataField (DFString s) = 
        if (s == "EN") then ENCNew else
            if (s == "ER") then ENCRevision else
                if (s == "DD") then IHODataDictionary else
                    error $ "invalid PROF: " ++ s
    fromDataField (DFInteger i) = 
        case i of
          1 -> ENCNew
          2 -> ENCRevision
          3 -> IHODataDictionary
          i -> error $ "invalid PROF: " ++ show i
    fromDataField f = error $ "unable to decode PROF from:" ++ show f


instance DataField (Maybe Day) where
    fromDataField (DFString s) = 
        parseTime defaultTimeLocale "%Y%m%d" s
    fromDataField f = error $ "unable to decode Date from:" ++ show f
    
instance DataField Day where
    fromDataField f = 
        maybe (error $ "unable to parse date: " ++ show f) id $ fromDataField f
 

instance DataField LexicalLevel where
    fromDataField (DFInteger i) = 
        case i of
          0 -> LexicalLevel0
          1 -> LexicalLevel1
          2 -> LexicalLevel2
          i -> error $ "invalid LexicalLevel: " ++ show i
    fromDataField i = error $ "unable to decode lexical level from:" ++ show i
                      
instance DataField RUIN where
    fromDataField (DFString s) = 
        if (s == "I") then Insert  else
            if (s == "D") then Delete else
                if (s == "M") then Modify else
                    error $ "invalid PROF: " ++ s
    fromDataField (DFInteger i) = 
        case i of
          1 -> Insert
          2 -> Delete
          3 -> Modify
    
