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
      dsid_rcnm :: Word32, -- ^ Record name
      dsid_rcid :: Integer, -- ^ Record identification number
      dsid_expp :: EXPP, -- ^ Exchange purpose
      dsid_intu :: Integer, -- ^ Intended usage. A numeric value indicating the inteded usage for wich the data has been compiled.
      dsid_dsnm :: String, -- ^ Data set name
      dsid_edtn :: String, -- ^ Edition number
      dsid_updn :: String, -- ^ Update number
      dsid_uadt :: Day, -- ^ Update application date 
      dsid_isdt :: Day, -- ^ Issue date
      dsid_sted :: Double, -- ^ Edition number of S-57 (3.0, 3.1)
      dsid_prsp :: PRSP, -- ^ Product specification
      dsid_psdn :: String, -- ^ Product specification description. A string identifying a non standard product specification
      dsid_pred :: String, -- ^ Product specification edition number
      dsid_prof :: PROF, -- ^ Application profile identification
      dsid_agen :: AGEN, -- ^ Producing agency
      dsid_comt  :: String, -- ^ Comment
      dsid_dssi :: DSSI -- ^ Data set structure information field
    } deriving (Eq, Show)
  
-- | Data set structure information field 'DSSI'
data DSSI = DSSI {
      dssi_dstr :: DSTR, -- ^ Data structure
      dssi_aall :: LexicalLevel, -- ^ 'ATTF' lexical level
      dssi_nall :: LexicalLevel, -- ^ 'NATF' lecical level
      dssi_nomr :: Integer, -- ^ Number of meta records
      dssi_nocr :: Integer, -- ^ Number of cartographic records
      dssi_nogr :: Integer, -- ^ Number of geo records
      dssi_nolr :: Integer, -- ^ Number of collection records
      dssi_noin :: Integer, -- ^ Number of isolated node records
      dssi_nocn :: Integer, -- ^ Number of connected node records
      dssi_noed :: Integer, -- ^ Number of edge records
      dssi_nofa :: Integer  -- ^ Number of face records    
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
      vrid_rcnm :: VRID_RCNM, -- ^ Record name
      vrid_rcid :: Word32, -- ^ Record identification number 
      vrid_rver :: Integer, -- ^ Record version
      vrid_ruin :: RUIN -- ^ Record update instruction
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
             dssi_dstr = fromDataField $ subRecordField "DSTR" dssi,
             dssi_aall = fromDataField $ subRecordField "AALL" dssi,
             dssi_nall = fromDataField $ subRecordField "NALL" dssi,
             dssi_nomr = fromDataField $ subRecordField "NOMR" dssi,
             dssi_nocr = fromDataField $ subRecordField "NOCR" dssi,
             dssi_nogr = fromDataField $ subRecordField "NOGR" dssi,
             dssi_nolr = fromDataField $ subRecordField "NOLR" dssi,
             dssi_noin = fromDataField $ subRecordField "NOIN" dssi,
             dssi_nocn = fromDataField $ subRecordField "NOCN" dssi,
             dssi_noed = fromDataField $ subRecordField "NOED" dssi,
             dssi_nofa = fromDataField $ subRecordField "NOFA" dssi
           }


vrids :: DataFile -> [VRID]
vrids = map vrid . findRecordsByTag "VRID"

vrid :: DataRecord -> VRID
vrid dr =
    let x = 1
    in VRID {
             vrid_rcnm = sdRecordField dr "RCNM",
             vrid_rcid = sdRecordField dr "RCID",
             vrid_rver = sdRecordField dr "RVER",
             vrid_ruin = sdRecordField dr "RUIN"
           }

--
-- helper functions
--


findRecordsByTag :: String -> DataFile -> [DataRecord]
findRecordsByTag t (_, rs) =
    let p n = isJust $ find (\n' -> (fst $ rootLabel n') == t) (subForest n)
    in filter p rs


findRecordByTag t f = head $ findRecordsByTag t f

findRecordField :: String ->  DataRecord -> DataFieldT
findRecordField t dr = 
   let fs = (dsLinearStruct . snd . rootLabel . head . subForest $ dr)
   in case (t `Map.lookup` fs) of
        Nothing -> error $ "unable to find subfield: " ++ t
        Just f -> f


findSubRecord :: String -> DataRecord -> DataRecord
findSubRecord t dr = 
    let srs = subForest . head . subForest $ dr
        p = (== t) . fst .  rootLabel
    in case (find p srs) of
         Nothing -> error $ "unable to find record with tag: " ++ t
         Just r -> r

subRecordValue :: DataRecord -> DataStructure
subRecordValue = snd . rootLabel

subRecordField :: String -> DataRecord -> DataFieldT
subRecordField t r = case ((Map.lookup t) . dsLinearStruct . subRecordValue $ r) of
                     Nothing -> error $ "unable to find sub record field: " ++ t
                     Just f -> f

sdRecordField :: DataField t => DataRecord -> String -> t
sdRecordField dr t = fromDataField (findRecordField t dr)


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

instance DataField Day where
    fromDataField (DFString s) = 
        maybe (error $ "unable to parse date: " ++ s) id $
        parseTime defaultTimeLocale "%Y%m%d" s
    fromDataField f = error $ "unable to decode Date from:" ++ show f
 

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
    
