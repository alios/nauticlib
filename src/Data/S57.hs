{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Data.S57 where

import Data.S57.ISO8211
import Data.Tree
import Data.List
import Data.Maybe
import Data.Time.Calendar
import Data.Time.Format
import System.Locale
import qualified Data.Map as Map

data DSID = DSID {
      dsid_rcnm :: Integer,
      dsid_rcid :: Integer,
      dsid_expp :: ExchangePurpose,
      dsid_intu :: Integer,
      dsid_dsnm :: String,
      dsid_edtn :: String,
      dsid_updn :: String,
      dsid_uadt :: Day,
      dsid_isdt :: Day,
      dsid_sted :: Double,
      dsid_prsp :: ProductSpec,
      dsid_psdn :: String,
      dsid_pred :: String,
      dsid_prof :: ApplicationProfileId,
      dsid_agen :: Integer,
      dsid_comt  :: String,
      dsid_dssi :: DSSI
    } deriving (Eq, Show)
  
data ProductSpec = 
    ElectronicNavigationalChart | IHOObjectCatalogueDataDictionary
    deriving (Eq, Show)

data ApplicationProfileId = 
    ENCNew | ENCRevision | IHODataDictionary
    deriving (Eq, Show)

data DSSI = DSSI {
      dssi_dstr :: DSTR,
      dssi_aall :: LexicalLevel,
      dssi_nall :: LexicalLevel,
      dssi_nomr :: Integer,
      dssi_nocr :: Integer,
      dssi_nogr :: Integer,
      dssi_nolr :: Integer,
      dssi_noin :: Integer,
      dssi_nocn :: Integer,
      dssi_noed :: Integer,
      dssi_nofa :: Integer                  
    } deriving (Eq, Show)


data ExchangePurpose = DataSetIsNew | DataSetIsRevision
                       deriving (Eq, Show)

data DSTR = CartographicSpaghetti
          | ChainMode
          | PlanarGraph
          | FullTopology
          | TopologyNotRelevant
                   deriving (Eq, Show)

--
-- exported functions
--
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


--
-- helper functions
--

findRecordByTag :: String -> DataFile -> DataRecord
findRecordByTag t (_, rs) =
    let p :: (DataRecord -> Bool)
        p n = isJust $ find (\n -> t == (fst . rootLabel) n) (subForest n)
    in case (find p rs) of
         Nothing -> error $ "unable to find record with tag: " ++ t
         Just r -> r

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



instance DataField ExchangePurpose where
    fromDataField (DFString s) = 
        if (s == "N") then DataSetIsNew else
            if (s == "R") then DataSetIsRevision else
                error $ "invalid EXPP: " ++ show s
    fromDataField (DFInteger i) = 
        if (i == 1) then DataSetIsNew else
            if (i == 2) then DataSetIsRevision else
                error $ "invalid EXPP: " ++ show i
    fromDataField f = error $ "unable to decode ExchangePurpose from:" ++ show f

instance DataField ProductSpec where
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
                                    
instance DataField ApplicationProfileId where
    fromDataField (DFString s) = 
        if (s == "EN") then ENCNew else
            if (s == "ER") then ENCRevision else
                if (s == "DD") then IHODataDictionary else
                    error $ "invalid ApplicationProfileId: " ++ s
    fromDataField (DFInteger i) = 
        case i of
          1 -> ENCNew
          2 -> ENCRevision
          3 -> IHODataDictionary
          i -> error $ "invalid ApplicationProfileId: " ++ show i
    fromDataField f = error $ "unable to decode ApplicationProfileId from:" ++ show f

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
                      
