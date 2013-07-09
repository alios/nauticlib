{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

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

module Data.S57 (
  DataFileS57 (..),
  s57dataFile,
  -- * Record types
  -- ** Data set descriptive records
  -- *** Data set general information record (DSID)
  DSID (..),
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
  -- *** Data dictionary definition (DDDF)
  DDDF (..),
  DDDR (..),
  -- *** Data dictionary domain (DDDI)
  DDDI (..),
  DDOM (..),
  DDRF (..),
  -- *** Data dictionary schema (DDSI)
  DDSI (..),
  DDSC (..),

  -- ** Feature records
  -- ** Spatial records
  -- *** Vector record (VRID)
  VRID (..),
  ATTV (..),
  VRPC (..),
  VRPT (..),
  SGCC (..),
  SG2D (..),
  SG3D (..),
  
  -- * Other data types
  PRSP (..),
  PROF (..),
  EXPP (..),
  DSTR (..),
  VRID_RCNM (..),
  RUIN (..),
  Orientation,
  UsageIndicator,
  TopologyIndicator,
  MaskingIndicator
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
import Data.ByteString (ByteString)
type AGEN = Integer

-- | a S-57 Datafile
data DataFileS57 = DataFileS57 {
      df_dsid :: !(Maybe DSID),
      df_dspm :: !(Maybe DSPM),
      df_vrids :: !([VRID])
} deriving (Eq, Show)

-- | Data set Identification field structure 'DSID'
data DSID = DSID {
      dsid_rcnm :: !Word8, -- ^ Record name
      dsid_rcid :: !Word32, -- ^ Record identification number
      dsid_expp :: !EXPP, -- ^ Exchange purpose
      dsid_intu :: !Integer, -- ^ Intended usage. A numeric value indicating the inteded usage for wich the data has been compiled.
      dsid_dsnm :: !String, -- ^ Data set name
      dsid_edtn :: !String, -- ^ Edition number
      dsid_updn :: !String, -- ^ Update number
      dsid_uadt :: !(Maybe Day), -- ^ Update application date 
      dsid_isdt :: !Day, -- ^ Issue date
      dsid_sted :: !Double, -- ^ Edition number of S-57 (3.0, 3.1)
      dsid_prsp :: !PRSP, -- ^ Product specification
      dsid_psdn :: !String, -- ^ Product specification description. A string identifying a non standard product specification
      dsid_pred :: !String, -- ^ Product specification edition number
      dsid_prof :: !PROF, -- ^ Application profile identification
      dsid_agen :: !AGEN, -- ^ Producing agency
      dsid_comt :: !String, -- ^ Comment
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
      dspm_rcnm :: !Word8, -- ^ Record name
      dspm_rcid :: !Word32,  -- ^Record Identification number
      dspm_hdat :: !Word8, -- ^ Horizontal geodetic datum (HORDAT)
      dspm_vdat :: !Word8, -- ^ Vertical geodetic datum (VERDAT)
      dspm_sdat :: !Word8, -- ^ Sounding geodetic datum (VERDAT)
      dspm_cscl :: !Integer, -- ^ Compilation scale of data
      dspm_duni :: !Word8,  -- ^ Units of depth measurement
      dspm_huni :: !Word8, -- ^ Units of height measurement
      dspm_puni :: !Word8, -- ^ Units of postitional accuracy
      dspm_coun :: !COUN, -- ^ Coordinate Units
      dspm_comf :: !Integer, -- ^ Coordinate muliplication factor
      dspm_somf :: !Integer, -- ^ 3-D (sounding) multiplication factor
      dspm_comt :: !String, -- ^ Comment
      dspm_dspr :: !(Maybe DSPR), -- ^ Data set projection
      dspm_dsrc :: !([DSRC]) -- ^ Data set registration control fields
} deriving (Eq, Show)

-- | Data set projection field (DSPR)
data DSPR = DSPR {
      dspr_proj :: !Word8, -- ^ Projection
      dspr_prp1 :: !Integer, -- ^ Projection Parameter 1
      dspr_prp2 :: !Integer, -- ^ Projection Parameter 2
      dspr_prp3 :: !Integer, -- ^ Projection Parameter 3
      dspr_prp4 :: !Integer, -- ^ Projection Parameter 4
      dspr_feas :: !Double,  -- ^ False Easting
      dspr_fnor :: !Double, -- ^ False Northing
      dspr_fpmf :: !Integer, -- ^ Floating Point mulitiplication factor
      dspr_comt :: !String
} deriving (Eq, Show)


-- | Data set registration conrol (DSRC)
data DSRC = DSRC {
      dsrc_rpid :: !Word8, -- ^ Registration point ID
      dsrc_ryco :: !Double, -- ^ Registration point Latitude or Northing
      dsrc_rxco :: !Double, -- ^ Registration point Longitude or Easting
      dsrc_curp :: !COUN, -- ^ Coordinate units for registration point
      dsrc_fpmf :: !Integer, -- ^ Floating point mulitplication factor
      dsrc_rxvl :: !Double, -- ^ Registration point X-value
      dsrc_ryvl :: !Double, -- ^ Registration point Y-value
      dsrc_comt :: !String -- ^ Comment
} deriving (Eq,Show)



-- | Data set history recrord (DSHT)
data DSHT = DSHT

-- | Data set accuracy record (DSAC)
data DSAC = DSAC


-- | Catalogue directory record (CATD)
data CATD = CATD
-- | Catalogue cross refernce record (CATX)
data CATX = CATX

data DDDF = DDDF

data DDDR = DDDR

data DDDI = DDDI

data DDOM = DDOM

data DDRF = DDRF

data DDSI = DDSI

data DDSC = DDSC



-- | Vector record
data VRID = VRID {
      vrid_rcnm :: !VRID_RCNM, -- ^ Record name
      vrid_rcid :: !Word32, -- ^ Record identification number 
      vrid_rver :: !Integer, -- ^ Record version
      vrid_ruin :: !RUIN, -- ^ Record update instruction
      vrid_attvs :: !([ATTV]), -- ^ Attribute Fields
      vrid_vrpc :: !(Maybe VRPC), -- ^ Pointer Control Field
      vrid_vrpt :: !([VRPT]), -- ^ Pointer Fields
      vrid_sgcc :: !(Maybe SGCC), -- ^ Coordinate Control Field
      vrid_sg2ds :: !([SG2D]), -- ^ 2-D coodrinate fields
      vrid_sg3ds :: !([SG3D])  -- ^ 3-D coodrinate (Sounding Array) fields
} deriving (Eq, Show)

-- | Vector record attribute
data ATTV = ATTV {
      attv_attl :: !Integer, -- ^ Attribute label/code
      attv_atvl :: !String -- ^ Attribute value
    } deriving (Eq, Show)

-- | Vector record pointer control
data VRPC = VRPC {
      vrpc_vpui :: !RUIN, -- ^ Vector record pointer update instruction
      vrpc_vpix :: !Integer, -- ^ Vector record pointer index
      vrpc_nvpt :: !Integer -- ^ Number of record pointers
    } deriving (Eq, Show)


-- | Vector record pointer
data VRPT = VRPT {
      vrpt_name :: !ByteString, -- ^ Name
      vrpt_ornt :: !(Maybe Orientation), -- ^ Orientation
      vrpt_usag :: !(Maybe UsageIndicator), -- ^ Usage indicator
      vrpt_topi :: !(Maybe TopologyIndicator), -- ^ Topology indicator
      vrpt_mask :: !(Maybe MaskingIndicator)  -- ^ Masking indicator
    } deriving (Eq, Show)


-- | Coordinate control field
data SGCC = SGCC {
      sgcc_ccui :: !RUIN, -- ^ Coordinate update instruction
      sgcc_ccix :: !Integer, -- ^ Coordinate index
      sgcc_ccnc :: !Integer -- ^ Number of coordinates
} deriving (Eq, Show)

-- | 2-D coodrinate fields
data SG2D = SG2D {
      sg2d_ycoo :: !Double, -- ^ Coordinat in Y axis
      sg2d_xcoo :: !Double  -- ^ Coordinat in X axis
} deriving (Eq, Show)

-- | 3-D coodrinate (Sounding Array) fields
data SG3D = SG3D {
      sg3d_ycoo :: !Double, -- ^ Coordinat in Y axis
      sg3d_xcoo :: !Double, -- ^ Coordinat in X axis
      sg3d_ve3d :: !Double -- ^ 3-D (sounding) value
} deriving (Eq, Show)



--
-- exported functions
--
s57dataFile :: DataFile -> DataFileS57
s57dataFile f =
    let dspm' = dspm f
        vrids' =
            case dspm' of
              Nothing -> []
              Just dspm'' -> vrids dspm'' f
    in DataFileS57 {
             df_dsid = dsid f,
             df_dspm = dspm',
             df_vrids = vrids'
           }

-- | get the 'DSID' from a ISO-8211 'DataFile' 
dsid :: DataFile -> Maybe DSID
dsid df = 
 case (findRecordByTag' "DSID" df) of
   Nothing -> Nothing
   Just dr -> Just DSID {
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
          dsid_dssi = dssi dr
        }
    
dssi :: DataRecord -> DSSI
dssi r =  
    let dr = findSubRecord "DSSI" r
    in DSSI {
                  dssi_dstr = sdRecordField dr "DSTR",
                  dssi_aall = sdRecordField dr "AALL",
                  dssi_nall = sdRecordField dr "NALL",
                  dssi_nomr = sdRecordField dr "NOMR",
                  dssi_nocr = sdRecordField dr "NOCR",
                  dssi_nogr = sdRecordField dr "NOGR",
                  dssi_nolr = sdRecordField dr "NOLR",
                  dssi_noin = sdRecordField dr "NOIN",
                  dssi_nocn = sdRecordField dr "NOCN",
                  dssi_noed = sdRecordField dr "NOED",
                  dssi_nofa = sdRecordField dr "NOFA"
           }

dspm :: DataFile -> Maybe DSPM
dspm df = 
 case (findRecordByTag' "DSPM" df) of
   Nothing -> Nothing
   Just dr -> Just  DSPM {
      dspm_rcnm = sdRecordField dr "RCNM",
      dspm_rcid = sdRecordField dr "RCID",
      dspm_hdat = sdRecordField dr "HDAT",
      dspm_vdat = sdRecordField dr "VDAT",
      dspm_sdat = sdRecordField dr "SDAT",
      dspm_cscl = sdRecordField dr "CSCL",
      dspm_duni = sdRecordField dr "DUNI",
      dspm_huni = sdRecordField dr "HUNI",
      dspm_puni = sdRecordField dr "PUNI",
      dspm_coun = sdRecordField dr "COUN",
      dspm_comf = sdRecordField dr "COMF",
      dspm_somf = sdRecordField dr "SOMF",
      dspm_comt = sdRecordField dr "COMT",
      dspm_dspr = dspr dr,
      dspm_dsrc = dsrcs dr
}




dspr :: DataRecord -> Maybe DSPR
dspr r = case (findSubRecord' "DSPR" r) of
           Nothing -> Nothing
           Just dr -> Just DSPR {
             dspr_proj = sdRecordField dr "PROJ", 
             dspr_prp1 = sdRecordField dr "PRP1",
             dspr_prp2 = sdRecordField dr "PRP2",
             dspr_prp3 = sdRecordField dr "PRP3",
             dspr_prp4 = sdRecordField dr "PRP4", 
             dspr_feas = sdRecordField dr "FEAS", 
             dspr_fnor = sdRecordField dr "FNOR", 
             dspr_fpmf = sdRecordField dr "FPMF",
             dspr_comt = sdRecordField dr "COMT"
           }



dsrcs :: DataRecord -> [DSRC]
dsrcs = map dsrc . findSubRecords "DSRC"

dsrc :: DataRecord -> DSRC
dsrc dr = 
    let fpmf,ryco,rxco :: Integer
        fpmf = sdRecordField dr "FPMF"
        ryco =  sdRecordField dr "RYCO"
        rxco = sdRecordField dr "RXCO"

    in DSRC {
            dsrc_rpid = sdRecordField dr "RPID",
            dsrc_ryco = (fromInteger ryco) / (fromInteger fpmf),
            dsrc_rxco = (fromInteger rxco) / (fromInteger fpmf),
            dsrc_curp = sdRecordField dr "CURP",
            dsrc_fpmf = fpmf,
            dsrc_rxvl = sdRecordField dr "RXVL",
            dsrc_ryvl = sdRecordField dr "RYVL", 
            dsrc_comt = sdRecordField dr "COMT"
          }

vrids :: DSPM -> DataFile -> [VRID]
vrids dspm' df = map (vrid dspm') $ findRecordsByTag "VRID" df

vrid    :: DSPM -> DataRecord -> VRID
vrid dspm' dr = VRID {
             vrid_rcnm  = sdRecordField dr "RCNM",
             vrid_rcid  = sdRecordField dr "RCID",
             vrid_rver  = sdRecordField dr "RVER",
             vrid_ruin  = sdRecordField dr "RUIN",
             vrid_attvs = attvs dr,
             vrid_vrpc  = vrpc dr,
             vrid_vrpt  = vrpts dr,
             vrid_sgcc  = sgcc dr,
             vrid_sg2ds = sg2ds dspm' dr,
             vrid_sg3ds = sg3ds dspm' dr
           }

vrpc :: DataRecord -> Maybe VRPC
vrpc r = 
    case (findSubRecord' "VRPC" r) of
      Nothing -> Nothing 
      Just dr -> Just VRPC {
                  vrpc_vpui = sdRecordField dr "VPUI",
                  vrpc_vpix = sdRecordField dr "VPIX",
                  vrpc_nvpt = sdRecordField dr "NVPT"
                }

sgcc :: DataRecord -> Maybe SGCC
sgcc r = 
    case (findSubRecord' "SGCC" r) of
      Nothing -> Nothing 
      Just dr -> Just SGCC {
                  sgcc_ccui = sdRecordField dr "VPUI",
                  sgcc_ccix = sdRecordField dr "VPIX",
                  sgcc_ccnc = sdRecordField dr "NVPT"
                }


vrpts :: DataRecord -> [VRPT]
vrpts = maybemdRecords "VRPT" vrpt

vrpt :: Map.Map String DataFieldT -> VRPT
vrpt m = VRPT {
         vrpt_name = mdRecordField "NAME" m,
         vrpt_ornt = mdRecordField "ORNT" m,
         vrpt_usag = mdRecordField "USAG" m,
         vrpt_topi = mdRecordField "TOPI" m,
         vrpt_mask = mdRecordField "MASK" m
       }


attvs :: DataRecord -> [ATTV]
attvs = maybemdRecords "ATTV" attv

attv :: Map.Map String DataFieldT -> ATTV
attv m = ATTV {
         attv_attl = mdRecordField "ATTL" m,
         attv_atvl = mdRecordField "ATVL" m
       }

sg2ds :: DSPM -> DataRecord -> [SG2D]
sg2ds dspm' = maybemdRecords "SG2D" (sg2d dspm')

sg2d :: DSPM -> Map.Map String DataFieldT -> SG2D
sg2d dspm' m = 
  let x, y :: Integer
      x = mdRecordField "YCOO" m
      y = mdRecordField "XCOO" m
      mf = fromInteger $ dspm_comf dspm'
  in SG2D {
      sg2d_ycoo = (fromInteger y) / mf ,
      sg2d_xcoo = (fromInteger x) / mf
         }

sg3ds :: DSPM -> DataRecord -> [SG3D]
sg3ds dspm' = maybemdRecords "SG3D" (sg3d dspm')

sg3d :: DSPM -> Map.Map String DataFieldT -> SG3D
sg3d dspm' m = 
  let x, y, s :: Integer
      x = mdRecordField "YCOO" m
      y = mdRecordField "XCOO" m
      s = mdRecordField "VE3D" m
      comf = fromInteger $ dspm_comf dspm'
      somf = fromInteger $ dspm_somf dspm'
  in SG3D {
           sg3d_ycoo = (fromInteger y) / comf,
           sg3d_xcoo = (fromInteger x) / comf,
           sg3d_ve3d = (fromInteger s) / somf
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

maybemdRecords ::
  String -> (Map.Map String DataFieldT -> a) -> DataRecord -> [a]
maybemdRecords t c r = maybe [] (map c) $ mdRecords' t r



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

data COUN
    = LatitudeLongitude
    | EastingNorthing
    | UnitsOnTheChartMap
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


data Orientation
    = Forward 
    | Reverse 
    | OrientNULL
    deriving (Eq, Show)


data UsageIndicator
    = Exterior
    | Interior
    | ExteriorBoundaryTruncated
    | UsageNULL
    deriving (Eq, Show)


data TopologyIndicator
    = BeginningNode
    | EndNode
    | LeftFace
    | RightFace
    | ContainingFace
    deriving (Show, Eq)

data MaskingIndicator 
    = MaskMask 
    | MaskShow
    deriving (Show, Eq)


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

instance DataField Word8 where
    fromDataField (DFInteger i) = fromInteger i
    fromDataField v = error $ "invalid word8 " ++ show v

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


instance DataField COUN where
    fromDataField (DFString s) = 
        if (s == "LL") then LatitudeLongitude else
            if (s == "EN") then EastingNorthing else
                if (s == "UC") then UnitsOnTheChartMap else
                    error $ "invalid PROF: " ++ s
    fromDataField (DFInteger i) = 
        case i of
          1 -> LatitudeLongitude
          2 -> EastingNorthing
          3 -> UnitsOnTheChartMap
          i -> error $ "invalid COUN: " ++ show i
    fromDataField f = error $ "unable to decode COUN from:" ++ show f


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
          i -> error $ "invalid RUIN: " ++ show i
    fromDataField i = error $ "unable to decode RUIN from:" ++ show i
    

instance DataField (Maybe Orientation) where
    fromDataField (DFString s) = 
        if (s == "F") then Just Forward else
            if (s == "R") then Just Reverse else
                if (s == "N") then Nothing else
                    error $ "invalid Orientation: " ++ s
    fromDataField (DFInteger i) = 
        case i of
          1 -> Just Forward
          2 -> Just Reverse
          255 -> Nothing
          i -> error $ "invalid Orientation: " ++ show i
    fromDataField i = error $ "unable to decode Orientation from:" ++ show i

instance DataField (Maybe UsageIndicator) where
    fromDataField (DFString s) = 
        if (s == "E") then Just Exterior else
            if (s == "I") then Just Interior else
                if (s == "C") then Just ExteriorBoundaryTruncated else
                    if (s == "N") then Nothing else
                        error $ "invalid Usage Indicator: " ++ s
    fromDataField (DFInteger i) = 
        case i of
          1 -> Just Exterior
          2 -> Just Interior
          3 -> Just ExteriorBoundaryTruncated
          255 -> Nothing
          i -> error $ "invalid Usage Indicator: " ++ show i
    fromDataField i = error $ "unable to decode Usage Indicator from:" ++ show i


instance DataField (Maybe TopologyIndicator) where
    fromDataField (DFString s) = 
        if (s == "B") then Just BeginningNode else
            if (s == "E") then Just EndNode else
                if (s == "S") then Just LeftFace else
                    if (s == "D") then Just RightFace else
                        if (s == "F") then Just ContainingFace else
                            if (s == "N") then Nothing else
                                error $ "invalid Topology Indicator: " ++ s
    fromDataField (DFInteger i) = 
        case i of
          1 -> Just BeginningNode
          2 -> Just EndNode
          3 -> Just LeftFace
          4 -> Just RightFace
          5 -> Just ContainingFace
          255 -> Nothing
          i -> error $ "invalid Topology Indicator: " ++ show i
    fromDataField i = error $ "unable to decode Topology Indicator from:" ++ show i

instance DataField (Maybe MaskingIndicator) where
    fromDataField (DFString s) = 
        if (s == "M") then Just MaskMask else
            if (s == "S") then Just MaskShow else
                if (s == "N") then Nothing else
                    error $ "invalid Masking Indicator: " ++ s
    fromDataField (DFInteger i) = 
        case i of
          1 -> Just MaskMask
          2 -> Just MaskShow
          255 -> Nothing
          i -> error $ "invalid Masking Indicator: " ++ show i
    fromDataField i = error $ "unable to decode Masking Indicator from:" ++ show i
