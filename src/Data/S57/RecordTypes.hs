{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

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

module Data.S57.RecordTypes (
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
  -- *** Feature record (FRID)
  FRID (..),
  FOID (..),
  ATTF (..),
  NATF (..),
  FFPC (..),
  FFPT (..),
  FSPT (..),

  -- ** Spatial records
  -- *** Vector record (VRID)
  VRID (..),
  ATTV (..),
  VRPC (..),
  VRPT (..),
  SGCC (..),
  SG2D (..),
  SG3D (..),
  ARCC (..),

  -- * Other data types
  Name,
  PRSP (..),
  PROF (..),
  EXPP (..),
  DataStruct (..),
  VectorRecordIdentifier (..),
  RUIN (..),
  Orientation (..),
  UsageIndicator (..),
  TopologyIndicator (..),
  MaskingIndicator (..),
  ObjectOrAttribute (..),
  TypeOfObjectOrAttribute (..),
  ArcCurveType (..),
  ConstructionSurface (..),
) where


import           Data.Binary.Get
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Time.Calendar
import           Data.Time.Format
import           Data.Word
import           System.Locale

import           Data.ISO8211.Parser


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
      dsid_agen :: !Integer, -- ^ Producing agency
      dsid_comt :: !String, -- ^ Comment
      dsid_dssi :: !DSSI -- ^ Data set structure information field
    } deriving (Eq, Show)

-- | Data set structure information field 'DSSI'
data DSSI = DSSI {
      dssi_dstr :: !(Maybe DataStruct), -- ^ Data structure
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
      dspr_comt :: !String -- ^ Comment
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
data DSHT = DSHT {
      dsht_rcnm :: !String, -- ^ Record name
      dsht_rcid :: !Integer, -- ^ Record identification number
      dsht_prco :: !Integer, -- ^ Agency Code
      dsht_esdt :: !(Maybe Day), -- ^ Earliest source date
      dsht_lsdt :: !(Maybe Day), -- ^ Latest source date
      dsht_dcrt :: !String, -- ^ Data collection criteria
      dsht_codt :: !(Maybe Day), -- ^ Compilation Date
      dsht_comt :: !String -- ^ Comment
    } deriving (Eq, Show)

-- | Data set accuracy record (DSAC)
data DSAC = DSAC {
      dsac_rcnm :: !String, -- ^ Record name
      dsac_rcid :: !Integer, -- ^ Record identification number
      dsac_pacc :: !Double, -- ^ Absolute positional accuracy
      dsac_hacc :: !Double, -- ^ Absolute horizontal/vertical measurement accuracy
      dsac_sacc :: !Double, -- ^ Absolute sounding accuracy
      dsac_fpmf :: !Integer, -- ^ Floating point multiplication factor
      dsac_comt :: !String -- ^ Comment
    } deriving (Eq, Show)

-- | Catalogue directory record (CATD)
data CATD = CATD {
      catd_rcnm  :: !String, -- ^  Record name
      catd_rcid  :: !Integer, -- ^ Record identification number
      catd_file  :: !String, -- ^  File name
      catd_lfil  :: !String, -- ^  File long name
      catd_volm  :: !String, -- ^  Volume
      catd_impl  :: !String, -- ^  Implementation
      catd_slat  :: !Double, -- ^  Souternmost Latitude
      catd_wlon  :: !Double, -- ^  Westernmost Longitude
      catd_nlat  :: !Double, -- ^  Nothermost Latitude
      catd_elon  :: !Double, -- ^  Easternmost Longitude
      catd_crcs  :: !String, -- ^  CRC
      catd_comt  :: !String, -- ^  Comment
      catd_catxs :: [CATX]
} deriving (Eq, Show)

-- | Catalogue cross refernce record (CATX)
data CATX = CATX  {
      catx_rcnm :: !String, -- ^ Record name
      catx_rcid :: !Integer, -- ^ Record identification Number
      catx_nam1 :: !Name, -- ^ Name 1
      catx_nam2 :: !Name, -- ^ Name 2
      catx_comt :: !String -- ^ Comment
} deriving (Eq, Show)


data DDDF = DDDF {
      dddf_rcnm :: !String, -- ^  Record name
      dddf_rcid :: !Integer, -- ^ Record identification number
      dddf_oora :: !ObjectOrAttribute, -- ^ Object or attribute
      dddf_oaac :: !String, -- ^ Object or attribute acronym
      dddf_oaco :: !Integer, -- ^ Object or attribute label/code
      dddf_oall :: !String, -- ^ Object or attribute label
      dddf_oaty :: !TypeOfObjectOrAttribute, -- ^ Type of object or attribute
      dddf_defn :: !String, -- ^ Definition
      dddf_auth :: !Integer, -- ^ Autorizing agency
      dddf_comt :: !String -- ^ Comment
} deriving (Eq, Show)


data DDDR = DDDR {
} deriving (Eq, Show)


data DDDI = DDDI {
      dddi_rcnm :: !String, -- ^  Record name
      dddi_rcid :: !Integer, -- ^ Record identification number
      dddi_atlb :: !Integer, -- ^ Attribute label/code
      dddi_atdo :: !AttributeDomainCode, -- ^ Attribute domain code
      dddi_admu :: !String, -- ^ Attribute domain value measurement unit
      dddi_adft :: !String, -- ^ Attribute domain format
      dddi_auth :: !Integer, -- ^ Autorizing agency
      dddi_comt :: !String -- ^ Comment
} deriving (Eq, Show)


data DDOM = DDOM {
} deriving (Eq, Show)


data DDRF = DDRF {
} deriving (Eq, Show)


data DDSI = DDSI {
      ddsi_rcnm :: !String, -- ^  Record name
      ddsi_rcid :: !Integer, -- ^ Record identification number
      ddsi_oblb :: !Integer -- ^ Object label/code
 } deriving (Eq, Show)

data DDSC = DDSC
 {
} deriving (Eq, Show)


-- | Featrure record
data FRID = FRID {
      frid_rcnm  :: !Integer, -- ^ Record name
      frid_rcid  :: !Integer, -- ^ Record identification number
      frid_prim  :: !(Maybe GeoPrimitive), -- ^ Object geometric primitive
      frid_grup  :: !Integer, -- ^ Group
      frid_objl  :: !Integer, -- ^ Object Label/Code
      frid_rver  :: !Integer, -- ^ Record version
      frid_ruin  :: !RUIN, -- ^ Record update instruction
      frid_foid  :: !FOID, -- ^ Feature object identifier
      frid_attfs :: !([ATTF]), -- ^ Feature attributes
      frid_natfs :: !([NATF]), -- ^ Feature national attributes
      frid_ffpc  :: !(Maybe FFPC), -- ^ Feature record to feature object pointer
      frid_ffpts :: !([FFPT]), -- ^ Feature Record to Feature Object Pointer
      frid_fspts :: !([FSPT]) -- ^ Feature Record to Spatial Record Pointer
} deriving (Eq, Show)


-- | Feature object identifier
data FOID = FOID {
      foid_agen :: !Integer, -- ^ Producing agency
      foid_fidn :: !Integer, -- ^ Feature identification number
      foid_fids :: !Integer -- ^ Feature idendification subdivision
} deriving (Eq, Show)


-- | Feature attribute
data ATTF = ATTF {
      attf_attl :: !Integer, -- ^ Attribute label/code
      attf_atvl :: !String -- ^ Attribute value
} deriving (Eq, Show)

-- | Feature national attribute
data NATF = NATF {
      natf_attl :: !Integer, -- ^ Attribute label/code
      natf_atvl :: !String -- ^ Attribute value
} deriving (Eq, Show)


-- | Feature record to feature object pointer
data FFPC = FFPC {
      ffpc_ffui :: !RUIN, -- ^ Feature object point update instruction
      ffpc_ffix :: !Integer, -- ^ Feature object pointer index
      ffpc_nfpt :: !Integer -- ^ Number of feature object pointers
} deriving (Eq, Show)


-- | Feature record to feature object pointer
data FFPT = FFPT {
      ffpt_lnam :: !LongName, -- ^ Long Name
      ffpt_rind :: !RelationShipIndicator, -- ^ Relationship indicator
      ffpt_comt :: !String -- ^ Comment
} deriving (Eq, Show)

-- | Feature record to spatial record pointer
data FSPT = FSPT {
      fspt_name :: !Name, -- ^ Long Name
      fspt_ornt :: !(Maybe Orientation), -- ^ Relationship indicator
      fspt_usag :: !(Maybe UsageIndicator), -- ^ Comment,
      fspt_mask :: !(Maybe MaskingIndicator) -- ^ Masking indicator
} deriving (Eq, Show)


-- | Vector record
data VRID = VRID {
      vrid_rcnm  :: !VectorRecordIdentifier, -- ^ Record name
      vrid_rcid  :: !Word32, -- ^ Record identification number
      vrid_rver  :: !Integer, -- ^ Record version
      vrid_ruin  :: !RUIN, -- ^ Record update instruction
      vrid_attvs :: !([ATTV]), -- ^ Attribute Fields
      vrid_vrpc  :: !(Maybe VRPC), -- ^ Pointer Control Field
      vrid_vrpt  :: !([VRPT]), -- ^ Pointer Fields
      vrid_sgcc  :: !(Maybe SGCC), -- ^ Coordinate Control Field
      vrid_sg2ds :: !([SG2D]), -- ^ 2-D coodrinate fields
      vrid_sg3ds :: !([SG3D]),  -- ^ 3-D coodrinate (Sounding Array) fields
      vrid_arccs :: !([ARCC])  -- ^ Arc/Curve definition fields
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
      vrpt_name :: !Name, -- ^ Name
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

-- | Arc/Curve definition
data ARCC = ARCC {
      arcc_atyp :: !ArcCurveType, -- ^ Arc/Curve type
      arcc_surf :: !ConstructionSurface, -- ^ Construction Surface
      arcc_ordr :: !Integer, -- ^ Curve order
      arcc_reso :: !Double, -- ^ Interpolated point resolution
      arcc_fpmf :: !Integer -- ^ Floating point multiplication factor
} deriving (Eq, Show)


type Name = (Word8, Word32)
type LongName = FOID

--
-- data types
--

data EXPP
    = DataSetIsNew
    | DataSetIsRevision
    deriving (Eq, Show)

data DataStruct
    = CartographicSpaghetti
    | ChainMode
    | PlanarGraph
    | FullTopology
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

-- | The vector record identifier field hold the record identifier (key) for that vector record.
--   It is also used to diffentiate between the various types of vector records.
data VectorRecordIdentifier
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


-- | The direction in which an edge is to be interpreted for a particular area is indicated in the
--   'Orientation' subfield.
data Orientation
    = Forward
    | Reverse
    | OrientNULL
    deriving (Eq, Show)

-- | The In the case of areas consisting of one outer boundary and one or more non-intersecting
--   inner boundaries (areas with holes), the 'UsageIndicator' subfield is used to distinguish
--   between interior and exterior boundaries. The subfield is also used to indicate that an
--   exterior boundary is part of the data limit.
data UsageIndicator
    = Exterior
    | Interior
    | ExteriorBoundaryTruncated
    | UsageNULL
    deriving (Eq, Show)

-- | Topology Indicator
data TopologyIndicator
    = BeginningNode
    | EndNode
    | LeftFace
    | RightFace
    | ContainingFace
    deriving (Show, Eq)

-- | Under certain circumstances it may be necessary to suppress the symbolization of one or more edges which define the inner or outer boundary of an area. Suppression off the symbolization can be controlled by using the 'MaskingIndicator'.
data MaskingIndicator
    = MaskMask
    | MaskShow
    deriving (Show, Eq)

data GeoPrimitive
    = Point
    | Line
    | Area
    deriving (Eq, Show)


data ObjectOrAttribute
    = IsObject
    | IsAttribute
    deriving (Show, Eq)

data TypeOfObjectOrAttribute
    = IsMetaObject
    | IsCartographicObject
    | IsGeoObject
    | IsCollectionObject
    | IsFeatureAttribute
    | IsFeatureNationalAttribute
    | IsSpatialAttribute
    deriving (Show, Eq)

data AttributeDomainCode
    = ADCEnumerated
    | ADCListOfEnumeratedValues
    | ADCFloat
    | ADCInteger
    | ADCCodeString
    | ADCFreeTextFormat
    deriving (Show,Eq)

data RelationShipIndicator
    = Master
    | Slave
    | Peer
    | ProductSpecInd (Either Word8 String)
    deriving (Eq, Show)

data ArcCurveType
    = Arc3PointCentre
    | EllipticalArc
    | UniformBspline
    | PiecewiseBezier
    | NonUniformRationalBspline
    deriving (Eq, Show)

data ConstructionSurface
    = Ellipsoidal
    | Planar
    deriving (Eq, Show)

--
-- instance declarations
--

instance DataField Word32 where
    fromDataField (DFInteger i) = fromInteger i
    fromDataField v = error $ "invalid word32 " ++ show v

instance DataField Word8 where
    fromDataField (DFInteger i) = fromInteger i
    fromDataField v = error $ "invalid word8 " ++ show v

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
          j -> error $ "invalid LexicalLevel: " ++ show j
    fromDataField i = error $ "unable to decode lexical level from:" ++ show i

instance DataField Name where
    fromDataField (DFByteString bs) =
        let (a,b) = BS.splitAt 1 bs
        in (BS.head a, runGet getWord32le $ BL.fromChunks [b])
    fromDataField i = error $ "unable to decode Name Field from: " ++ show i


instance DataField FOID where
    fromDataField (DFByteString bs) =
        let getter = do
              agen <- fmap toInteger getWord16le
              fidn <- fmap toInteger getWord32le
              fids <- fmap toInteger getWord16le
              return $ FOID {
                           foid_agen = agen,
                           foid_fidn = fidn,
                           foid_fids = fids
                         }
        in runGet getter $ BL.fromChunks [bs]

    fromDataField i = error $ "unable to decode Name Field from: " ++ show i

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

instance DataField (Maybe GeoPrimitive) where
    fromDataField (DFString s) =
        if (s == "P") then Just Point else
            if (s == "L") then Just Line else
                if (s == "A") then Just Area else
                    if (s == "N") then Nothing else
                        error $ "invalid GeoPrimitive: " ++ s
    fromDataField (DFInteger i) =
        case i of
          1 -> Just Point
          2 -> Just Line
          3 -> Just Area
          255 -> Nothing
          j -> error $ "invalid GeoPrimitive: " ++ show j
    fromDataField f = error $ "unable to decode DataStruct from:" ++ show f

instance DataField (Maybe DataStruct) where
    fromDataField (DFString s) =
        if (s == "CS") then Just CartographicSpaghetti else
            if (s == "CN") then Just ChainMode else
                if (s == "PG") then Just PlanarGraph else
                    if (s == "FT") then Just FullTopology else
                        if (s == "NO") then Nothing else
                            error $ "invalid Orientation: " ++ s
    fromDataField (DFInteger i) =
        case i of
          1 -> Just CartographicSpaghetti
          2 -> Just ChainMode
          3 -> Just PlanarGraph
          4 -> Just FullTopology
          255 -> Nothing
          j -> error $ "invalid DataStruct: " ++ show j
    fromDataField f = error $ "unable to decode DataStruct from:" ++ show f



instance DataField PRSP where
    fromDataField (DFString s) =
        if (s == "ENC") then ElectronicNavigationalChart else
            if (s == "ODD") then IHOObjectCatalogueDataDictionary else
                error $ "invalid ProductSpec: " ++ s
    fromDataField (DFInteger i) =
        case i of
          1 -> ElectronicNavigationalChart
          2 -> IHOObjectCatalogueDataDictionary
          j -> error $ "invalid ProductSpec: " ++ show j
    fromDataField f = error $ "unable to decode ProductSpec from:" ++ show f


instance DataField VectorRecordIdentifier where
    fromDataField (DFString s) =
        if (s == "VI") then IsolatedNode else
            if (s == "VC") then ConnectedNode else
                if (s == "VE") then Edge else
                    if (s == "VF") then Face else
                        error $ "invalid VectorRecordIdentifier: " ++ s
    fromDataField (DFInteger i) =
        case i of
          110 -> IsolatedNode
          120 -> ConnectedNode
          130 -> Edge
          140 -> Face
          j -> error $ "invalid VectorRecordIdentifier: " ++ show j
    fromDataField f = error $ "unable to decode VectorRecordIdentifier from:" ++ show f

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
          j -> error $ "invalid PROF: " ++ show j
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
          j -> error $ "invalid COUN: " ++ show j
    fromDataField f = error $ "unable to decode COUN from:" ++ show f



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
          j -> error $ "invalid RUIN: " ++ show j
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
          j -> error $ "invalid Orientation: " ++ show j
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
          j -> error $ "invalid Usage Indicator: " ++ show j
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
          j -> error $ "invalid Topology Indicator: " ++ show j
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
          j -> error $ "invalid Masking Indicator: " ++ show j
    fromDataField i = error $ "unable to decode Masking Indicator from:" ++ show i


instance DataField ObjectOrAttribute where
    fromDataField (DFString s) =
        if (s == "A") then IsAttribute else
            if (s == "O") then IsObject else
                error $ "invalid ObjectOrAttribute: " ++ show s
    fromDataField (DFInteger i) =
        if (i == 1) then IsAttribute else
            if (i == 2) then IsObject else
                error $ "invalid ObjectOrAttribute: " ++ show i
    fromDataField f = error $ "unable to decode ObjectOrAttribute from:" ++ show f

instance DataField TypeOfObjectOrAttribute where
    fromDataField (DFString s) =
        if (s == "M") then IsMetaObject else
        if (s == "$") then IsCartographicObject else
        if (s == "G") then IsGeoObject else
        if (s == "C") then IsCollectionObject else
        if (s == "F") then IsFeatureAttribute else
        if (s == "N") then IsFeatureNationalAttribute else
        if (s == "S") then IsSpatialAttribute else
                error $ "invalid TypeOfObjectOrAttribute: " ++ show s
    fromDataField (DFInteger i) =
        if (i == 1) then IsMetaObject else
        if (i == 2) then IsCartographicObject else
        if (i == 3) then IsGeoObject else
        if (i == 4) then IsCollectionObject else
        if (i == 5) then IsFeatureAttribute else
        if (i == 6) then IsFeatureNationalAttribute else
        if (i == 7) then IsSpatialAttribute else
                error $ "invalid ObjectOrAttribute: " ++ show i
    fromDataField f = error $ "unable to decode ObjectOrAttribute from:" ++ show f


instance DataField AttributeDomainCode where
    fromDataField (DFString s) =
        if (s == "E") then ADCEnumerated else
        if (s == "L") then ADCListOfEnumeratedValues else
        if (s == "F") then ADCFloat else
        if (s == "I") then ADCInteger else
        if (s == "A") then ADCCodeString else
        if (s == "S") then ADCFreeTextFormat else
                error $ "invalid AttributeDomainCode: " ++ show s
    fromDataField (DFInteger i) =
        if (i == 1) then ADCEnumerated else
        if (i == 2) then ADCListOfEnumeratedValues else
        if (i == 3) then ADCFloat else
        if (i == 4) then ADCInteger else
        if (i == 5) then ADCCodeString else
        if (i == 6) then ADCFreeTextFormat else
                error $ "invalid AttributeDomainCode: " ++ show i
    fromDataField f = error $ "unable to decode AttributeDomainCode from:" ++ show f


instance DataField RelationShipIndicator where
    fromDataField (DFString s) =
        if (s == "M") then Master else
        if (s == "S") then Slave else
        if (s == "P") then Peer else
            ProductSpecInd $ Right s
    fromDataField (DFInteger i) =
        if (i == 1) then Master else
        if (i == 2) then Slave else
        if (i == 3) then Peer else
            ProductSpecInd . Left . fromInteger $ i
    fromDataField f = error $ "unable to decode ObjectOrAttribute from:" ++ show f



instance DataField ArcCurveType where
    fromDataField (DFString s) =
        if (s == "C") then Arc3PointCentre else
        if (s == "E") then EllipticalArc else
        if (s == "U") then UniformBspline else
        if (s == "B") then PiecewiseBezier else
        if (s == "N") then NonUniformRationalBspline else
                error $ "invalid ArcCurveType: " ++ show s
    fromDataField (DFInteger i) =
        if (i == 1) then Arc3PointCentre  else
        if (i == 2) then EllipticalArc else
        if (i == 3) then UniformBspline else
        if (i == 4) then PiecewiseBezier else
        if (i == 5) then NonUniformRationalBspline else
                error $ "invalid ArcCurveType: " ++ show i
    fromDataField f = error $ "unable to decode ArcCurveType from:" ++ show f


instance DataField ConstructionSurface where
    fromDataField (DFString s) =
        if (s == "E") then Ellipsoidal else
        if (s == "P") then Planar else
                error $ "invalid ArcCurveType: " ++ show s
    fromDataField (DFInteger i) =
        if (i == 1) then Ellipsoidal  else
        if (i == 2) then Planar else
                error $ "invalid ArcCurveType: " ++ show i
    fromDataField f = error $ "unable to decode ArcCurveType from:" ++ show f

