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

module Data.S57.Attributes
    ( Attribute(..)
    , AttributeT(..)
    , AttributeValue(..)
    ) where

import           Data.List.Split
import           Data.Map        (Map)
import qualified Data.Map        as M
import           Data.Maybe
import           Data.Set        (Set)
import qualified Data.Set        as S

data AttributeType
    = AttEnumerated
    | AttList
    | AttFloat
    | AttInteger
    | AttCodedString
    | AttFreeText
    deriving (Eq, Show)

data AttributeClass = AttributeClass Char
                      deriving (Eq, Show)
data AttributeValue
    = UnparsedValue AttributeType AttributeClass String
    | EnumValue (Maybe (Integer, String))
    | ListValue [AttributeValue]
    | FloatValue Double
    | IntegerValue Integer
    | CodedStringValue String
    | FreeTextValue String
    deriving (Eq, Show)

class (Enum a, Show a) => Attribute a where
    att_code :: a -> Integer
    att_code = toInteger . fromEnum
    att_desc :: a -> String
    att_acronym :: a -> String
    att_acronym = show
    att_type' :: a -> Char
    att_type :: a -> AttributeType
    att_ValueP :: a -> String -> AttributeValue
    att_ValueP a i =
        let c = att_class a
            t = att_type a
            upv = UnparsedValue t c i
        in case c of
             AttributeClass 'F' ->
                 case t of
                   AttEnumerated -> att_EnumV a i
                   AttList       -> ListValue $ map (att_EnumV a) $ splitOn "," i
                   AttCodedString -> CodedStringValue i
                   AttFreeText    -> FreeTextValue i
                   AttInteger     -> maybe (error $ "unable to parse IntegerValue from: " ++ i)
                                    IntegerValue $ readMaybeI i
                   AttFloat       -> maybe (error $ "unable to parse IntegerValue from: " ++ i)
                                    FloatValue $ readMaybeF i
             _ -> upv
    att_EnumV :: a -> String -> AttributeValue
    att_EnumV a i =
        let m = M.lookup (att_code a) enumValues
        in EnumValue $ case (readMaybe i) of
             Nothing -> Nothing
             Just k -> maybe Nothing (\v -> Just (k, v))
                   (maybe Nothing (M.lookup k) m)
    att_type a =
        case (att_type' a) of
          'E' -> AttEnumerated
          'L' -> AttList
          'F' -> AttFloat
          'I' -> AttInteger
          'A' -> AttCodedString
          'S' -> AttFreeText
    att_class' :: a -> Char
    att_class :: a -> AttributeClass
    att_class a = AttributeClass $ att_class' a


readMaybeI :: String -> Maybe Integer
readMaybeI i = readMaybe $ "0" ++ i

readMaybeF :: String -> Maybe Double
readMaybeF i = readMaybe $ "0" ++ i

readMaybe :: Read a => String -> Maybe a
readMaybe = fmap fst . listToMaybe . reads

instance Attribute AttributeT where
    att_desc AGENCY = "Agency responsible for production"
    att_desc BCNSHP = "Beacon shape"
    att_desc BUISHP = "Building shape"
    att_desc BOYSHP = "Buoy shape"
    att_desc BURDEP = "Buried depth"
    att_desc CALSGN = "Call sign"
    att_desc CATAIR = "Category of airport/airfield"
    att_desc CATACH = "Category of anchorage"
    att_desc CATBRG = "Category of bridge"
    att_desc CATBUA = "Category of built-up area"
    att_desc CATCBL = "Category of cable"
    att_desc CATCAN = "Category of canal"
    att_desc CATCAM = "Category of cardinal mark"
    att_desc CATCHP = "Category of checkpoint"
    att_desc CATCOA = "Category of coastline"
    att_desc CATCTR = "Category of control point"
    att_desc CATCON = "Category of conveyor"
    att_desc CATCOV = "Category of coverage"
    att_desc CATCRN = "Category of crane"
    att_desc CATDAM = "Category of dam"
    att_desc CATDIS = "Category of distance mark"
    att_desc CATDOC = "Category of dock"
    att_desc CATDPG = "Category of dumping ground"
    att_desc CATFNC = "Category of  fence/wall"
    att_desc CATFRY = "Category of ferry"
    att_desc CATFIF = "Category of  fishing  facility"
    att_desc CATFOG = "Category of  fog signal"
    att_desc CATFOR = "Category of  fortified structure"
    att_desc CATGAT = "Category of gate"
    att_desc CATHAF = "Category of harbour facility"
    att_desc CATHLK = "Category of hulk"
    att_desc CATICE = "Category of  ice"
    att_desc CATINB = "Category of installation buoy"
    att_desc CATLND = "Category of land region"
    att_desc CATLMK = "Category of landmark"
    att_desc CATLAM = "Category of lateral mark"
    att_desc CATLIT = "Category of light"
    att_desc CATMFA = "Category of marine farm/culture"
    att_desc CATMPA = "Category of military practice area"
    att_desc CATMOR = "Category of mooring/warping facility"
    att_desc CATNAV = "Category of navigation line"
    att_desc CATOBS = "Category of obstruction"
    att_desc CATOFP = "Category of offshore platform"
    att_desc CATOLB = "Category of oil barrier"
    att_desc CATPLE = "Category of pile"
    att_desc CATPIL = "Category of pilot boarding place"
    att_desc CATPIP = "Category of pipeline / pipe"
    att_desc CATPRA = "Category of production area"
    att_desc CATPYL = "Category of pylon"
    att_desc CATQUA = "Category of quality of data"
    att_desc CATRAS = "Category of radar station"
    att_desc CATRTB = "Category of radar transponder beacon"
    att_desc CATROS = "Category of radio station"
    att_desc CATTRK = "Category of recommended track"
    att_desc CATRSC = "Category of rescue station"
    att_desc CATREA = "Category of restricted area"
    att_desc CATROD = "Category of road"
    att_desc CATRUN = "Category of runway"
    att_desc CATSEA = "Category of sea area"
    att_desc CATSLC = "Category of shoreline construction"
    att_desc CATSIT = "Category of signal station / traffic"
    att_desc CATSIW = "Category of signal station / warning"
    att_desc CATSIL = "Category of silo/tank"
    att_desc CATSLO = "Category of slope"
    att_desc CATSCF = "Category of small craft facility"
    att_desc CATSPM = "Category of special purpose mark"
    att_desc CATTSS = "Category of Traffic Separation Scheme"
    att_desc CATVEG = "Category of vegetation"
    att_desc CATWAT = "Category of water turbulence"
    att_desc CATWED = "Category of weed/kelp"
    att_desc CATWRK = "Category of wreck"
    att_desc CATZOC = "Category of zone of confidence data"
    att_desc SPACE = "Character spacing"
    att_desc CHARS = "Character specification"
    att_desc COLOUR = "Colour"
    att_desc COLPAT = "Colour pattern"
    att_desc COMCHA = "Communication channel"
    att_desc CSIZE = "Compass size"
    att_desc CPDATE = "Compilation date"
    att_desc CSCALE = "Compilation scale"
    att_desc CONDTN = "Condition"
    att_desc CONRAD = "Conspicuous Radar"
    att_desc CONVIS = "Conspicuous visual"
    att_desc CURVEL = "Current velocity"
    att_desc DATEND = "Date end"
    att_desc DATSTA = "Date start"
    att_desc DRVAL1 = "Depth range value 1"
    att_desc DRVAL2 = "Depth range value 2"
    att_desc DUNITS = "Depth units"
    att_desc ELEVAT = "Elevation"
    att_desc ESTRNG = "Estimated range of transmission"
    att_desc EXCLIT = "Exhibition condition of light"
    att_desc EXPSOU = "Exposition of sounding"
    att_desc FUNCTN = "Function"
    att_desc HEIGHT = "Height"
    att_desc HUNITS = "Height/length units"
    att_desc HORACC = "Horizontal accuracy"
    att_desc HORCLR = "Horizontal clearance"
    att_desc HORLEN = "Horizontal length"
    att_desc HORWID = "Horizontal width"
    att_desc ICEFAC = "Ice factor"
    att_desc INFORM = "Information"
    att_desc JRSDTN = "Jurisdiction"
    att_desc JUSTH = "Justification - horizontal"
    att_desc JUSTV = "Justification - vertical"
    att_desc LIFCAP = "Lifting capacity"
    att_desc LITCHR = "Light characteristic"
    att_desc LITVIS = "Light visibility"
    att_desc MARSYS = "Marks navigational - System of"
    att_desc MLTYLT = "Multiplicity of lights"
    att_desc NATION = "Nationality"
    att_desc NATCON = "Nature of construction"
    att_desc NATSUR = "Nature of surface"
    att_desc NATQUA = "Nature of surface - qualifying terms"
    att_desc NMDATE = "Notice to Mariners date"
    att_desc OBJNAM = "Object name"
    att_desc ORIENT = "Orientation"
    att_desc PEREND = "Periodic date end"
    att_desc PERSTA = "Periodic date start"
    att_desc PICREP = "Pictorial representation"
    att_desc PILDST = "Pilot district"
    att_desc PRCTRY = "Producing country"
    att_desc PRODCT = "Product"
    att_desc PUBREF = "Publication reference"
    att_desc QUASOU = "Quality of sounding measurement"
    att_desc RADWAL = "Radar wave length"
    att_desc RADIUS = "Radius"
    att_desc RECDAT = "Recording date"
    att_desc RECIND = "Recording indication"
    att_desc RYRMGV = "Reference year for magnetic variation"
    att_desc RESTRN = "Restriction"
    att_desc SCAMAX = "Scale maximum"
    att_desc SCAMIN = "Scale minimum"
    att_desc SCVAL1 = "Scale value one"
    att_desc SCVAL2 = "Scale value two"
    att_desc SECTR1 = "Sector limit one"
    att_desc SECTR2 = "Sector limit two"
    att_desc SHIPAM = "Shift parameters"
    att_desc SIGFRQ = "Signal frequency"
    att_desc SIGGEN = "Signal generation"
    att_desc SIGGRP = "Signal group"
    att_desc SIGPER = "Signal period"
    att_desc SIGSEQ = "Signal sequence"
    att_desc SOUACC = "Sounding accuracy"
    att_desc SDISMX = "Sounding distance - maximum"
    att_desc SDISMN = "Sounding distance - minimum"
    att_desc SORDAT = "Source date"
    att_desc SORIND = "Source indication"
    att_desc STATUS = "Status"
    att_desc SURATH = "Survey authority"
    att_desc SUREND = "Survey date - end"
    att_desc SURSTA = "Survey date - start"
    att_desc SURTYP = "Survey type"
    att_desc SCALE = "Symbol scaling factor"
    att_desc SCODE = "Symbolization code"
    att_desc TECSOU = "Technique of sounding measurement"
    att_desc TXSTR = "Text string"
    att_desc TXTDSC = "Textual description"
    att_desc TS_TSP = "Tidal stream - panel values"
    att_desc TS_TSV = "Tidal stream current - time series values"
    att_desc T_ACWL = "Tide - accuracy of water level"
    att_desc T_HWLW = "Tide - high and low water values"
    att_desc T_MTOD = "Tide - method of tidal prediction"
    att_desc T_THDF = "Tide - time and height differences"
    att_desc T_TINT = "Tide current - time interval of values"
    att_desc T_TSVL = "Tide - time series values"
    att_desc T_VAHC = "Tide - value of harmonic constituents"
    att_desc TIMEND = "Time end"
    att_desc TIMSTA = "Time start"
    att_desc TINTS = "Tint"
    att_desc TOPSHP = "Topmark/daymark shape"
    att_desc TRAFIC = "Traffic flow"
    att_desc VALACM = "Value of annual change in magnetic variation"
    att_desc VALDCO = "Value of depth contour"
    att_desc VALLMA = "Value of local magnetic anomaly"
    att_desc VALMAG = "Value of magnetic variation"
    att_desc VALMXR = "Value of maximum range"
    att_desc VALNMR = "Value of nominal range"
    att_desc VALSOU = "Value of sounding"
    att_desc VERACC = "Vertical accuracy"
    att_desc VERCLR = "Vertical clearance"
    att_desc VERCCL = "Vertical clearance / closed"
    att_desc VERCOP = "Vertical clearance / open"
    att_desc VERCSA = "Vertical clearance / safe"
    att_desc VERDAT = "Vertical datum"
    att_desc VERLEN = "Vertical length"
    att_desc WATLEV = "Water level effect"
    att_desc CAT_TS = "Category of Tidal stream"
    att_desc PUNITS = "Positional accuracy units"
    att_desc NINFOM = "Information in national language"
    att_desc NOBJNM = "Object name in national language"
    att_desc NPLDST = "Pilot district in national language"
    att_desc NTXST = "Text string in national language"
    att_desc NTXTDS = "Textual description in national language"
    att_desc HORDAT = "Horizontal datum"
    att_desc POSACC = "Positional Accuracy"
    att_desc QUAPOS = "Quality of position"

    att_type' AGENCY = 'A'
    att_type' BCNSHP = 'E'
    att_type' BUISHP = 'E'
    att_type' BOYSHP = 'E'
    att_type' BURDEP = 'F'
    att_type' CALSGN = 'S'
    att_type' CATAIR = 'L'
    att_type' CATACH = 'L'
    att_type' CATBRG = 'L'
    att_type' CATBUA = 'E'
    att_type' CATCBL = 'E'
    att_type' CATCAN = 'E'
    att_type' CATCAM = 'E'
    att_type' CATCHP = 'E'
    att_type' CATCOA = 'E'
    att_type' CATCTR = 'E'
    att_type' CATCON = 'E'
    att_type' CATCOV = 'E'
    att_type' CATCRN = 'E'
    att_type' CATDAM = 'E'
    att_type' CATDIS = 'E'
    att_type' CATDOC = 'E'
    att_type' CATDPG = 'L'
    att_type' CATFNC = 'E'
    att_type' CATFRY = 'E'
    att_type' CATFIF = 'E'
    att_type' CATFOG = 'E'
    att_type' CATFOR = 'E'
    att_type' CATGAT = 'E'
    att_type' CATHAF = 'L'
    att_type' CATHLK = 'L'
    att_type' CATICE = 'E'
    att_type' CATINB = 'E'
    att_type' CATLND = 'L'
    att_type' CATLMK = 'L'
    att_type' CATLAM = 'E'
    att_type' CATLIT = 'L'
    att_type' CATMFA = 'E'
    att_type' CATMPA = 'L'
    att_type' CATMOR = 'E'
    att_type' CATNAV = 'E'
    att_type' CATOBS = 'E'
    att_type' CATOFP = 'L'
    att_type' CATOLB = 'E'
    att_type' CATPLE = 'E'
    att_type' CATPIL = 'E'
    att_type' CATPIP = 'L'
    att_type' CATPRA = 'E'
    att_type' CATPYL = 'E'
    att_type' CATQUA = 'E'
    att_type' CATRAS = 'E'
    att_type' CATRTB = 'E'
    att_type' CATROS = 'L'
    att_type' CATTRK = 'E'
    att_type' CATRSC = 'L'
    att_type' CATREA = 'L'
    att_type' CATROD = 'E'
    att_type' CATRUN = 'E'
    att_type' CATSEA = 'E'
    att_type' CATSLC = 'E'
    att_type' CATSIT = 'L'
    att_type' CATSIW = 'L'
    att_type' CATSIL = 'E'
    att_type' CATSLO = 'E'
    att_type' CATSCF = 'L'
    att_type' CATSPM = 'L'
    att_type' CATTSS = 'E'
    att_type' CATVEG = 'L'
    att_type' CATWAT = 'E'
    att_type' CATWED = 'E'
    att_type' CATWRK = 'E'
    att_type' CATZOC = 'E'
    att_type' SPACE = 'E'
    att_type' CHARS = 'A'
    att_type' COLOUR = 'L'
    att_type' COLPAT = 'L'
    att_type' COMCHA = 'A'
    att_type' CSIZE = 'F'
    att_type' CPDATE = 'A'
    att_type' CSCALE = 'I'
    att_type' CONDTN = 'E'
    att_type' CONRAD = 'E'
    att_type' CONVIS = 'E'
    att_type' CURVEL = 'F'
    att_type' DATEND = 'A'
    att_type' DATSTA = 'A'
    att_type' DRVAL1 = 'F'
    att_type' DRVAL2 = 'F'
    att_type' DUNITS = 'E'
    att_type' ELEVAT = 'F'
    att_type' ESTRNG = 'F'
    att_type' EXCLIT = 'E'
    att_type' EXPSOU = 'E'
    att_type' FUNCTN = 'L'
    att_type' HEIGHT = 'F'
    att_type' HUNITS = 'E'
    att_type' HORACC = 'F'
    att_type' HORCLR = 'F'
    att_type' HORLEN = 'F'
    att_type' HORWID = 'F'
    att_type' ICEFAC = 'F'
    att_type' INFORM = 'S'
    att_type' JRSDTN = 'E'
    att_type' JUSTH = 'E'
    att_type' JUSTV = 'E'
    att_type' LIFCAP = 'F'
    att_type' LITCHR = 'E'
    att_type' LITVIS = 'L'
    att_type' MARSYS = 'E'
    att_type' MLTYLT = 'I'
    att_type' NATION = 'A'
    att_type' NATCON = 'L'
    att_type' NATSUR = 'L'
    att_type' NATQUA = 'L'
    att_type' NMDATE = 'A'
    att_type' OBJNAM = 'S'
    att_type' ORIENT = 'F'
    att_type' PEREND = 'A'
    att_type' PERSTA = 'A'
    att_type' PICREP = 'S'
    att_type' PILDST = 'S'
    att_type' PRCTRY = 'A'
    att_type' PRODCT = 'L'
    att_type' PUBREF = 'S'
    att_type' QUASOU = 'L'
    att_type' RADWAL = 'A'
    att_type' RADIUS = 'F'
    att_type' RECDAT = 'A'
    att_type' RECIND = 'A'
    att_type' RYRMGV = 'A'
    att_type' RESTRN = 'L'
    att_type' SCAMAX = 'I'
    att_type' SCAMIN = 'I'
    att_type' SCVAL1 = 'I'
    att_type' SCVAL2 = 'I'
    att_type' SECTR1 = 'F'
    att_type' SECTR2 = 'F'
    att_type' SHIPAM = 'A'
    att_type' SIGFRQ = 'I'
    att_type' SIGGEN = 'E'
    att_type' SIGGRP = 'A'
    att_type' SIGPER = 'F'
    att_type' SIGSEQ = 'A'
    att_type' SOUACC = 'F'
    att_type' SDISMX = 'I'
    att_type' SDISMN = 'I'
    att_type' SORDAT = 'A'
    att_type' SORIND = 'A'
    att_type' STATUS = 'L'
    att_type' SURATH = 'S'
    att_type' SUREND = 'A'
    att_type' SURSTA = 'A'
    att_type' SURTYP = 'L'
    att_type' SCALE = 'F'
    att_type' SCODE = 'A'
    att_type' TECSOU = 'L'
    att_type' TXSTR = 'S'
    att_type' TXTDSC = 'S'
    att_type' TS_TSP = 'A'
    att_type' TS_TSV = 'A'
    att_type' T_ACWL = 'E'
    att_type' T_HWLW = 'A'
    att_type' T_MTOD = 'E'
    att_type' T_THDF = 'A'
    att_type' T_TINT = 'I'
    att_type' T_TSVL = 'A'
    att_type' T_VAHC = 'A'
    att_type' TIMEND = 'A'
    att_type' TIMSTA = 'A'
    att_type' TINTS = 'E'
    att_type' TOPSHP = 'E'
    att_type' TRAFIC = 'E'
    att_type' VALACM = 'F'
    att_type' VALDCO = 'F'
    att_type' VALLMA = 'F'
    att_type' VALMAG = 'F'
    att_type' VALMXR = 'F'
    att_type' VALNMR = 'F'
    att_type' VALSOU = 'F'
    att_type' VERACC = 'F'
    att_type' VERCLR = 'F'
    att_type' VERCCL = 'F'
    att_type' VERCOP = 'F'
    att_type' VERCSA = 'F'
    att_type' VERDAT = 'E'
    att_type' VERLEN = 'F'
    att_type' WATLEV = 'E'
    att_type' CAT_TS = 'E'
    att_type' PUNITS = 'E'
    att_type' NINFOM = 'S'
    att_type' NOBJNM = 'S'
    att_type' NPLDST = 'S'
    att_type' NTXST = 'S'
    att_type' NTXTDS = 'S'
    att_type' HORDAT = 'E'
    att_type' POSACC = 'F'
    att_type' QUAPOS = 'E'

    att_class' AGENCY = 'F'
    att_class' BCNSHP = 'F'
    att_class' BUISHP = 'F'
    att_class' BOYSHP = 'F'
    att_class' BURDEP = 'F'
    att_class' CALSGN = 'F'
    att_class' CATAIR = 'F'
    att_class' CATACH = 'F'
    att_class' CATBRG = 'F'
    att_class' CATBUA = 'F'
    att_class' CATCBL = 'F'
    att_class' CATCAN = 'F'
    att_class' CATCAM = 'F'
    att_class' CATCHP = 'F'
    att_class' CATCOA = 'F'
    att_class' CATCTR = 'F'
    att_class' CATCON = 'F'
    att_class' CATCOV = 'F'
    att_class' CATCRN = 'F'
    att_class' CATDAM = 'F'
    att_class' CATDIS = 'F'
    att_class' CATDOC = 'F'
    att_class' CATDPG = 'F'
    att_class' CATFNC = 'F'
    att_class' CATFRY = 'F'
    att_class' CATFIF = 'F'
    att_class' CATFOG = 'F'
    att_class' CATFOR = 'F'
    att_class' CATGAT = 'F'
    att_class' CATHAF = 'F'
    att_class' CATHLK = 'F'
    att_class' CATICE = 'F'
    att_class' CATINB = 'F'
    att_class' CATLND = 'F'
    att_class' CATLMK = 'F'
    att_class' CATLAM = 'F'
    att_class' CATLIT = 'F'
    att_class' CATMFA = 'F'
    att_class' CATMPA = 'F'
    att_class' CATMOR = 'F'
    att_class' CATNAV = 'F'
    att_class' CATOBS = 'F'
    att_class' CATOFP = 'F'
    att_class' CATOLB = 'F'
    att_class' CATPLE = 'F'
    att_class' CATPIL = 'F'
    att_class' CATPIP = 'F'
    att_class' CATPRA = 'F'
    att_class' CATPYL = 'F'
    att_class' CATQUA = 'F'
    att_class' CATRAS = 'F'
    att_class' CATRTB = 'F'
    att_class' CATROS = 'F'
    att_class' CATTRK = 'F'
    att_class' CATRSC = 'F'
    att_class' CATREA = 'F'
    att_class' CATROD = 'F'
    att_class' CATRUN = 'F'
    att_class' CATSEA = 'F'
    att_class' CATSLC = 'F'
    att_class' CATSIT = 'F'
    att_class' CATSIW = 'F'
    att_class' CATSIL = 'F'
    att_class' CATSLO = 'F'
    att_class' CATSCF = 'F'
    att_class' CATSPM = 'F'
    att_class' CATTSS = 'F'
    att_class' CATVEG = 'F'
    att_class' CATWAT = 'F'
    att_class' CATWED = 'F'
    att_class' CATWRK = 'F'
    att_class' CATZOC = 'F'
    att_class' SPACE = '$'
    att_class' CHARS = '$'
    att_class' COLOUR = 'F'
    att_class' COLPAT = 'F'
    att_class' COMCHA = 'F'
    att_class' CSIZE = '$'
    att_class' CPDATE = 'F'
    att_class' CSCALE = 'F'
    att_class' CONDTN = 'F'
    att_class' CONRAD = 'F'
    att_class' CONVIS = 'F'
    att_class' CURVEL = 'F'
    att_class' DATEND = 'F'
    att_class' DATSTA = 'F'
    att_class' DRVAL1 = 'F'
    att_class' DRVAL2 = 'F'
    att_class' DUNITS = 'F'
    att_class' ELEVAT = 'F'
    att_class' ESTRNG = 'F'
    att_class' EXCLIT = 'F'
    att_class' EXPSOU = 'F'
    att_class' FUNCTN = 'F'
    att_class' HEIGHT = 'F'
    att_class' HUNITS = 'F'
    att_class' HORACC = 'F'
    att_class' HORCLR = 'F'
    att_class' HORLEN = 'F'
    att_class' HORWID = 'F'
    att_class' ICEFAC = 'F'
    att_class' INFORM = 'F'
    att_class' JRSDTN = 'F'
    att_class' JUSTH = '$'
    att_class' JUSTV = '$'
    att_class' LIFCAP = 'F'
    att_class' LITCHR = 'F'
    att_class' LITVIS = 'F'
    att_class' MARSYS = 'F'
    att_class' MLTYLT = 'F'
    att_class' NATION = 'F'
    att_class' NATCON = 'F'
    att_class' NATSUR = 'F'
    att_class' NATQUA = 'F'
    att_class' NMDATE = 'F'
    att_class' OBJNAM = 'F'
    att_class' ORIENT = 'F'
    att_class' PEREND = 'F'
    att_class' PERSTA = 'F'
    att_class' PICREP = 'F'
    att_class' PILDST = 'F'
    att_class' PRCTRY = 'F'
    att_class' PRODCT = 'F'
    att_class' PUBREF = 'F'
    att_class' QUASOU = 'F'
    att_class' RADWAL = 'F'
    att_class' RADIUS = 'F'
    att_class' RECDAT = 'F'
    att_class' RECIND = 'F'
    att_class' RYRMGV = 'F'
    att_class' RESTRN = 'F'
    att_class' SCAMAX = 'F'
    att_class' SCAMIN = 'F'
    att_class' SCVAL1 = 'F'
    att_class' SCVAL2 = 'F'
    att_class' SECTR1 = 'F'
    att_class' SECTR2 = 'F'
    att_class' SHIPAM = 'F'
    att_class' SIGFRQ = 'F'
    att_class' SIGGEN = 'F'
    att_class' SIGGRP = 'F'
    att_class' SIGPER = 'F'
    att_class' SIGSEQ = 'F'
    att_class' SOUACC = 'F'
    att_class' SDISMX = 'F'
    att_class' SDISMN = 'F'
    att_class' SORDAT = 'F'
    att_class' SORIND = 'F'
    att_class' STATUS = 'F'
    att_class' SURATH = 'F'
    att_class' SUREND = 'F'
    att_class' SURSTA = 'F'
    att_class' SURTYP = 'F'
    att_class' SCALE = '$'
    att_class' SCODE = '$'
    att_class' TECSOU = 'F'
    att_class' TXSTR = '$'
    att_class' TXTDSC = 'F'
    att_class' TS_TSP = 'F'
    att_class' TS_TSV = 'F'
    att_class' T_ACWL = 'F'
    att_class' T_HWLW = 'F'
    att_class' T_MTOD = 'F'
    att_class' T_THDF = 'F'
    att_class' T_TINT = 'F'
    att_class' T_TSVL = 'F'
    att_class' T_VAHC = 'F'
    att_class' TIMEND = 'F'
    att_class' TIMSTA = 'F'
    att_class' TINTS = '$'
    att_class' TOPSHP = 'F'
    att_class' TRAFIC = 'F'
    att_class' VALACM = 'F'
    att_class' VALDCO = 'F'
    att_class' VALLMA = 'F'
    att_class' VALMAG = 'F'
    att_class' VALMXR = 'F'
    att_class' VALNMR = 'F'
    att_class' VALSOU = 'F'
    att_class' VERACC = 'F'
    att_class' VERCLR = 'F'
    att_class' VERCCL = 'F'
    att_class' VERCOP = 'F'
    att_class' VERCSA = 'F'
    att_class' VERDAT = 'F'
    att_class' VERLEN = 'F'
    att_class' WATLEV = 'F'
    att_class' CAT_TS = 'F'
    att_class' PUNITS = 'F'
    att_class' NINFOM = 'N'
    att_class' NOBJNM = 'N'
    att_class' NPLDST = 'N'
    att_class' NTXST = 'N'
    att_class' NTXTDS = 'N'
    att_class' HORDAT = 'S'
    att_class' POSACC = 'S'
    att_class' QUAPOS = 'S'


data AttributeT = AGENCY | BCNSHP | BUISHP | BOYSHP | BURDEP | CALSGN | CATAIR | CATACH | CATBRG | CATBUA | CATCBL | CATCAN | CATCAM | CATCHP | CATCOA | CATCTR | CATCON | CATCOV | CATCRN | CATDAM | CATDIS | CATDOC | CATDPG | CATFNC | CATFRY | CATFIF | CATFOG | CATFOR | CATGAT | CATHAF | CATHLK | CATICE | CATINB | CATLND | CATLMK | CATLAM | CATLIT | CATMFA | CATMPA | CATMOR | CATNAV | CATOBS | CATOFP | CATOLB | CATPLE | CATPIL | CATPIP | CATPRA | CATPYL | CATQUA | CATRAS | CATRTB | CATROS | CATTRK | CATRSC | CATREA | CATROD | CATRUN | CATSEA | CATSLC | CATSIT | CATSIW | CATSIL | CATSLO | CATSCF | CATSPM | CATTSS | CATVEG | CATWAT | CATWED | CATWRK | CATZOC | SPACE | CHARS | COLOUR | COLPAT | COMCHA | CSIZE | CPDATE | CSCALE | CONDTN | CONRAD | CONVIS | CURVEL | DATEND | DATSTA | DRVAL1 | DRVAL2 | DUNITS | ELEVAT | ESTRNG | EXCLIT | EXPSOU | FUNCTN | HEIGHT | HUNITS | HORACC | HORCLR | HORLEN | HORWID | ICEFAC | INFORM | JRSDTN | JUSTH | JUSTV | LIFCAP | LITCHR | LITVIS | MARSYS | MLTYLT | NATION | NATCON | NATSUR | NATQUA | NMDATE | OBJNAM | ORIENT | PEREND | PERSTA | PICREP | PILDST | PRCTRY | PRODCT | PUBREF | QUASOU | RADWAL | RADIUS | RECDAT | RECIND | RYRMGV | RESTRN | SCAMAX | SCAMIN | SCVAL1 | SCVAL2 | SECTR1 | SECTR2 | SHIPAM | SIGFRQ | SIGGEN | SIGGRP | SIGPER | SIGSEQ | SOUACC | SDISMX | SDISMN | SORDAT | SORIND | STATUS | SURATH | SUREND | SURSTA | SURTYP | SCALE | SCODE | TECSOU | TXSTR | TXTDSC | TS_TSP | TS_TSV | T_ACWL | T_HWLW | T_MTOD | T_THDF | T_TINT | T_TSVL | T_VAHC | TIMEND | TIMSTA | TINTS | TOPSHP | TRAFIC | VALACM | VALDCO | VALLMA | VALMAG | VALMXR | VALNMR | VALSOU | VERACC | VERCLR | VERCCL | VERCOP | VERCSA | VERDAT | VERLEN | WATLEV | CAT_TS | PUNITS | NINFOM | NOBJNM | NPLDST | NTXST | NTXTDS | HORDAT | POSACC | QUAPOS
                  deriving (Show, Eq)



instance Enum AttributeT where
    toEnum 1 = AGENCY
    toEnum 2 = BCNSHP
    toEnum 3 = BUISHP
    toEnum 4 = BOYSHP
    toEnum 5 = BURDEP
    toEnum 6 = CALSGN
    toEnum 7 = CATAIR
    toEnum 8 = CATACH
    toEnum 9 = CATBRG
    toEnum 10 = CATBUA
    toEnum 11 = CATCBL
    toEnum 12 = CATCAN
    toEnum 13 = CATCAM
    toEnum 14 = CATCHP
    toEnum 15 = CATCOA
    toEnum 16 = CATCTR
    toEnum 17 = CATCON
    toEnum 18 = CATCOV
    toEnum 19 = CATCRN
    toEnum 20 = CATDAM
    toEnum 21 = CATDIS
    toEnum 22 = CATDOC
    toEnum 23 = CATDPG
    toEnum 24 = CATFNC
    toEnum 25 = CATFRY
    toEnum 26 = CATFIF
    toEnum 27 = CATFOG
    toEnum 28 = CATFOR
    toEnum 29 = CATGAT
    toEnum 30 = CATHAF
    toEnum 31 = CATHLK
    toEnum 32 = CATICE
    toEnum 33 = CATINB
    toEnum 34 = CATLND
    toEnum 35 = CATLMK
    toEnum 36 = CATLAM
    toEnum 37 = CATLIT
    toEnum 38 = CATMFA
    toEnum 39 = CATMPA
    toEnum 40 = CATMOR
    toEnum 41 = CATNAV
    toEnum 42 = CATOBS
    toEnum 43 = CATOFP
    toEnum 44 = CATOLB
    toEnum 45 = CATPLE
    toEnum 46 = CATPIL
    toEnum 47 = CATPIP
    toEnum 48 = CATPRA
    toEnum 49 = CATPYL
    toEnum 50 = CATQUA
    toEnum 51 = CATRAS
    toEnum 52 = CATRTB
    toEnum 53 = CATROS
    toEnum 54 = CATTRK
    toEnum 55 = CATRSC
    toEnum 56 = CATREA
    toEnum 57 = CATROD
    toEnum 58 = CATRUN
    toEnum 59 = CATSEA
    toEnum 60 = CATSLC
    toEnum 61 = CATSIT
    toEnum 62 = CATSIW
    toEnum 63 = CATSIL
    toEnum 64 = CATSLO
    toEnum 65 = CATSCF
    toEnum 66 = CATSPM
    toEnum 67 = CATTSS
    toEnum 68 = CATVEG
    toEnum 69 = CATWAT
    toEnum 70 = CATWED
    toEnum 71 = CATWRK
    toEnum 72 = CATZOC
    toEnum 73 = SPACE
    toEnum 74 = CHARS
    toEnum 75 = COLOUR
    toEnum 76 = COLPAT
    toEnum 77 = COMCHA
    toEnum 78 = CSIZE
    toEnum 79 = CPDATE
    toEnum 80 = CSCALE
    toEnum 81 = CONDTN
    toEnum 82 = CONRAD
    toEnum 83 = CONVIS
    toEnum 84 = CURVEL
    toEnum 85 = DATEND
    toEnum 86 = DATSTA
    toEnum 87 = DRVAL1
    toEnum 88 = DRVAL2
    toEnum 89 = DUNITS
    toEnum 90 = ELEVAT
    toEnum 91 = ESTRNG
    toEnum 92 = EXCLIT
    toEnum 93 = EXPSOU
    toEnum 94 = FUNCTN
    toEnum 95 = HEIGHT
    toEnum 96 = HUNITS
    toEnum 97 = HORACC
    toEnum 98 = HORCLR
    toEnum 99 = HORLEN
    toEnum 100 = HORWID
    toEnum 101 = ICEFAC
    toEnum 102 = INFORM
    toEnum 103 = JRSDTN
    toEnum 104 = JUSTH
    toEnum 105 = JUSTV
    toEnum 106 = LIFCAP
    toEnum 107 = LITCHR
    toEnum 108 = LITVIS
    toEnum 109 = MARSYS
    toEnum 110 = MLTYLT
    toEnum 111 = NATION
    toEnum 112 = NATCON
    toEnum 113 = NATSUR
    toEnum 114 = NATQUA
    toEnum 115 = NMDATE
    toEnum 116 = OBJNAM
    toEnum 117 = ORIENT
    toEnum 118 = PEREND
    toEnum 119 = PERSTA
    toEnum 120 = PICREP
    toEnum 121 = PILDST
    toEnum 122 = PRCTRY
    toEnum 123 = PRODCT
    toEnum 124 = PUBREF
    toEnum 125 = QUASOU
    toEnum 126 = RADWAL
    toEnum 127 = RADIUS
    toEnum 128 = RECDAT
    toEnum 129 = RECIND
    toEnum 130 = RYRMGV
    toEnum 131 = RESTRN
    toEnum 132 = SCAMAX
    toEnum 133 = SCAMIN
    toEnum 134 = SCVAL1
    toEnum 135 = SCVAL2
    toEnum 136 = SECTR1
    toEnum 137 = SECTR2
    toEnum 138 = SHIPAM
    toEnum 139 = SIGFRQ
    toEnum 140 = SIGGEN
    toEnum 141 = SIGGRP
    toEnum 142 = SIGPER
    toEnum 143 = SIGSEQ
    toEnum 144 = SOUACC
    toEnum 145 = SDISMX
    toEnum 146 = SDISMN
    toEnum 147 = SORDAT
    toEnum 148 = SORIND
    toEnum 149 = STATUS
    toEnum 150 = SURATH
    toEnum 151 = SUREND
    toEnum 152 = SURSTA
    toEnum 153 = SURTYP
    toEnum 154 = SCALE
    toEnum 155 = SCODE
    toEnum 156 = TECSOU
    toEnum 157 = TXSTR
    toEnum 158 = TXTDSC
    toEnum 159 = TS_TSP
    toEnum 160 = TS_TSV
    toEnum 161 = T_ACWL
    toEnum 162 = T_HWLW
    toEnum 163 = T_MTOD
    toEnum 164 = T_THDF
    toEnum 165 = T_TINT
    toEnum 166 = T_TSVL
    toEnum 167 = T_VAHC
    toEnum 168 = TIMEND
    toEnum 169 = TIMSTA
    toEnum 170 = TINTS
    toEnum 171 = TOPSHP
    toEnum 172 = TRAFIC
    toEnum 173 = VALACM
    toEnum 174 = VALDCO
    toEnum 175 = VALLMA
    toEnum 176 = VALMAG
    toEnum 177 = VALMXR
    toEnum 178 = VALNMR
    toEnum 179 = VALSOU
    toEnum 180 = VERACC
    toEnum 181 = VERCLR
    toEnum 182 = VERCCL
    toEnum 183 = VERCOP
    toEnum 184 = VERCSA
    toEnum 185 = VERDAT
    toEnum 186 = VERLEN
    toEnum 187 = WATLEV
    toEnum 188 = CAT_TS
    toEnum 189 = PUNITS
    toEnum 300 = NINFOM
    toEnum 301 = NOBJNM
    toEnum 302 = NPLDST
    toEnum 303 = NTXST
    toEnum 304 = NTXTDS
    toEnum 400 = HORDAT
    toEnum 401 = POSACC
    toEnum 402 = QUAPOS
    toEnum i = error $ "unknown AttributeT code: " ++ show i
    fromEnum AGENCY = 1
    fromEnum BCNSHP = 2
    fromEnum BUISHP = 3
    fromEnum BOYSHP = 4
    fromEnum BURDEP = 5
    fromEnum CALSGN = 6
    fromEnum CATAIR = 7
    fromEnum CATACH = 8
    fromEnum CATBRG = 9
    fromEnum CATBUA = 10
    fromEnum CATCBL = 11
    fromEnum CATCAN = 12
    fromEnum CATCAM = 13
    fromEnum CATCHP = 14
    fromEnum CATCOA = 15
    fromEnum CATCTR = 16
    fromEnum CATCON = 17
    fromEnum CATCOV = 18
    fromEnum CATCRN = 19
    fromEnum CATDAM = 20
    fromEnum CATDIS = 21
    fromEnum CATDOC = 22
    fromEnum CATDPG = 23
    fromEnum CATFNC = 24
    fromEnum CATFRY = 25
    fromEnum CATFIF = 26
    fromEnum CATFOG = 27
    fromEnum CATFOR = 28
    fromEnum CATGAT = 29
    fromEnum CATHAF = 30
    fromEnum CATHLK = 31
    fromEnum CATICE = 32
    fromEnum CATINB = 33
    fromEnum CATLND = 34
    fromEnum CATLMK = 35
    fromEnum CATLAM = 36
    fromEnum CATLIT = 37
    fromEnum CATMFA = 38
    fromEnum CATMPA = 39
    fromEnum CATMOR = 40
    fromEnum CATNAV = 41
    fromEnum CATOBS = 42
    fromEnum CATOFP = 43
    fromEnum CATOLB = 44
    fromEnum CATPLE = 45
    fromEnum CATPIL = 46
    fromEnum CATPIP = 47
    fromEnum CATPRA = 48
    fromEnum CATPYL = 49
    fromEnum CATQUA = 50
    fromEnum CATRAS = 51
    fromEnum CATRTB = 52
    fromEnum CATROS = 53
    fromEnum CATTRK = 54
    fromEnum CATRSC = 55
    fromEnum CATREA = 56
    fromEnum CATROD = 57
    fromEnum CATRUN = 58
    fromEnum CATSEA = 59
    fromEnum CATSLC = 60
    fromEnum CATSIT = 61
    fromEnum CATSIW = 62
    fromEnum CATSIL = 63
    fromEnum CATSLO = 64
    fromEnum CATSCF = 65
    fromEnum CATSPM = 66
    fromEnum CATTSS = 67
    fromEnum CATVEG = 68
    fromEnum CATWAT = 69
    fromEnum CATWED = 70
    fromEnum CATWRK = 71
    fromEnum CATZOC = 72
    fromEnum SPACE = 73
    fromEnum CHARS = 74
    fromEnum COLOUR = 75
    fromEnum COLPAT = 76
    fromEnum COMCHA = 77
    fromEnum CSIZE = 78
    fromEnum CPDATE = 79
    fromEnum CSCALE = 80
    fromEnum CONDTN = 81
    fromEnum CONRAD = 82
    fromEnum CONVIS = 83
    fromEnum CURVEL = 84
    fromEnum DATEND = 85
    fromEnum DATSTA = 86
    fromEnum DRVAL1 = 87
    fromEnum DRVAL2 = 88
    fromEnum DUNITS = 89
    fromEnum ELEVAT = 90
    fromEnum ESTRNG = 91
    fromEnum EXCLIT = 92
    fromEnum EXPSOU = 93
    fromEnum FUNCTN = 94
    fromEnum HEIGHT = 95
    fromEnum HUNITS = 96
    fromEnum HORACC = 97
    fromEnum HORCLR = 98
    fromEnum HORLEN = 99
    fromEnum HORWID = 100
    fromEnum ICEFAC = 101
    fromEnum INFORM = 102
    fromEnum JRSDTN = 103
    fromEnum JUSTH = 104
    fromEnum JUSTV = 105
    fromEnum LIFCAP = 106
    fromEnum LITCHR = 107
    fromEnum LITVIS = 108
    fromEnum MARSYS = 109
    fromEnum MLTYLT = 110
    fromEnum NATION = 111
    fromEnum NATCON = 112
    fromEnum NATSUR = 113
    fromEnum NATQUA = 114
    fromEnum NMDATE = 115
    fromEnum OBJNAM = 116
    fromEnum ORIENT = 117
    fromEnum PEREND = 118
    fromEnum PERSTA = 119
    fromEnum PICREP = 120
    fromEnum PILDST = 121
    fromEnum PRCTRY = 122
    fromEnum PRODCT = 123
    fromEnum PUBREF = 124
    fromEnum QUASOU = 125
    fromEnum RADWAL = 126
    fromEnum RADIUS = 127
    fromEnum RECDAT = 128
    fromEnum RECIND = 129
    fromEnum RYRMGV = 130
    fromEnum RESTRN = 131
    fromEnum SCAMAX = 132
    fromEnum SCAMIN = 133
    fromEnum SCVAL1 = 134
    fromEnum SCVAL2 = 135
    fromEnum SECTR1 = 136
    fromEnum SECTR2 = 137
    fromEnum SHIPAM = 138
    fromEnum SIGFRQ = 139
    fromEnum SIGGEN = 140
    fromEnum SIGGRP = 141
    fromEnum SIGPER = 142
    fromEnum SIGSEQ = 143
    fromEnum SOUACC = 144
    fromEnum SDISMX = 145
    fromEnum SDISMN = 146
    fromEnum SORDAT = 147
    fromEnum SORIND = 148
    fromEnum STATUS = 149
    fromEnum SURATH = 150
    fromEnum SUREND = 151
    fromEnum SURSTA = 152
    fromEnum SURTYP = 153
    fromEnum SCALE = 154
    fromEnum SCODE = 155
    fromEnum TECSOU = 156
    fromEnum TXSTR = 157
    fromEnum TXTDSC = 158
    fromEnum TS_TSP = 159
    fromEnum TS_TSV = 160
    fromEnum T_ACWL = 161
    fromEnum T_HWLW = 162
    fromEnum T_MTOD = 163
    fromEnum T_THDF = 164
    fromEnum T_TINT = 165
    fromEnum T_TSVL = 166
    fromEnum T_VAHC = 167
    fromEnum TIMEND = 168
    fromEnum TIMSTA = 169
    fromEnum TINTS = 170
    fromEnum TOPSHP = 171
    fromEnum TRAFIC = 172
    fromEnum VALACM = 173
    fromEnum VALDCO = 174
    fromEnum VALLMA = 175
    fromEnum VALMAG = 176
    fromEnum VALMXR = 177
    fromEnum VALNMR = 178
    fromEnum VALSOU = 179
    fromEnum VERACC = 180
    fromEnum VERCLR = 181
    fromEnum VERCCL = 182
    fromEnum VERCOP = 183
    fromEnum VERCSA = 184
    fromEnum VERDAT = 185
    fromEnum VERLEN = 186
    fromEnum WATLEV = 187
    fromEnum CAT_TS = 188
    fromEnum PUNITS = 189
    fromEnum NINFOM = 300
    fromEnum NOBJNM = 301
    fromEnum NPLDST = 302
    fromEnum NTXST = 303
    fromEnum NTXTDS = 304
    fromEnum HORDAT = 400
    fromEnum POSACC = 401
    fromEnum QUAPOS = 402


enumValues :: Map Integer (Map Integer String)
enumValues = M.fromList [ (e, enumValuesMap e) | e <- S.elems allEnums]
    where enumValuesMap :: Integer -> Map Integer String
          enumValuesMap i = M.fromList . map snd . filter (\(k, _) -> k == i) $ enumValues'
          allEnums :: Set Integer
          allEnums = S.fromList $ map fst enumValues'

enumValues' = [
    (2,(1,"stake, pole, perch, post")),
    (2,(2,"whity")),
    (2,(3,"beacon tower")),
    (2,(4,"lattice beacon")),
    (2,(5,"pile beacon")),
    (2,(6,"cairn")),
    (2,(7,"buoyant beacon")),
    (3,(5,"high-rise building")),
    (3,(6,"pyramid")),
    (3,(7,"cylindrical")),
    (3,(8,"spherical")),
    (3,(9,"cubic")),
    (4,(1,"conical (nun, ogival)")),
    (4,(2,"can (cylindrical)")),
    (4,(3,"spherical")),
    (4,(4,"pillar")),
    (4,(5,"spar (spindle)")),
    (4,(6,"barrel (tun)")),
    (4,(7,"super-buoy")),
    (4,(8,"ice buoy")),
    (7,(1,"military aeroplane airport")),
    (7,(2,"civil aeroplane airport")),
    (7,(3,"military heliport")),
    (7,(4,"civil heliport")),
    (7,(5,"glider airfield")),
    (7,(6,"small planes airfield")),
    (7,(8,"emergency airfield")),
    (8,(1,"unrestricted anchorage")),
    (8,(2,"deep water anchorage")),
    (8,(3,"tanker anchorage")),
    (8,(4,"explosives anchorage")),
    (8,(5,"quarantine anchorage")),
    (8,(6,"sea-plane anchorage")),
    (8,(7,"small craft anchorage")),
    (8,(8,"small craft mooring area")),
    (8,(9,"anchorage for periods up to 24 hours")),
    (9,(1,"fixed bridge")),
    (9,(2,"opening bridge")),
    (9,(3,"swing bridge")),
    (9,(4,"lifting bridge")),
    (9,(5,"bascule bridge")),
    (9,(6,"pontoon bridge")),
    (9,(7,"draw bridge")),
    (9,(8,"transporter bridge")),
    (9,(9,"footbridge")),
    (9,(10,"viaduct")),
    (9,(11,"aqueduct")),
    (9,(12,"suspension bridge")),
    (10,(1,"urban area")),
    (10,(2,"settlement")),
    (10,(3,"village")),
    (10,(4,"town")),
    (10,(5,"city")),
    (10,(6,"holiday village")),
    (11,(1,"power line")),
    (11,(3,"transmission line")),
    (11,(4,"telephone")),
    (11,(5,"telegraph")),
    (11,(6,"mooring cable/chain")),
    (12,(1,"transportation")),
    (12,(2,"drainage")),
    (12,(3,"irrigation")),
    (13,(1,"north cardinal mark")),
    (13,(2,"east cardinal mark")),
    (13,(3,"south cardinal mark")),
    (13,(4,"west cardinal mark")),
    (14,(1,"custom")),
    (15,(1,"steep coast")),
    (15,(2,"flat coast")),
    (15,(3,"sandy shore")),
    (15,(4,"stony shore")),
    (15,(5,"shingly shore")),
    (15,(6,"glacier (seaward end)")),
    (15,(7,"mangrove")),
    (15,(8,"marshy shore")),
    (15,(9,"coral reef")),
    (15,(10,"ice coast")),
    (16,(1,"triangulation point")),
    (16,(2,"observation spot")),
    (16,(3,"fixed point")),
    (16,(4,"bench-mark")),
    (16,(5,"boundary mark")),
    (16,(6,"horizontal control, main station")),
    (16,(7,"horizontal control, secondary station")),
    (17,(1,"aerial cableway (telepheric)")),
    (17,(2,"belt conveyor")),
    (18,(1,"coverage available")),
    (18,(2,"no coverage available")),
    (19,(2,"container crane/gantry")),
    (19,(3,"sheerlegs")),
    (19,(4,"travelling crane")),
    (19,(5,"A-frame")),
    (20,(1,"weir")),
    (20,(2,"dam")),
    (20,(3,"flood barrage")),
    (21,(1,"distance mark not physically installed")),
    (21,(2,"visible mark, pole")),
    (21,(3,"visible mark, board")),
    (21,(4,"visible mark, unknown shape")),
    (22,(1,"tidal")),
    (22,(2,"non-tidal (wet dock)")),
    (23,(2,"chemical waste dumping ground")),
    (23,(3,"nuclear waste dumping ground")),
    (23,(4,"explosives dumping ground")),
    (23,(5,"spoil ground")),
    (23,(6,"vessel dumping ground")),
    (24,(1,"fence")),
    (24,(3,"hedge")),
    (24,(4,"wall")),
    (25,(1,"'free-moving' ferry")),
    (25,(2,"cable ferry")),
    (25,(3,"ice ferry")),
    (26,(1,"fishing stake")),
    (26,(2,"fish trap")),
    (26,(3,"fish weir")),
    (26,(4,"tunny net")),
    (27,(1,"explosive")),
    (27,(2,"diaphone")),
    (27,(3,"siren")),
    (27,(4,"nautophone")),
    (27,(5,"reed")),
    (27,(6,"tyfon")),
    (27,(7,"bell")),
    (27,(8,"whistle")),
    (27,(9,"gong")),
    (27,(10,"horn")),
    (28,(1,"castle")),
    (28,(2,"fort")),
    (28,(3,"battery")),
    (28,(4,"blockhouse")),
    (28,(5,"Martello tower")),
    (29,(2,"flood barrage gate")),
    (29,(3,"caisson")),
    (29,(4,"lock gate")),
    (29,(5,"dyke gate")),
    (30,(1,"RoRo-terminal")),
    (30,(3,"ferry terminal")),
    (30,(4,"fishing harbour")),
    (30,(5,"yacht harbour/marina")),
    (30,(6,"naval base")),
    (30,(7,"tanker terminal")),
    (30,(8,"passenger terminal")),
    (30,(9,"shipyard")),
    (30,(10,"container terminal")),
    (30,(11,"bulk terminal")),
    (31,(1,"floating restaurant")),
    (31,(2,"historic ship")),
    (31,(3,"museum")),
    (31,(4,"accomodation")),
    (31,(5,"floating breakwater")),
    (32,(1,"fast ice")),
    (32,(5,"glacier")),
    (32,(8,"polar ice")),
    (33,(1,"catenary anchor leg mooring (CALM)")),
    (33,(2,"single buoy mooring (SBM or SPM)")),
    (34,(1,"fen")),
    (34,(2,"marsh")),
    (34,(3,"moor/bog")),
    (34,(4,"heathland")),
    (34,(5,"mountain range")),
    (34,(6,"lowlands")),
    (34,(7,"canyon lands")),
    (34,(8,"paddy field")),
    (34,(9,"agricultural land")),
    (34,(10,"savanna/grassland")),
    (34,(11,"parkland")),
    (34,(12,"swamp")),
    (34,(13,"landslide")),
    (34,(14,"lava flow")),
    (34,(15,"salt pan")),
    (34,(16,"moraine")),
    (34,(17,"crater")),
    (34,(18,"cave")),
    (34,(19,"rock column or pinnacle")),
    (35,(1,"cairn")),
    (35,(2,"cemetery")),
    (35,(3,"chimney")),
    (35,(4,"dish aerial")),
    (35,(5,"flagstaff (flagpole)")),
    (35,(6,"flare stack")),
    (35,(7,"mast")),
    (35,(8,"windsock")),
    (35,(9,"monument")),
    (35,(10,"column (pillar)")),
    (35,(11,"memorial plaque")),
    (35,(12,"obelisk")),
    (35,(13,"statue")),
    (35,(14,"cross")),
    (35,(15,"dome")),
    (35,(16,"radar scanner")),
    (35,(17,"tower")),
    (35,(18,"windmill")),
    (35,(19,"windmotor")),
    (35,(20,"spire/minaret")),
    (36,(1,"port-hand lateral mark")),
    (36,(2,"starboard-hand lateral mark")),
    (36,(3,"preferred channel to starboard lateral mark")),
    (36,(4,"preferred channel to port lateral mark")),
    (37,(1,"directional function")),
    (37,(4,"leading light")),
    (37,(5,"aero light")),
    (37,(6,"air obstruction light")),
    (37,(7,"fog detector light")),
    (37,(8,"flood light")),
    (37,(9,"strip light")),
    (37,(10,"subsidiary light")),
    (37,(11,"spotlight")),
    (37,(12,"front")),
    (37,(13,"rear")),
    (37,(14,"lower")),
    (37,(15,"upper")),
    (37,(16,"moiré effect")),
    (37,(17,"emergency")),
    (37,(18,"bearing light")),
    (37,(19,"horizontally disposed")),
    (37,(20,"vertically disposed")),
    (38,(1,"crustaceans")),
    (38,(2,"oyster/mussels")),
    (38,(3,"fish")),
    (38,(4,"seaweed")),
    (39,(2,"torpedo exercise area")),
    (39,(3,"submarine exercise area")),
    (39,(4,"firing danger area")),
    (39,(5,"mine-laying practice area")),
    (39,(6,"small arms firing range")),
    (40,(1,"dolphin")),
    (40,(2,"deviation dolphin")),
    (40,(3,"bollard")),
    (40,(4,"tie-up wall")),
    (40,(5,"post or pile")),
    (40,(6,"chain/wire/cable")),
    (40,(7,"mooring buoy")),
    (41,(1,"clearing line")),
    (41,(2,"transit line")),
    (41,(3,"leading line bearing a recommended track")),
    (42,(1,"snag / stump")),
    (42,(2,"wellhead")),
    (42,(3,"diffuser")),
    (42,(4,"crib")),
    (42,(5,"fish haven")),
    (42,(6,"foul area")),
    (42,(7,"foul ground")),
    (42,(8,"ice boom")),
    (42,(9,"ground tackle")),
    (43,(1,"oil derrick / rig")),
    (43,(2,"production platform")),
    (43,(3,"observation / research platform")),
    (43,(4,"articulated loading platform (ALP)")),
    (43,(5,"single anchor leg mooring (SALM)")),
    (43,(6,"mooring tower")),
    (43,(7,"artificial island")),
    (43,(8,"floating production, storage and off-loading vessel (FPSO)")),
    (43,(9,"accomodation platform")),
    (43,(10,"navigation, communication and control buoy (NCCB)")),
    (44,(1,"oil retention (high pressure pipe)")),
    (44,(2,"floating oil barrier")),
    (45,(1,"stake")),
    (45,(3,"post")),
    (45,(4,"tripodal")),
    (46,(1,"boarding by pilot-cruising vessel")),
    (46,(2,"boarding by helicopter")),
    (46,(3,"pilot comes out from shore")),
    (47,(2,"outfall pipe")),
    (47,(3,"intake pipe")),
    (47,(4,"sewer")),
    (47,(5,"bubbler system")),
    (47,(6,"supply pipe")),
    (48,(1,"quarry")),
    (48,(2,"mine")),
    (48,(3,"stockpile")),
    (48,(4,"power station area")),
    (48,(5,"refinery area")),
    (48,(6,"timber yard")),
    (48,(7,"factory area")),
    (48,(8,"tank farm")),
    (48,(9,"wind farm")),
    (49,(1,"power transmission pylon/pole")),
    (49,(2,"telephone/telegraph pylon/pole")),
    (49,(3,"aerial cableway/sky pylon")),
    (49,(4,"bridge pylon/tower")),
    (49,(5,"bridge pier")),
    (50,(1,"data quality A")),
    (50,(2,"data quality B")),
    (50,(3,"data quality C")),
    (50,(4,"data quality D")),
    (50,(5,"data quality E")),
    (50,(6,"quality not evaluated")),
    (51,(1,"radar surveillance station")),
    (51,(2,"coast radar station")),
    (52,(1,"ramark, radar beacon transmitting continuously")),
    (52,(2,"racon, radar transponder beacon")),
    (52,(3,"leading racon/radar transponder beacon")),
    (53,(1,"circular (non-directional) marine or aero-marine radiobeacon")),
    (53,(2,"directional radiobeacon")),
    (53,(3,"rotating-pattern radiobeacon")),
    (53,(4,"Consol beacon")),
    (53,(5,"radio direction-finding station")),
    (53,(6,"coast radio station providing QTG service")),
    (53,(7,"aeronautical radiobeacon")),
    (53,(8,"Decca")),
    (53,(9,"Loran C")),
    (53,(10,"Differential GPS")),
    (53,(11,"Toran")),
    (53,(12,"Omega")),
    (53,(13,"Syledis")),
    (53,(14,"Chaika (Chayka)")),
    (54,(1,"based on a system of fixed marks")),
    (54,(2,"not based on a system of fixed marks")),
    (55,(1,"rescue station with lifeboat")),
    (55,(2,"rescue station with rocket")),
    (55,(4,"refuge for shipwrecked mariners")),
    (55,(5,"refuge for intertidal area walkers")),
    (55,(6,"lifeboat lying at a mooring")),
    (56,(1,"offshore safety zone")),
    (56,(4,"nature reserve")),
    (56,(5,"bird sanctuary")),
    (56,(6,"game preserve")),
    (56,(7,"seal sanctuary")),
    (56,(8,"degaussing range")),
    (56,(9,"military area")),
    (56,(10,"historic wreck area")),
    (56,(12,"navigational aid safety zone")),
    (56,(14,"minefield")),
    (56,(18,"swimming area")),
    (56,(19,"waiting area")),
    (56,(20,"research area")),
    (56,(21,"dredging area")),
    (56,(22,"fish sanctuary")),
    (56,(23,"ecological reserve")),
    (56,(24,"no wake area")),
    (56,(25,"swinging area")),
    (57,(1,"motorway")),
    (57,(2,"major road")),
    (57,(3,"minor road")),
    (57,(4,"track / path")),
    (57,(5,"major street")),
    (57,(6,"minor street")),
    (57,(7,"crossing")),
    (58,(1,"aeroplane")),
    (58,(2,"helicopter landing pad")),
    (59,(2,"gat")),
    (59,(3,"bank")),
    (59,(4,"deep")),
    (59,(5,"bay")),
    (59,(6,"trench")),
    (59,(7,"basin")),
    (59,(8,"mud flats")),
    (59,(9,"reef")),
    (59,(10,"ledge")),
    (59,(11,"canyon")),
    (59,(12,"narrows")),
    (59,(13,"shoal")),
    (59,(14,"knoll")),
    (59,(15,"ridge")),
    (59,(16,"seamount")),
    (59,(17,"pinnacle")),
    (59,(18,"abyssal plain")),
    (59,(19,"plateau")),
    (59,(20,"spur")),
    (59,(21,"shelf")),
    (59,(22,"trough")),
    (59,(23,"saddle")),
    (59,(24,"abyssal hills")),
    (59,(25,"apron")),
    (59,(26,"archipelagic apron")),
    (59,(27,"borderland")),
    (59,(28,"continental margin")),
    (59,(29,"continental rise")),
    (59,(30,"escarpment")),
    (59,(31,"fan")),
    (59,(32,"fracture zone")),
    (59,(33,"gap")),
    (59,(34,"guyot")),
    (59,(35,"hill")),
    (59,(36,"hole")),
    (59,(37,"levee")),
    (59,(38,"median valley")),
    (59,(39,"moat")),
    (59,(40,"mountains")),
    (59,(41,"peak")),
    (59,(42,"province")),
    (59,(43,"rise")),
    (59,(44,"seachannel")),
    (59,(45,"seamount chain")),
    (59,(46,"shelf edge")),
    (59,(47,"sill")),
    (59,(48,"slope")),
    (59,(49,"terrace")),
    (59,(50,"valley")),
    (59,(51,"canal")),
    (59,(52,"lake")),
    (59,(53,"river")),
    (60,(1,"breakwater")),
    (60,(2,"groyne (groin)")),
    (60,(3,"mole")),
    (60,(4,"pier ( jetty)")),
    (60,(5,"promenadepier")),
    (60,(6,"wharf (quay)")),
    (60,(7,"training wall")),
    (60,(8,"rip rap")),
    (60,(9,"revetment")),
    (60,(10,"sea wall")),
    (60,(11,"landing steps")),
    (60,(12,"ramp")),
    (60,(13,"slipway")),
    (60,(14,"fender")),
    (60,(15,"solid face wharf")),
    (60,(16,"open face wharf")),
    (61,(1,"port control")),
    (61,(2,"port entry and departure")),
    (61,(3,"International Port Traffic")),
    (61,(4,"berthing")),
    (61,(5,"dock")),
    (61,(6,"lock")),
    (61,(7,"flood barrage")),
    (61,(8,"bridge passage")),
    (61,(9,"dredging")),
    (62,(1,"danger")),
    (62,(2,"maritime obstruction")),
    (62,(3,"cable")),
    (62,(4,"military practice")),
    (62,(5,"distress")),
    (62,(6,"weather")),
    (62,(7,"storm")),
    (62,(8,"ice")),
    (62,(9,"time")),
    (62,(10,"tide")),
    (62,(11,"tidal stream")),
    (62,(12,"tide gauge")),
    (62,(13,"tide scale")),
    (62,(14,"diving")),
    (63,(1,"silo in general")),
    (63,(2,"tank in general")),
    (63,(3,"grain elevator")),
    (63,(4,"water tower")),
    (64,(1,"cutting")),
    (64,(2,"embankment")),
    (64,(3,"dune")),
    (64,(4,"hill")),
    (64,(5,"pingo")),
    (64,(6,"cliff")),
    (64,(7,"scree")),
    (65,(1,"visitor`s berth")),
    (65,(2,"nautical club")),
    (65,(3,"boat hoist")),
    (65,(4,"sailmaker")),
    (65,(5,"boatyard")),
    (65,(6,"public inn")),
    (65,(7,"restaurant")),
    (65,(8,"chandler")),
    (65,(9,"provisions")),
    (65,(10,"doctor")),
    (65,(11,"pharmacy")),
    (65,(12,"water tap")),
    (65,(13,"fuel station")),
    (65,(14,"electricity")),
    (65,(15,"bottle gas")),
    (65,(16,"showers")),
    (65,(17,"launderette")),
    (65,(18,"public toilets")),
    (65,(19,"post box")),
    (65,(20,"public telephone")),
    (65,(21,"refuse bin")),
    (65,(22,"car park")),
    (65,(23,"parking for boats and trailers")),
    (65,(24,"caravan site")),
    (65,(25,"camping site")),
    (65,(26,"sewerage pump-out station")),
    (65,(27,"emergency telephone")),
    (65,(28,"landing / launching place for boats")),
    (65,(29,"visitors mooring")),
    (65,(30,"scrubbing berth")),
    (65,(31,"picnic area")),
    (66,(1,"firing danger area mark")),
    (66,(2,"target mark")),
    (66,(3,"marker ship mark")),
    (66,(4,"degaussing range mark")),
    (66,(5,"barge mark")),
    (66,(6,"cable mark")),
    (66,(7,"spoil ground mark")),
    (66,(8,"outfall mark")),
    (66,(9,"ODAS (Ocean-Data-Acquisition-System)")),
    (66,(10,"recording mark")),
    (66,(11,"seaplane anchorage mark")),
    (66,(12,"recreation zone mark")),
    (66,(13,"private mark")),
    (66,(14,"mooring mark")),
    (66,(15,"LANBY (Large Automatic Navigational Buoy)")),
    (66,(16,"leading mark")),
    (66,(17,"measured distance mark")),
    (66,(18,"notice mark")),
    (66,(19,"TSS mark (Traffic Separation Scheme)")),
    (66,(20,"anchoring prohibited mark")),
    (66,(21,"berthing prohibited mark")),
    (66,(22,"overtaking prohibited mark")),
    (66,(23,"two-way traffic prohibited mark")),
    (66,(24,"'reduced wake' mark")),
    (66,(25,"speed limit mark")),
    (66,(26,"stop mark")),
    (66,(27,"general warning mark")),
    (66,(28,"'sound ship's siren' mark")),
    (66,(29,"restricted vertical clearence mark")),
    (66,(30,"maximum vessel's draught mark")),
    (66,(31,"restricted horizontal clearance mark")),
    (66,(32,"strong current warning mark")),
    (66,(33,"berthing permitted mark")),
    (66,(34,"overhead power cable mark")),
    (66,(35,"'channel edge gradient' mark")),
    (66,(36,"telephone mark")),
    (66,(37,"ferry crossing mark")),
    (66,(39,"pipline mark")),
    (66,(40,"anchorage mark")),
    (66,(41,"clearing mark")),
    (66,(42,"control mark")),
    (66,(43,"diving mark")),
    (66,(44,"refuge beacon")),
    (66,(45,"foul ground mark")),
    (66,(46,"yachting mark")),
    (66,(47,"heliport mark")),
    (66,(48,"GPS mark")),
    (66,(49,"seaplane landing mark")),
    (66,(50,"entry prohibited mark")),
    (66,(51,"work in progress mark")),
    (66,(52,"mark with unknown purpose")),
    (67,(1,"IMO - adopted")),
    (67,(2,"not IMO - adopted")),
    (68,(1,"grassland")),
    (68,(3,"bush")),
    (68,(4,"deciduous wood")),
    (68,(5,"coniferous  wood")),
    (68,(6,"wood in general (inc mixed wood)")),
    (68,(7,"mangroves")),
    (68,(10,"mixed crops")),
    (68,(11,"reed")),
    (68,(12,"moos")),
    (68,(13,"tree in general")),
    (68,(14,"evergreen tree")),
    (68,(15,"coniferous tree")),
    (68,(16,"palm tree")),
    (68,(17,"nipa palm tree")),
    (68,(18,"casuarina tree")),
    (68,(19,"eucalypt tree")),
    (68,(20,"deciduous tree")),
    (68,(21,"mangrove tree")),
    (68,(22,"filao tree")),
    (69,(1,"breakers")),
    (69,(2,"eddies")),
    (69,(3,"overfalls")),
    (69,(4,"tide rips")),
    (69,(5,"bombora")),
    (70,(1,"kelp")),
    (70,(2,"sea weed")),
    (70,(3,"sea grass")),
    (70,(4,"saragasso")),
    (71,(1,"non-dangerous wreck")),
    (71,(2,"dangerous wreck")),
    (71,(3,"distributed remains of wreck")),
    (71,(4,"wreck showing mast/masts")),
    (71,(5,"wreck showing any portion of hull or superstructure")),
    (72,(1,"zone of confidence A1")),
    (72,(2,"zone of confidence A2")),
    (72,(3,"zone of confidence B")),
    (72,(4,"zone of confidence C")),
    (72,(5,"zone of confidence D")),
    (72,(6,"zone of confidence U (data not assessed)")),
    (73,(1,"expanded/condensed")),
    (73,(2,"standard")),
    (75,(1,"white")),
    (75,(2,"black")),
    (75,(3,"red")),
    (75,(4,"green")),
    (75,(5,"blue")),
    (75,(6,"yellow")),
    (75,(7,"grey")),
    (75,(8,"brown")),
    (75,(9,"amber")),
    (75,(10,"violet")),
    (75,(11,"orange")),
    (75,(12,"magenta")),
    (75,(13,"pink")),
    (76,(1,"horizontal stripes")),
    (76,(2,"vertical stripes")),
    (76,(3,"diagonal stripes")),
    (76,(4,"squared")),
    (76,(5,"stripes (direction unknown)")),
    (76,(6,"border stripes")),
    (81,(1,"under construction")),
    (81,(2,"ruined")),
    (81,(3,"under reclamation")),
    (81,(4,"wingless")),
    (81,(5,"planned construction")),
    (82,(1,"radar conspicuous")),
    (82,(2,"not radar conspicuous")),
    (82,(3,"radar conspicuous (has radar reflector)")),
    (83,(1,"visual conspicuous")),
    (83,(2,"not visual conspicuous")),
    (89,(1,"metres")),
    (89,(2,"fathoms and feet")),
    (89,(3,"feet")),
    (89,(4,"fathoms and fractions")),
    (92,(1,"light shown without change of character")),
    (92,(2,"daytime light")),
    (92,(3,"fog light")),
    (92,(4,"night light")),
    (93,(1,"within the range of depth of the surrounding depth area")),
    (93,(2,"shoaler than range of depth of the surrounding depth area")),
    (93,(3,"deeper than range of depth of the surrounding depth area")),
    (94,(2,"harbour-master's office")),
    (94,(3,"custom office")),
    (94,(4,"health office")),
    (94,(5,"hospital")),
    (94,(6,"post office")),
    (94,(7,"hotel")),
    (94,(8,"railway station")),
    (94,(9,"police station")),
    (94,(10,"water-police station")),
    (94,(11,"pilot office")),
    (94,(12,"pilot lookout")),
    (94,(13,"bank office")),
    (94,(14,"headquarters for district control")),
    (94,(15,"transit shed/warehouse")),
    (94,(16,"factory")),
    (94,(17,"power station")),
    (94,(18,"administrative")),
    (94,(19,"educational facility")),
    (94,(20,"church")),
    (94,(21,"chapel")),
    (94,(22,"temple")),
    (94,(23,"pagoda")),
    (94,(24,"shinto shrine")),
    (94,(25,"buddhist temple")),
    (94,(26,"mosque")),
    (94,(27,"marabout")),
    (94,(28,"lookout")),
    (94,(29,"communication")),
    (94,(30,"television")),
    (94,(31,"radio")),
    (94,(32,"radar")),
    (94,(33,"light support")),
    (94,(34,"microwave")),
    (94,(35,"cooling")),
    (94,(36,"observation")),
    (94,(37,"timeball")),
    (94,(38,"clock")),
    (94,(39,"control")),
    (94,(40,"airship mooring")),
    (94,(41,"stadium")),
    (94,(42,"bus station")),
    (96,(1,"metres")),
    (96,(2,"feet")),
    (103,(1,"international")),
    (103,(2,"national")),
    (103,(3,"national sub-division")),
    (104,(1,"centre justified")),
    (104,(2,"right justified")),
    (104,(3,"left justified")),
    (105,(1,"bottom justified")),
    (105,(2,"centre justified")),
    (105,(3,"top justified")),
    (107,(1,"fixed")),
    (107,(2,"flashing")),
    (107,(3,"long-flashing")),
    (107,(4,"quick-flashing")),
    (107,(5,"very quick-flashing")),
    (107,(6,"ultra quick-flashing")),
    (107,(7,"isophased")),
    (107,(8,"occulting")),
    (107,(9,"interrupted quick-flashing")),
    (107,(10,"interrupted very quick-flashing")),
    (107,(11,"interrupted ultra quick-flashing")),
    (107,(12,"morse")),
    (107,(13,"fixed / flash")),
    (107,(14,"flash / long-flash")),
    (107,(15,"occulting / flash")),
    (107,(16,"fixed / long-flash")),
    (107,(17,"occulting alternating")),
    (107,(18,"long-flash alternating")),
    (107,(19,"flash alternating")),
    (107,(20,"group alternating")),
    (107,(25,"quick-flash plus long-flash")),
    (107,(26,"very quick-flash plus long-flash")),
    (107,(27,"ultra quick-flash plus long-flash")),
    (107,(28,"alternating")),
    (107,(29,"fixed and alternating flashing")),
    (108,(1,"high intensity")),
    (108,(2,"low intensity")),
    (108,(3,"faint")),
    (108,(4,"intensified")),
    (108,(5,"unintensified")),
    (108,(6,"visibility deliberately restricted")),
    (108,(7,"obscured")),
    (108,(8,"partially obscured")),
    (109,(1,"IALA A")),
    (109,(2,"IALA B")),
    (109,(9,"no system")),
    (109,(10,"other sytem")),
    (112,(1,"masonry")),
    (112,(2,"concreted")),
    (112,(3,"loose boulders")),
    (112,(4,"hard surfaced")),
    (112,(5,"unsurfaced")),
    (112,(6,"wooden")),
    (112,(7,"metal")),
    (112,(8,"glass reinforced plastic (GRP)")),
    (112,(9,"painted")),
    (113,(1,"mud")),
    (113,(2,"clay")),
    (113,(3,"silt")),
    (113,(4,"sand")),
    (113,(5,"stone")),
    (113,(6,"gravel")),
    (113,(7,"pebbles")),
    (113,(8,"cobbles")),
    (113,(9,"rock")),
    (113,(11,"lava")),
    (113,(14,"coral")),
    (113,(17,"shells")),
    (113,(18,"boulder")),
    (114,(1,"fine")),
    (114,(2,"medium")),
    (114,(3,"coarse")),
    (114,(4,"broken")),
    (114,(5,"sticky")),
    (114,(6,"soft")),
    (114,(7,"stiff")),
    (114,(8,"volcanic")),
    (114,(9,"calcareous")),
    (114,(10,"hard")),
    (123,(1,"oil")),
    (123,(2,"gas")),
    (123,(3,"water")),
    (123,(4,"stone")),
    (123,(5,"coal")),
    (123,(6,"ore")),
    (123,(7,"chemicals")),
    (123,(8,"drinking water")),
    (123,(9,"milk")),
    (123,(10,"bauxite")),
    (123,(11,"coke")),
    (123,(12,"iron ingots")),
    (123,(13,"salt")),
    (123,(14,"sand")),
    (123,(15,"timber")),
    (123,(16,"sawdust / wood chips")),
    (123,(17,"scrap metal")),
    (123,(18,"liquified natural gas (LNG)")),
    (123,(19,"liquified petroleum gas (LPG)")),
    (123,(20,"wine")),
    (123,(21,"cement")),
    (123,(22,"grain")),
    (125,(1,"depth known")),
    (125,(2,"depth unknown")),
    (125,(3,"doubtful sounding")),
    (125,(4,"unreliable sounding")),
    (125,(5,"no bottom found at value shown")),
    (125,(6,"least depth known")),
    (125,(7,"least depth unknown, safe clearance at value shown")),
    (125,(8,"value reported (not surveyed)")),
    (125,(9,"value reported (not confirmed)")),
    (125,(10,"maintained depth")),
    (125,(11,"not reguraly maintained")),
    (131,(1,"anchoring prohibited")),
    (131,(2,"anchoring restricted")),
    (131,(3,"fishing prohibited")),
    (131,(4,"fishing restricted")),
    (131,(5,"trawling prohibited")),
    (131,(6,"trawling restricted")),
    (131,(7,"entry prohibited")),
    (131,(8,"entry restricted")),
    (131,(9,"dredging prohibited")),
    (131,(10,"dredging restricted")),
    (131,(11,"diving prohibited")),
    (131,(12,"diving restricted")),
    (131,(13,"no wake")),
    (131,(14,"area to be avoided")),
    (131,(15,"construction prohibited")),
    (140,(1,"automatically")),
    (140,(2,"by wave action")),
    (140,(3,"by hand")),
    (140,(4,"by wind")),
    (149,(1,"permanent")),
    (149,(2,"occasional")),
    (149,(3,"recommended")),
    (149,(4,"disused")),
    (149,(5,"periodically/intermittent")),
    (149,(6,"reserved")),
    (149,(7,"temporary")),
    (149,(8,"private")),
    (149,(9,"mandatory")),
    (149,(11,"extinguished")),
    (149,(12,"illuminated")),
    (149,(13,"historic")),
    (149,(14,"public")),
    (149,(15,"synchronized")),
    (149,(16,"watched")),
    (149,(17,"un-watched")),
    (149,(18,"existence doubtful")),
    (153,(1,"reconnaissance/sketch survey")),
    (153,(2,"controlled survey")),
    (153,(4,"examintion survey")),
    (153,(5,"passage survey")),
    (153,(6,"remotely sensed")),
    (156,(1,"found by echo-sounder")),
    (156,(2,"found by side scan sonar")),
    (156,(3,"found by multi-beam")),
    (156,(4,"found by diver")),
    (156,(5,"found by lead-line")),
    (156,(6,"swept by wire-drag")),
    (156,(7,"found by laser")),
    (156,(8,"swept by vertical acoustic system")),
    (156,(9,"found by electromagnetic sensor")),
    (156,(10,"photogrammetry")),
    (156,(11,"satelite imagery")),
    (156,(12,"found by levelling")),
    (156,(13,"swept by side-scan sonar")),
    (156,(14,"computer generated")),
    (161,(1,"better than 0.1m and 10 minutes")),
    (161,(2,"worse than 0.1m or 10 minutes")),
    (163,(1,"simplified harmonic method of tidal prediction")),
    (163,(2,"full harmonic method of tidal prediction")),
    (163,(3,"height and time difference non-harmonic method")),
    (170,(1,"darkest blue")),
    (170,(2,"medium blue")),
    (170,(3,"lightest blue")),
    (171,(1,"cone, point up")),
    (171,(2,"cone, point down")),
    (171,(3,"sphere")),
    (171,(4,"2 sphere")),
    (171,(5,"cylinder (can)")),
    (171,(6,"board")),
    (171,(7,"x-shape (St. Andrew's cross)")),
    (171,(8,"upright cross (St. George cross)")),
    (171,(9,"cube, point up")),
    (171,(10,"2 cones, point to point")),
    (171,(11,"2 cones, base to base")),
    (171,(12,"rhombus (diamond)")),
    (171,(13,"2 cones (points upward)")),
    (171,(14,"2 cones (points downward)")),
    (171,(15,"besom, point up (broom or perch)")),
    (171,(16,"besom, point down (broom or perch)")),
    (171,(17,"flag")),
    (171,(18,"sphere over rhombus")),
    (171,(19,"square")),
    (171,(20,"rectangle, horizontal")),
    (171,(21,"rectangle, vertical")),
    (171,(22,"trapezium, up")),
    (171,(23,"trapezium, down")),
    (171,(24,"triangle, point up")),
    (171,(25,"triangle, point down")),
    (171,(26,"circle")),
    (171,(27,"two upright crosses (one over the other)")),
    (171,(28,"T-shape")),
    (171,(29,"triangle pointing up over a circle")),
    (171,(30,"upright cross over a circle")),
    (171,(31,"rhombus over a circle")),
    (171,(32,"circle over a triangle pointing up")),
    (171,(33,"other shape (see INFORM)")),
    (172,(1,"inbound")),
    (172,(2,"outbound")),
    (172,(3,"one-way")),
    (172,(4,"two-way")),
    (185,(1,"Mean low water springs")),
    (185,(2,"Mean lower low water springs")),
    (185,(3,"Mean sea level")),
    (185,(4,"Lowest low water")),
    (185,(5,"Mean low water")),
    (185,(6,"Lowest low water springs")),
    (185,(7,"Approximate mean low water springs")),
    (185,(8,"Indian spring low water")),
    (185,(9,"Low water springs")),
    (185,(10,"Approximate lowest astronomical tide")),
    (185,(11,"Nearly lowest low water")),
    (185,(12,"Mean lower low water")),
    (185,(13,"Low water")),
    (185,(14,"Approximate mean low water")),
    (185,(15,"Approximate mean lower low water")),
    (185,(16,"Mean high water")),
    (185,(17,"Mean high water springs")),
    (185,(18,"High water")),
    (185,(19,"Approximate mean sea level")),
    (185,(20,"High water springs")),
    (185,(21,"Mean higher high water")),
    (185,(22,"Equinoctial spring low water")),
    (185,(23,"Lowest astronomical tide")),
    (185,(24,"Local datum")),
    (185,(25,"International Great Lakes Datum 1985")),
    (185,(26,"Mean water level")),
    (185,(27,"Lower low water large tide")),
    (185,(28,"Higher high water lage tide")),
    (185,(29,"Nearly highest high water")),
    (187,(1,"partly submerged at high water")),
    (187,(2,"always dry")),
    (187,(3,"always under water/submerged")),
    (187,(4,"covers and uncovers")),
    (187,(5,"awash")),
    (187,(6,"subject to inundation or flooding")),
    (400,(1,"WGS 72")),
    (400,(2,"WGS 84")),
    (400,(3,"European 1950")),
    (400,(4,"Potsdam Datum")),
    (400,(5,"Adindan")),
    (400,(6,"Afgooye")),
    (400,(7,"Ain el Abd 1970")),
    (400,(8,"Anna 1 Astro 1965")),
    (400,(9,"Antigua Island Astro 1943")),
    (400,(10,"Arc 1950")),
    (400,(11,"Arc 1960")),
    (400,(12,"Ascension Island 1958")),
    (400,(13,"Astro beacon \"E\" 1945")),
    (400,(14,"Astro DOS 71/4")),
    (400,(15,"Astro Tern Island (FRIG) 1961")),
    (400,(16,"Astronimical Station 1952")),
    (400,(17,"Australian Geodetic 1966")),
    (400,(18,"Australian Geodetic 1984")),
    (400,(19,"Ayabelle Lighthouse")),
    (400,(20,"Bellevue (IGN)")),
    (400,(21,"Bermuda 1957")),
    (400,(22,"Bissau")),
    (400,(23,"Bogota Observatory")),
    (400,(24,"Bukit Rimpah")),
    (400,(25,"Camp Area Astro")),
    (400,(26,"Campo Inchauspe 1969")),
    (400,(27,"Canton Astro 1966")),
    (400,(28,"Cape")),
    (400,(29,"Cape Canaveral")),
    (400,(30,"Carthage")),
    (400,(31,"Chatam Island Astro 1971")),
    (400,(32,"Chua Astro")),
    (400,(33,"Corrego Alegre")),
    (400,(34,"Dabola")),
    (400,(35,"Djakarta (Batavia)")),
    (400,(36,"DOS 1968")),
    (400,(37,"Easter Island 1967")),
    (400,(38,"European 1979")),
    (400,(39,"Fort Thomas 1955")),
    (400,(40,"Gan 1970")),
    (400,(41,"Geodetic Datum 1949")),
    (400,(42,"Graciosa Base SW 1948")),
    (400,(43,"Guam 1963")),
    (400,(44,"Ganung Segara")),
    (400,(45,"GUX 1 Astro")),
    (400,(46,"Herat North")),
    (400,(47,"Hjorsey 1955")),
    (400,(48,"Hong Kong 1963")),
    (400,(49,"Hu-Tzu-Shan")),
    (400,(50,"Indian")),
    (400,(51,"Indian 1954")),
    (400,(52,"Indian 1975")),
    (400,(53,"Ireland 1965")),
    (400,(54,"ISTS 061 Astro 1968")),
    (400,(55,"ISTS 073 Astro 1969")),
    (400,(56,"Johnston Island 1961")),
    (400,(57,"Kandawala")),
    (400,(58,"Kerguelen Island 1949")),
    (400,(59,"Kertau 1948")),
    (400,(60,"Kusaie Astro 1951")),
    (400,(61,"")),
    (400,(62,"")),
    (400,(63,"")),
    (400,(64,"")),
    (400,(65,"")),
    (400,(66,"")),
    (400,(67,"")),
    (400,(68,"")),
    (400,(69,"")),
    (400,(70,"")),
    (400,(71,"")),
    (400,(72,"")),
    (400,(73,"")),
    (400,(74,"")),
    (400,(75,"")),
    (400,(76,"")),
    (400,(77,"")),
    (400,(78,"")),
    (400,(79,"")),
    (400,(80,"")),
    (400,(81,"")),
    (400,(82,"")),
    (400,(83,"")),
    (400,(84,"")),
    (400,(85,"")),
    (400,(86,"")),
    (400,(87,"")),
    (400,(88,"")),
    (400,(89,"")),
    (400,(90,"")),
    (400,(91,"")),
    (400,(92,"")),
    (400,(93,"")),
    (400,(94,"")),
    (400,(95,"")),
    (400,(96,"")),
    (400,(97,"")),
    (400,(98,"")),
    (400,(99,"South Asia")),
    (400,(100,"Tananarive Observatory 1925")),
    (402,(1,"surveyed")),
    (402,(2,"unsurveyed")),
    (402,(3,"inadequately surveyed")),
    (402,(4,"approximated")),
    (402,(5,"position doubtful")),
    (402,(6,"unreliable")),
    (402,(7,"reported (not surveyed)")),
    (402,(8,"reported (not confirmed)")),
    (402,(9,"estimated")),
    (402,(10,"precisely known")),
    (402,(11,"calculated"))]
