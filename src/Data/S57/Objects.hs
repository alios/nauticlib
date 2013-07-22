{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

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

module Data.S57.Objects where

import           Data.S57.Attributes


data ClassTag
    = Meta -- ^ Feature 'Object' which contains information about other objects
    | Cartographic -- ^ Feature 'Object' which contains information about the cartographic representation (including text) of real world entities
    | Geo -- ^ Feature 'Object' which contains information which carries the descriptive characteristics of a real world entity
    | Collection -- ^ Feature 'Object' which describes the relationship between other 'Object's

data Primitive =
    Point | Line | Area
    deriving (Eq, Show)


class Object t where
    type BaseType t :: *
    obj_code :: t -> Integer
    obj_fromCode :: Integer -> BaseType t
    obj_class :: t -> String
    obj_attrA :: t -> [AttributeT]
    obj_attrB :: t -> [AttributeT]
    obj_attrC :: t -> [AttributeT]
    obj_classTag :: t -> ClassTag
    obj_primitive :: t -> [Primitive]



data ADMARE = ADMARE
data AIRARE = AIRARE
data ACHBRT = ACHBRT
data ACHARE = ACHARE
data BCNCAR = BCNCAR
data BCNISD = BCNISD
data BCNLAT = BCNLAT
data BCNSAW = BCNSAW
data BCNSPP = BCNSPP
data BERTHS = BERTHS
data BRIDGE = BRIDGE
data BUISGL = BUISGL
data BUAARE = BUAARE
data BOYCAR = BOYCAR
data BOYINB = BOYINB
data BOYISD = BOYISD
data BOYLAT = BOYLAT
data BOYSAW = BOYSAW
data BOYSPP = BOYSPP
data CBLARE = CBLARE
data CBLOHD = CBLOHD
data CBLSUB = CBLSUB
data CANALS = CANALS
data CTSARE = CTSARE
data CAUSWY = CAUSWY
data CTNARE = CTNARE
data CHKPNT = CHKPNT
data CGUSTA = CGUSTA
data COALNE = COALNE
data CONZNE = CONZNE
data COSARE = COSARE
data CTRPNT = CTRPNT
data CONVYR = CONVYR
data CRANES = CRANES
data CURENT = CURENT
data CUSZNE = CUSZNE
data DAMCON = DAMCON
data DAYMAR = DAYMAR
data DWRTCL = DWRTCL
data DWRTPT = DWRTPT
data DEPARE = DEPARE
data DEPCNT = DEPCNT
data DISMAR = DISMAR
data DOCARE = DOCARE
data DRGARE = DRGARE
data DRYDOC = DRYDOC
data DMPGRD = DMPGRD
data DYKCON = DYKCON
data EXEZNE = EXEZNE
data FAIRWY = FAIRWY
data FNCLNE = FNCLNE
data FERYRT = FERYRT
data FSHZNE = FSHZNE
data FSHFAC = FSHFAC
data FSHGRD = FSHGRD
data FLODOC = FLODOC
data FOGSIG = FOGSIG
data FORSTC = FORSTC
data FRPARE = FRPARE
data GATCON = GATCON
data GRIDRN = GRIDRN
data HRBARE = HRBARE
data HRBFAC = HRBFAC
data HULKES = HULKES
data ICEARE = ICEARE
data ICNARE = ICNARE
data ISTZNE = ISTZNE
data LAKARE = LAKARE
data LNDARE = LNDARE
data LNDELV = LNDELV
data LNDRGN = LNDRGN
data LNDMRK = LNDMRK
data LIGHTS = LIGHTS
data LITFLT = LITFLT
data LITVES = LITVES
data LOCMAG = LOCMAG
data LOKBSN = LOKBSN
data LOGPON = LOGPON
data MAGVAR = MAGVAR
data MARCUL = MARCUL
data MIPARE = MIPARE
data MORFAC = MORFAC
data NAVLNE = NAVLNE
data OBSTRN = OBSTRN
data OFSPLF = OFSPLF
data OSPARE = OSPARE
data OILBAR = OILBAR
data PILPNT = PILPNT
data PILBOP = PILBOP
data PIPARE = PIPARE
data PIPOHD = PIPOHD
data PIPSOL = PIPSOL
data PONTON = PONTON
data PRCARE = PRCARE
data PRDARE = PRDARE
data PYLONS = PYLONS
data RADLNE = RADLNE
data RADRNG = RADRNG
data RADRFL = RADRFL
data RADSTA = RADSTA
data RTPBCN = RTPBCN
data RDOCAL = RDOCAL
data RDOSTA = RDOSTA
data RAILWY = RAILWY
data RAPIDS = RAPIDS
data RCRTCL = RCRTCL
data RECTRC = RECTRC
data RCTLPT = RCTLPT
data RSCSTA = RSCSTA
data RESARE = RESARE
data RETRFL = RETRFL
data RIVERS = RIVERS
data ROADWY = ROADWY
data RUNWAY = RUNWAY
data SNDWAV = SNDWAV
data SEAARE = SEAARE
data SPLARE = SPLARE
data SBDARE = SBDARE
data SLCONS = SLCONS
data SISTAT = SISTAT
data SISTAW = SISTAW
data SILTNK = SILTNK
data SLOTOP = SLOTOP
data SLOGRD = SLOGRD
data SMCFAC = SMCFAC
data SOUNDG = SOUNDG
data SPRING = SPRING
data STSLNE = STSLNE
data SUBTLN = SUBTLN
data SWPARE = SWPARE
data TESARE = TESARE
data TS_PRH = TS_PRH
data TS_PNH = TS_PNH
data TS_PAD = TS_PAD
data TS_TIS = TS_TIS
data T_HMON = T_HMON
data T_NHMN = T_NHMN
data T_TIMS = T_TIMS
data TIDEWY = TIDEWY
data TOPMAR = TOPMAR
data TSELNE = TSELNE
data TSSBND = TSSBND
data TSSCRS = TSSCRS
data TSSLPT = TSSLPT
data TSSRON = TSSRON
data TSEZNE = TSEZNE
data TUNNEL = TUNNEL
data TWRTPT = TWRTPT
data UWTROC = UWTROC
data UNSARE = UNSARE
data VEGATN = VEGATN
data WATTUR = WATTUR
data WATFAL = WATFAL
data WEDKLP = WEDKLP
data WRECKS = WRECKS
data M_ACCY = M_ACCY
data M_CSCL = M_CSCL
data M_COVR = M_COVR
data M_HDAT = M_HDAT
data M_HOPA = M_HOPA
data M_NPUB = M_NPUB
data M_NSYS = M_NSYS
data M_PROD = M_PROD
data M_QUAL = M_QUAL
data M_SDAT = M_SDAT
data M_SREL = M_SREL
data M_UNIT = M_UNIT
data M_VDAT = M_VDAT
data C_AGGR = C_AGGR
data C_ASSO = C_ASSO
data C_STAC = C_STAC
data AREAS = AREAS
data LINES = LINES
data CSYMB = CSYMB
data COMPS = COMPS
data TEXTS = TEXTS


instance Object ADMARE where
    type BaseType ADMARE = ADMARE
    obj_code _ = 1
    obj_fromCode _ = ADMARE
    obj_class _ = "Administration area (Named)"
    obj_attrA _ = [JRSDTN,NATION,NOBJNM,OBJNAM]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object AIRARE where
    type BaseType AIRARE = AIRARE
    obj_code _ = 2
    obj_fromCode _ = AIRARE
    obj_class _ = "Airport / airfield"
    obj_attrA _ = [CATAIR,CONDTN,CONVIS,NOBJNM,OBJNAM,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object ACHBRT where
    type BaseType ACHBRT = ACHBRT
    obj_code _ = 3
    obj_fromCode _ = ACHBRT
    obj_class _ = "Anchor berth"
    obj_attrA _ = [CATACH,DATEND,DATSTA,NOBJNM,OBJNAM,PEREND,PERSTA,RADIUS,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object ACHARE where
    type BaseType ACHARE = ACHARE
    obj_code _ = 4
    obj_fromCode _ = ACHARE
    obj_class _ = "Anchorage area"
    obj_attrA _ = [CATACH,DATEND,DATSTA,NOBJNM,OBJNAM,PEREND,PERSTA,RESTRN,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object BCNCAR where
    type BaseType BCNCAR = BCNCAR
    obj_code _ = 5
    obj_fromCode _ = BCNCAR
    obj_class _ = "Beacon cardinal"
    obj_attrA _ = [BCNSHP,CATCAM,COLOUR,COLPAT,CONDTN,CONVIS,CONRAD,DATEND,DATSTA,ELEVAT,HEIGHT,MARSYS,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object BCNISD where
    type BaseType BCNISD = BCNISD
    obj_code _ = 6
    obj_fromCode _ = BCNISD
    obj_class _ = "Beacon isolated danger"
    obj_attrA _ = [BCNSHP,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,ELEVAT,HEIGHT,MARSYS,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object BCNLAT where
    type BaseType BCNLAT = BCNLAT
    obj_code _ = 7
    obj_fromCode _ = BCNLAT
    obj_class _ = "Beacon lateral"
    obj_attrA _ = [BCNSHP,CATLAM,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,ELEVAT,HEIGHT,MARSYS,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object BCNSAW where
    type BaseType BCNSAW = BCNSAW
    obj_code _ = 8
    obj_fromCode _ = BCNSAW
    obj_class _ = "Beacon safe water"
    obj_attrA _ = [BCNSHP,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,ELEVAT,HEIGHT,MARSYS,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object BCNSPP where
    type BaseType BCNSPP = BCNSPP
    obj_code _ = 9
    obj_fromCode _ = BCNSPP
    obj_class _ = "Beacon special purpose/general"
    obj_attrA _ = [BCNSHP,CATSPM,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,ELEVAT,HEIGHT,MARSYS,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object BERTHS where
    type BaseType BERTHS = BERTHS
    obj_code _ = 10
    obj_fromCode _ = BERTHS
    obj_class _ = "Berth"
    obj_attrA _ = [DATEND,DATSTA,DRVAL1,NOBJNM,OBJNAM,PEREND,PERSTA,QUASOU,SOUACC,STATUS,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object BRIDGE where
    type BaseType BRIDGE = BRIDGE
    obj_code _ = 11
    obj_fromCode _ = BRIDGE
    obj_class _ = "Bridge"
    obj_attrA _ = [CATBRG,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,HORACC,HORCLR,NATCON,NOBJNM,OBJNAM,VERACC,VERCCL,VERCLR,VERCOP,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object BUISGL where
    type BaseType BUISGL = BUISGL
    obj_code _ = 12
    obj_fromCode _ = BUISGL
    obj_class _ = "Building single"
    obj_attrA _ = [BUISHP,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,ELEVAT,FUNCTN,HEIGHT,NATCON,NOBJNM,OBJNAM,STATUS,VERACC,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object BUAARE where
    type BaseType BUAARE = BUAARE
    obj_code _ = 13
    obj_fromCode _ = BUAARE
    obj_class _ = "Built-up area"
    obj_attrA _ = [CATBUA,CONDTN,CONRAD,CONVIS,HEIGHT,NOBJNM,OBJNAM,VERACC,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object BOYCAR where
    type BaseType BOYCAR = BOYCAR
    obj_code _ = 14
    obj_fromCode _ = BOYCAR
    obj_class _ = "Buoy cardinal"
    obj_attrA _ = [BOYSHP,CATCAM,COLOUR,COLPAT,CONRAD,DATEND,DATSTA,MARSYS,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object BOYINB where
    type BaseType BOYINB = BOYINB
    obj_code _ = 15
    obj_fromCode _ = BOYINB
    obj_class _ = "Buoy installation"
    obj_attrA _ = [BOYSHP,CATINB,COLOUR,COLPAT,CONRAD,DATEND,DATSTA,MARSYS,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,PRODCT,STATUS,VERACC,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object BOYISD where
    type BaseType BOYISD = BOYISD
    obj_code _ = 16
    obj_fromCode _ = BOYISD
    obj_class _ = "Buoy isolated danger"
    obj_attrA _ = [BOYSHP,COLOUR,COLPAT,CONRAD,DATEND,DATSTA,MARSYS,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object BOYLAT where
    type BaseType BOYLAT = BOYLAT
    obj_code _ = 17
    obj_fromCode _ = BOYLAT
    obj_class _ = "Buoy lateral"
    obj_attrA _ = [BOYSHP,CATLAM,COLOUR,COLPAT,CONRAD,DATEND,DATSTA,MARSYS,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object BOYSAW where
    type BaseType BOYSAW = BOYSAW
    obj_code _ = 18
    obj_fromCode _ = BOYSAW
    obj_class _ = "Buoy safe water"
    obj_attrA _ = [BOYSHP,COLOUR,COLPAT,CONRAD,DATEND,DATSTA,MARSYS,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object BOYSPP where
    type BaseType BOYSPP = BOYSPP
    obj_code _ = 19
    obj_fromCode _ = BOYSPP
    obj_class _ = "Buoy special purpose/general"
    obj_attrA _ = [BOYSHP,CATSPM,COLOUR,COLPAT,CONRAD,DATEND,DATSTA,MARSYS,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object CBLARE where
    type BaseType CBLARE = CBLARE
    obj_code _ = 20
    obj_fromCode _ = CBLARE
    obj_class _ = "Cable area"
    obj_attrA _ = [CATCBL,DATEND,DATSTA,NOBJNM,OBJNAM,RESTRN,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object CBLOHD where
    type BaseType CBLOHD = CBLOHD
    obj_code _ = 21
    obj_fromCode _ = CBLOHD
    obj_class _ = "Cable overhead"
    obj_attrA _ = [CATCBL,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,ICEFAC,NOBJNM,OBJNAM,STATUS,VERACC,VERCLR,VERCSA,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line]

instance Object CBLSUB where
    type BaseType CBLSUB = CBLSUB
    obj_code _ = 22
    obj_fromCode _ = CBLSUB
    obj_class _ = "Cable submarine"
    obj_attrA _ = [BURDEP,CATCBL,CONDTN,DATEND,DATSTA,NOBJNM,OBJNAM,STATUS,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line]

instance Object CANALS where
    type BaseType CANALS = CANALS
    obj_code _ = 23
    obj_fromCode _ = CANALS
    obj_class _ = "Canal"
    obj_attrA _ = [CATCAN,CONDTN,DATEND,DATSTA,HORACC,HORCLR,HORWID,NOBJNM,OBJNAM,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line,Area]

instance Object CTSARE where
    type BaseType CTSARE = CTSARE
    obj_code _ = 25
    obj_fromCode _ = CTSARE
    obj_class _ = "Cargo transshipment area"
    obj_attrA _ = [DATEND,DATSTA,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object CAUSWY where
    type BaseType CAUSWY = CAUSWY
    obj_code _ = 26
    obj_fromCode _ = CAUSWY
    obj_class _ = "Causeway"
    obj_attrA _ = [CONDTN,NATCON,NOBJNM,OBJNAM,STATUS,WATLEV]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line,Area]

instance Object CTNARE where
    type BaseType CTNARE = CTNARE
    obj_code _ = 27
    obj_fromCode _ = CTNARE
    obj_class _ = "Caution area"
    obj_attrA _ = [DATEND,DATSTA,PEREND,PERSTA]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object CHKPNT where
    type BaseType CHKPNT = CHKPNT
    obj_code _ = 28
    obj_fromCode _ = CHKPNT
    obj_class _ = "Checkpoint"
    obj_attrA _ = [CATCHP,NOBJNM,OBJNAM,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object CGUSTA where
    type BaseType CGUSTA = CGUSTA
    obj_code _ = 29
    obj_fromCode _ = CGUSTA
    obj_class _ = "Coastguard station"
    obj_attrA _ = [DATEND,DATSTA,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object COALNE where
    type BaseType COALNE = COALNE
    obj_code _ = 30
    obj_fromCode _ = COALNE
    obj_class _ = "Coastline"
    obj_attrA _ = [CATCOA,COLOUR,CONRAD,CONVIS,ELEVAT,NOBJNM,OBJNAM,VERACC,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line]

instance Object CONZNE where
    type BaseType CONZNE = CONZNE
    obj_code _ = 31
    obj_fromCode _ = CONZNE
    obj_class _ = "Contiguous zone"
    obj_attrA _ = [DATEND,DATSTA,NATION,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object COSARE where
    type BaseType COSARE = COSARE
    obj_code _ = 32
    obj_fromCode _ = COSARE
    obj_class _ = "Continental shelf area"
    obj_attrA _ = [NATION,NOBJNM,OBJNAM]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object CTRPNT where
    type BaseType CTRPNT = CTRPNT
    obj_code _ = 33
    obj_fromCode _ = CTRPNT
    obj_class _ = "Control point"
    obj_attrA _ = [CATCTR,DATEND,DATSTA,ELEVAT,NOBJNM,OBJNAM,VERACC,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object CONVYR where
    type BaseType CONVYR = CONVYR
    obj_code _ = 34
    obj_fromCode _ = CONVYR
    obj_class _ = "Conveyor"
    obj_attrA _ = [CATCON,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,HEIGHT,LIFCAP,NOBJNM,OBJNAM,PRODCT,STATUS,VERACC,VERCLR,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line,Area]

instance Object CRANES where
    type BaseType CRANES = CRANES
    obj_code _ = 35
    obj_fromCode _ = CRANES
    obj_class _ = "Crane"
    obj_attrA _ = [CATCRN,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,HEIGHT,LIFCAP,NOBJNM,OBJNAM,ORIENT,RADIUS,STATUS,VERACC,VERCLR,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object CURENT where
    type BaseType CURENT = CURENT
    obj_code _ = 36
    obj_fromCode _ = CURENT
    obj_class _ = "Current - non - gravitational"
    obj_attrA _ = [CURVEL,DATEND,DATSTA,NOBJNM,OBJNAM,ORIENT,PEREND,PERSTA]
    obj_attrB _ = [INFORM,NINFOM,SCAMAX,SCAMIN]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object CUSZNE where
    type BaseType CUSZNE = CUSZNE
    obj_code _ = 37
    obj_fromCode _ = CUSZNE
    obj_class _ = "Custom zone"
    obj_attrA _ = [NATION]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object DAMCON where
    type BaseType DAMCON = DAMCON
    obj_code _ = 38
    obj_fromCode _ = DAMCON
    obj_class _ = "Dam"
    obj_attrA _ = [CATDAM,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,HEIGHT,NATCON,NOBJNM,OBJNAM,VERACC,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object DAYMAR where
    type BaseType DAYMAR = DAYMAR
    obj_code _ = 39
    obj_fromCode _ = DAYMAR
    obj_class _ = "Daymark"
    obj_attrA _ = [CATSPM,COLOUR,COLPAT,DATEND,DATSTA,ELEVAT,HEIGHT,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,TOPSHP,VERACC,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object DWRTCL where
    type BaseType DWRTCL = DWRTCL
    obj_code _ = 40
    obj_fromCode _ = DWRTCL
    obj_class _ = "Deep water route centerline"
    obj_attrA _ = [CATTRK,DATEND,DATSTA,DRVAL1,DRVAL2,NOBJNM,OBJNAM,ORIENT,QUASOU,SOUACC,STATUS,TECSOU,TRAFIC,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line]

instance Object DWRTPT where
    type BaseType DWRTPT = DWRTPT
    obj_code _ = 41
    obj_fromCode _ = DWRTPT
    obj_class _ = "Deep water route part"
    obj_attrA _ = [DATEND,DATSTA,DRVAL1,DRVAL2,NOBJNM,OBJNAM,ORIENT,QUASOU,SOUACC,STATUS,TECSOU,TRAFIC,VERDAT,RESTRN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object DEPARE where
    type BaseType DEPARE = DEPARE
    obj_code _ = 42
    obj_fromCode _ = DEPARE
    obj_class _ = "Depth area"
    obj_attrA _ = [DRVAL1,DRVAL2,QUASOU,SOUACC,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line,Area]

instance Object DEPCNT where
    type BaseType DEPCNT = DEPCNT
    obj_code _ = 43
    obj_fromCode _ = DEPCNT
    obj_class _ = "Depth contour"
    obj_attrA _ = [VALDCO,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line]

instance Object DISMAR where
    type BaseType DISMAR = DISMAR
    obj_code _ = 44
    obj_fromCode _ = DISMAR
    obj_class _ = "Distance mark"
    obj_attrA _ = [CATDIS,DATEND,DATSTA,NOBJNM,OBJNAM]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object DOCARE where
    type BaseType DOCARE = DOCARE
    obj_code _ = 45
    obj_fromCode _ = DOCARE
    obj_class _ = "Dock area"
    obj_attrA _ = [CATDOC,CONDTN,DATEND,DATSTA,HORACC,HORCLR,NOBJNM,OBJNAM,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object DRGARE where
    type BaseType DRGARE = DRGARE
    obj_code _ = 46
    obj_fromCode _ = DRGARE
    obj_class _ = "Dredged area"
    obj_attrA _ = [DRVAL1,DRVAL2,NOBJNM,OBJNAM,QUASOU,RESTRN,SOUACC,TECSOU,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object DRYDOC where
    type BaseType DRYDOC = DRYDOC
    obj_code _ = 47
    obj_fromCode _ = DRYDOC
    obj_class _ = "Dry dock"
    obj_attrA _ = [CONDTN,HORACC,HORCLR,HORLEN,HORWID,NOBJNM,OBJNAM,STATUS,DRVAL1,QUASOU,SOUACC,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,SCAMAX,SCAMIN]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object DMPGRD where
    type BaseType DMPGRD = DMPGRD
    obj_code _ = 48
    obj_fromCode _ = DMPGRD
    obj_class _ = "Dumping ground"
    obj_attrA _ = [CATDPG,NOBJNM,OBJNAM,RESTRN,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object DYKCON where
    type BaseType DYKCON = DYKCON
    obj_code _ = 49
    obj_fromCode _ = DYKCON
    obj_class _ = "Dyke"
    obj_attrA _ = [CONDTN,CONRAD,DATEND,DATSTA,HEIGHT,NATCON,VERACC,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line,Area]

instance Object EXEZNE where
    type BaseType EXEZNE = EXEZNE
    obj_code _ = 50
    obj_fromCode _ = EXEZNE
    obj_class _ = "Exclusive Economic Zone"
    obj_attrA _ = [NATION]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object FAIRWY where
    type BaseType FAIRWY = FAIRWY
    obj_code _ = 51
    obj_fromCode _ = FAIRWY
    obj_class _ = "Fairway"
    obj_attrA _ = [DATEND,DATSTA,DRVAL1,NOBJNM,OBJNAM,ORIENT,QUASOU,RESTRN,SOUACC,STATUS,TRAFIC,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object FNCLNE where
    type BaseType FNCLNE = FNCLNE
    obj_code _ = 52
    obj_fromCode _ = FNCLNE
    obj_class _ = "Fence/wall"
    obj_attrA _ = [CATFNC,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,ELEVAT,HEIGHT,NATCON,NOBJNM,OBJNAM,STATUS,VERACC,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line]

instance Object FERYRT where
    type BaseType FERYRT = FERYRT
    obj_code _ = 53
    obj_fromCode _ = FERYRT
    obj_class _ = "Ferry route"
    obj_attrA _ = [CATFRY,DATEND,DATSTA,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line,Area]

instance Object FSHZNE where
    type BaseType FSHZNE = FSHZNE
    obj_code _ = 54
    obj_fromCode _ = FSHZNE
    obj_class _ = "Fishery zone"
    obj_attrA _ = [NATION,NOBJNM,OBJNAM,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object FSHFAC where
    type BaseType FSHFAC = FSHFAC
    obj_code _ = 55
    obj_fromCode _ = FSHFAC
    obj_class _ = "Fishing facility"
    obj_attrA _ = [CATFIF,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object FSHGRD where
    type BaseType FSHGRD = FSHGRD
    obj_code _ = 56
    obj_fromCode _ = FSHGRD
    obj_class _ = "Fishing ground"
    obj_attrA _ = [NOBJNM,OBJNAM,PEREND,PERSTA,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object FLODOC where
    type BaseType FLODOC = FLODOC
    obj_code _ = 57
    obj_fromCode _ = FLODOC
    obj_class _ = "Floating dock"
    obj_attrA _ = [COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,DRVAL1,HORACC,HORCLR,HORLEN,HORWID,LIFCAP,NOBJNM,OBJNAM,STATUS,VERACC,VERLEN,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line,Area]

instance Object FOGSIG where
    type BaseType FOGSIG = FOGSIG
    obj_code _ = 58
    obj_fromCode _ = FOGSIG
    obj_class _ = "Fog signal"
    obj_attrA _ = [CATFOG,DATEND,DATSTA,NOBJNM,OBJNAM,SIGFRQ,SIGGEN,SIGGRP,SIGPER,SIGSEQ,STATUS,VALMXR]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object FORSTC where
    type BaseType FORSTC = FORSTC
    obj_code _ = 59
    obj_fromCode _ = FORSTC
    obj_class _ = "Fortified structure"
    obj_attrA _ = [CATFOR,CONDTN,CONRAD,CONVIS,HEIGHT,NATCON,NOBJNM,OBJNAM,VERACC,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object FRPARE where
    type BaseType FRPARE = FRPARE
    obj_code _ = 60
    obj_fromCode _ = FRPARE
    obj_class _ = "Free port area"
    obj_attrA _ = [NOBJNM,OBJNAM,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object GATCON where
    type BaseType GATCON = GATCON
    obj_code _ = 61
    obj_fromCode _ = GATCON
    obj_class _ = "Gate"
    obj_attrA _ = [CATGAT,CONDTN,DRVAL1,HORACC,HORCLR,NATCON,NOBJNM,OBJNAM,QUASOU,SOUACC,STATUS,VERACC,VERCLR,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object GRIDRN where
    type BaseType GRIDRN = GRIDRN
    obj_code _ = 62
    obj_fromCode _ = GRIDRN
    obj_class _ = "Gridiron"
    obj_attrA _ = [HORACC,HORLEN,HORWID,NATCON,NOBJNM,OBJNAM,STATUS,VERACC,VERLEN,WATLEV]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object HRBARE where
    type BaseType HRBARE = HRBARE
    obj_code _ = 63
    obj_fromCode _ = HRBARE
    obj_class _ = "Harbour area (administrative)"
    obj_attrA _ = [NOBJNM,OBJNAM,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object HRBFAC where
    type BaseType HRBFAC = HRBFAC
    obj_code _ = 64
    obj_fromCode _ = HRBFAC
    obj_class _ = "Harbour facility"
    obj_attrA _ = [CATHAF,CONDTN,DATEND,DATSTA,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object HULKES where
    type BaseType HULKES = HULKES
    obj_code _ = 65
    obj_fromCode _ = HULKES
    obj_class _ = "Hulk"
    obj_attrA _ = [CATHLK,COLOUR,COLPAT,CONRAD,CONVIS,HORACC,HORLEN,HORWID,NOBJNM,OBJNAM,VERACC,VERLEN,CONDTN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object ICEARE where
    type BaseType ICEARE = ICEARE
    obj_code _ = 66
    obj_fromCode _ = ICEARE
    obj_class _ = "Ice area"
    obj_attrA _ = [CATICE,CONVIS,ELEVAT,HEIGHT,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object ICNARE where
    type BaseType ICNARE = ICNARE
    obj_code _ = 67
    obj_fromCode _ = ICNARE
    obj_class _ = "Incineration area"
    obj_attrA _ = [NOBJNM,OBJNAM,PEREND,PERSTA,RESTRN,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object ISTZNE where
    type BaseType ISTZNE = ISTZNE
    obj_code _ = 68
    obj_fromCode _ = ISTZNE
    obj_class _ = "Inshore traffic zone"
    obj_attrA _ = [CATTSS,DATEND,DATSTA,RESTRN,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object LAKARE where
    type BaseType LAKARE = LAKARE
    obj_code _ = 69
    obj_fromCode _ = LAKARE
    obj_class _ = "Lake"
    obj_attrA _ = [ELEVAT,NOBJNM,OBJNAM,VERACC,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object LNDARE where
    type BaseType LNDARE = LNDARE
    obj_code _ = 71
    obj_fromCode _ = LNDARE
    obj_class _ = "Land area"
    obj_attrA _ = [CONDTN,NOBJNM,OBJNAM,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object LNDELV where
    type BaseType LNDELV = LNDELV
    obj_code _ = 72
    obj_fromCode _ = LNDELV
    obj_class _ = "Land elevation"
    obj_attrA _ = [CONVIS,ELEVAT,NOBJNM,OBJNAM,VERACC,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line]

instance Object LNDRGN where
    type BaseType LNDRGN = LNDRGN
    obj_code _ = 73
    obj_fromCode _ = LNDRGN
    obj_class _ = "Land region"
    obj_attrA _ = [CATLND,NATQUA,NATSUR,NOBJNM,OBJNAM,WATLEV]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object LNDMRK where
    type BaseType LNDMRK = LNDMRK
    obj_code _ = 74
    obj_fromCode _ = LNDMRK
    obj_class _ = "Landmark"
    obj_attrA _ = [CATLMK,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,ELEVAT,FUNCTN,HEIGHT,NATCON,NOBJNM,OBJNAM,STATUS,VERACC,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object LIGHTS where
    type BaseType LIGHTS = LIGHTS
    obj_code _ = 75
    obj_fromCode _ = LIGHTS
    obj_class _ = "Light"
    obj_attrA _ = [CATLIT,COLOUR,DATEND,DATSTA,EXCLIT,HEIGHT,LITCHR,LITVIS,MARSYS,MLTYLT,NOBJNM,OBJNAM,ORIENT,PEREND,PERSTA,SECTR1,SECTR2,SIGGRP,SIGPER,SIGSEQ,STATUS,VERACC,VALNMR,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object LITFLT where
    type BaseType LITFLT = LITFLT
    obj_code _ = 76
    obj_fromCode _ = LITFLT
    obj_class _ = "Light float"
    obj_attrA _ = [COLOUR,COLPAT,CONRAD,CONVIS,DATEND,DATSTA,HORACC,HORLEN,HORWID,MARSYS,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object LITVES where
    type BaseType LITVES = LITVES
    obj_code _ = 77
    obj_fromCode _ = LITVES
    obj_class _ = "Light vessel"
    obj_attrA _ = [COLOUR,COLPAT,CONRAD,CONVIS,DATEND,DATSTA,HORACC,HORLEN,HORWID,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object LOCMAG where
    type BaseType LOCMAG = LOCMAG
    obj_code _ = 78
    obj_fromCode _ = LOCMAG
    obj_class _ = "Local magnetic anomaly"
    obj_attrA _ = [NOBJNM,OBJNAM,VALLMA]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object LOKBSN where
    type BaseType LOKBSN = LOKBSN
    obj_code _ = 79
    obj_fromCode _ = LOKBSN
    obj_class _ = "Lock basin"
    obj_attrA _ = [DATEND,DATSTA,HORACC,HORCLR,HORLEN,HORWID,NOBJNM,OBJNAM,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object LOGPON where
    type BaseType LOGPON = LOGPON
    obj_code _ = 80
    obj_fromCode _ = LOGPON
    obj_class _ = "Log pond"
    obj_attrA _ = [NOBJNM,OBJNAM,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object MAGVAR where
    type BaseType MAGVAR = MAGVAR
    obj_code _ = 81
    obj_fromCode _ = MAGVAR
    obj_class _ = "Magnetic variation"
    obj_attrA _ = [DATEND,DATSTA,RYRMGV,VALACM,VALMAG]
    obj_attrB _ = [INFORM,NINFOM,SCAMAX,SCAMIN]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object MARCUL where
    type BaseType MARCUL = MARCUL
    obj_code _ = 82
    obj_fromCode _ = MARCUL
    obj_class _ = "Marine farm/culture"
    obj_attrA _ = [CATMFA,DATEND,DATSTA,EXPSOU,NOBJNM,OBJNAM,PEREND,PERSTA,QUASOU,RESTRN,SOUACC,STATUS,VALSOU,VERACC,VERDAT,VERLEN,WATLEV]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object MIPARE where
    type BaseType MIPARE = MIPARE
    obj_code _ = 83
    obj_fromCode _ = MIPARE
    obj_class _ = "Military practice area"
    obj_attrA _ = [CATMPA,DATEND,DATSTA,NOBJNM,OBJNAM,PEREND,PERSTA,RESTRN,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object MORFAC where
    type BaseType MORFAC = MORFAC
    obj_code _ = 84
    obj_fromCode _ = MORFAC
    obj_class _ = "Mooring/warping facility"
    obj_attrA _ = [BOYSHP,CATMOR,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,HEIGHT,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERDAT,VERLEN,WATLEV]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object NAVLNE where
    type BaseType NAVLNE = NAVLNE
    obj_code _ = 85
    obj_fromCode _ = NAVLNE
    obj_class _ = "Navigation line"
    obj_attrA _ = [CATNAV,DATEND,DATSTA,ORIENT,PEREND,PERSTA,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line]

instance Object OBSTRN where
    type BaseType OBSTRN = OBSTRN
    obj_code _ = 86
    obj_fromCode _ = OBSTRN
    obj_class _ = "Obstruction"
    obj_attrA _ = [CATOBS,CONDTN,EXPSOU,HEIGHT,NATCON,NATQUA,NOBJNM,OBJNAM,PRODCT,QUASOU,SOUACC,STATUS,TECSOU,VALSOU,VERACC,VERDAT,VERLEN,WATLEV,NATSUR]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object OFSPLF where
    type BaseType OFSPLF = OFSPLF
    obj_code _ = 87
    obj_fromCode _ = OFSPLF
    obj_class _ = "Offshore platform"
    obj_attrA _ = [CATOFP,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,HEIGHT,NATCON,NOBJNM,OBJNAM,PRODCT,STATUS,VERACC,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object OSPARE where
    type BaseType OSPARE = OSPARE
    obj_code _ = 88
    obj_fromCode _ = OSPARE
    obj_class _ = "Offshore production area"
    obj_attrA _ = [CATPRA,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,HEIGHT,NOBJNM,OBJNAM,PRODCT,RESTRN,STATUS,VERACC,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object OILBAR where
    type BaseType OILBAR = OILBAR
    obj_code _ = 89
    obj_fromCode _ = OILBAR
    obj_class _ = "Oil barrier"
    obj_attrA _ = [CATOLB,CONDTN,DATEND,DATSTA,NOBJNM,OBJNAM,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line]

instance Object PILPNT where
    type BaseType PILPNT = PILPNT
    obj_code _ = 90
    obj_fromCode _ = PILPNT
    obj_class _ = "Pile"
    obj_attrA _ = [CATPLE,COLOUR,COLPAT,CONDTN,CONVIS,DATEND,DATSTA,HEIGHT,NOBJNM,OBJNAM,VERACC,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object PILBOP where
    type BaseType PILBOP = PILBOP
    obj_code _ = 91
    obj_fromCode _ = PILBOP
    obj_class _ = "Pilot boarding place"
    obj_attrA _ = [CATPIL,COMCHA,DATEND,DATSTA,NOBJNM,NPLDST,OBJNAM,PEREND,PERSTA,PILDST,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object PIPARE where
    type BaseType PIPARE = PIPARE
    obj_code _ = 92
    obj_fromCode _ = PIPARE
    obj_class _ = "Pipeline area"
    obj_attrA _ = [CONDTN,DATEND,DATSTA,NOBJNM,OBJNAM,PRODCT,RESTRN,STATUS,CATPIP]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object PIPOHD where
    type BaseType PIPOHD = PIPOHD
    obj_code _ = 93
    obj_fromCode _ = PIPOHD
    obj_class _ = "Pipeline overhead"
    obj_attrA _ = [CATPIP,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,NOBJNM,OBJNAM,PRODCT,STATUS,VERACC,VERCLR,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line]

instance Object PIPSOL where
    type BaseType PIPSOL = PIPSOL
    obj_code _ = 94
    obj_fromCode _ = PIPSOL
    obj_class _ = "Pipeline submarine/on land"
    obj_attrA _ = [BURDEP,CATPIP,CONDTN,DATEND,DATSTA,DRVAL1,DRVAL2,NOBJNM,OBJNAM,PRODCT,STATUS,VERACC,VERLEN,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line]

instance Object PONTON where
    type BaseType PONTON = PONTON
    obj_code _ = 95
    obj_fromCode _ = PONTON
    obj_class _ = "Pontoon"
    obj_attrA _ = [CONDTN,CONRAD,CONVIS,DATEND,DATSTA,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line,Area]

instance Object PRCARE where
    type BaseType PRCARE = PRCARE
    obj_code _ = 96
    obj_fromCode _ = PRCARE
    obj_class _ = "Precautionary area"
    obj_attrA _ = [DATEND,DATSTA,RESTRN,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object PRDARE where
    type BaseType PRDARE = PRDARE
    obj_code _ = 97
    obj_fromCode _ = PRDARE
    obj_class _ = "Production / storage area"
    obj_attrA _ = [CATPRA,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,ELEVAT,HEIGHT,NOBJNM,OBJNAM,PRODCT,STATUS,VERACC,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object PYLONS where
    type BaseType PYLONS = PYLONS
    obj_code _ = 98
    obj_fromCode _ = PYLONS
    obj_class _ = "Pylon/bridge support"
    obj_attrA _ = [CATPYL,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,HEIGHT,NATCON,NOBJNM,OBJNAM,VERACC,VERDAT,VERLEN,WATLEV]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object RADLNE where
    type BaseType RADLNE = RADLNE
    obj_code _ = 99
    obj_fromCode _ = RADLNE
    obj_class _ = "Radar line"
    obj_attrA _ = [NOBJNM,OBJNAM,ORIENT,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line]

instance Object RADRNG where
    type BaseType RADRNG = RADRNG
    obj_code _ = 100
    obj_fromCode _ = RADRNG
    obj_class _ = "Radar range"
    obj_attrA _ = [COMCHA,DATEND,DATSTA,NOBJNM,OBJNAM,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object RADRFL where
    type BaseType RADRFL = RADRFL
    obj_code _ = 101
    obj_fromCode _ = RADRFL
    obj_class _ = "Radar reflector"
    obj_attrA _ = [HEIGHT,STATUS,VERACC,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object RADSTA where
    type BaseType RADSTA = RADSTA
    obj_code _ = 102
    obj_fromCode _ = RADSTA
    obj_class _ = "Radar station"
    obj_attrA _ = [CATRAS,DATEND,DATSTA,HEIGHT,NOBJNM,OBJNAM,STATUS,VERACC,VALMXR,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object RTPBCN where
    type BaseType RTPBCN = RTPBCN
    obj_code _ = 103
    obj_fromCode _ = RTPBCN
    obj_class _ = "Radar transponder beacon"
    obj_attrA _ = [CATRTB,DATEND,DATSTA,NOBJNM,OBJNAM,RADWAL,SECTR1,SECTR2,SIGGRP,SIGSEQ,STATUS,VALMXR]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object RDOCAL where
    type BaseType RDOCAL = RDOCAL
    obj_code _ = 104
    obj_fromCode _ = RDOCAL
    obj_class _ = "Radio calling-in point"
    obj_attrA _ = [COMCHA,DATEND,DATSTA,NOBJNM,OBJNAM,ORIENT,PEREND,PERSTA,STATUS,TRAFIC]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line]

instance Object RDOSTA where
    type BaseType RDOSTA = RDOSTA
    obj_code _ = 105
    obj_fromCode _ = RDOSTA
    obj_class _ = "Radio station"
    obj_attrA _ = [CALSGN,CATROS,COMCHA,DATEND,DATSTA,ESTRNG,NOBJNM,OBJNAM,ORIENT,PEREND,PERSTA,SIGFRQ,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object RAILWY where
    type BaseType RAILWY = RAILWY
    obj_code _ = 106
    obj_fromCode _ = RAILWY
    obj_class _ = "Railway"
    obj_attrA _ = [CONDTN,HEIGHT,NOBJNM,OBJNAM,STATUS,VERACC]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line]

instance Object RAPIDS where
    type BaseType RAPIDS = RAPIDS
    obj_code _ = 107
    obj_fromCode _ = RAPIDS
    obj_class _ = "Rapids"
    obj_attrA _ = [NOBJNM,OBJNAM,VERACC,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object RCRTCL where
    type BaseType RCRTCL = RCRTCL
    obj_code _ = 108
    obj_fromCode _ = RCRTCL
    obj_class _ = "Recommended route centerline"
    obj_attrA _ = [CATTRK,DATEND,DATSTA,DRVAL1,DRVAL2,NOBJNM,OBJNAM,ORIENT,PEREND,PERSTA,QUASOU,SOUACC,STATUS,TECSOU,TRAFIC,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line]

instance Object RECTRC where
    type BaseType RECTRC = RECTRC
    obj_code _ = 109
    obj_fromCode _ = RECTRC
    obj_class _ = "Recommended track"
    obj_attrA _ = [CATTRK,DATEND,DATSTA,DRVAL1,DRVAL2,NOBJNM,OBJNAM,ORIENT,PEREND,PERSTA,QUASOU,SOUACC,STATUS,TECSOU,TRAFIC,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line,Area]

instance Object RCTLPT where
    type BaseType RCTLPT = RCTLPT
    obj_code _ = 110
    obj_fromCode _ = RCTLPT
    obj_class _ = "Recommended Traffic Lane Part"
    obj_attrA _ = [DATEND,DATSTA,ORIENT,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object RSCSTA where
    type BaseType RSCSTA = RSCSTA
    obj_code _ = 111
    obj_fromCode _ = RSCSTA
    obj_class _ = "Rescue station"
    obj_attrA _ = [CATRSC,DATEND,DATSTA,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS]
    obj_attrB _ = [INFORM,NINFOM,SCAMAX,SCAMIN]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object RESARE where
    type BaseType RESARE = RESARE
    obj_code _ = 112
    obj_fromCode _ = RESARE
    obj_class _ = "Restricted area"
    obj_attrA _ = [CATREA,DATEND,DATSTA,NOBJNM,OBJNAM,PEREND,PERSTA,RESTRN,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object RETRFL where
    type BaseType RETRFL = RETRFL
    obj_code _ = 113
    obj_fromCode _ = RETRFL
    obj_class _ = "Retro-reflector"
    obj_attrA _ = [COLOUR,COLPAT,HEIGHT,MARSYS,STATUS,VERACC,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object RIVERS where
    type BaseType RIVERS = RIVERS
    obj_code _ = 114
    obj_fromCode _ = RIVERS
    obj_class _ = "River"
    obj_attrA _ = [NOBJNM,OBJNAM,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line,Area]

instance Object ROADWY where
    type BaseType ROADWY = ROADWY
    obj_code _ = 116
    obj_fromCode _ = ROADWY
    obj_class _ = "Road"
    obj_attrA _ = [CATROD,CONDTN,NATCON,NOBJNM,OBJNAM,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object RUNWAY where
    type BaseType RUNWAY = RUNWAY
    obj_code _ = 117
    obj_fromCode _ = RUNWAY
    obj_class _ = "Runway"
    obj_attrA _ = [CATRUN,CONDTN,CONVIS,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object SNDWAV where
    type BaseType SNDWAV = SNDWAV
    obj_code _ = 118
    obj_fromCode _ = SNDWAV
    obj_class _ = "Sand waves"
    obj_attrA _ = [VERACC,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object SEAARE where
    type BaseType SEAARE = SEAARE
    obj_code _ = 119
    obj_fromCode _ = SEAARE
    obj_class _ = "Sea area / named water area"
    obj_attrA _ = [CATSEA,NOBJNM,OBJNAM]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object SPLARE where
    type BaseType SPLARE = SPLARE
    obj_code _ = 120
    obj_fromCode _ = SPLARE
    obj_class _ = "Sea-plane landing area"
    obj_attrA _ = [NOBJNM,OBJNAM,PEREND,PERSTA,RESTRN,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC,VALDCO]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object SBDARE where
    type BaseType SBDARE = SBDARE
    obj_code _ = 121
    obj_fromCode _ = SBDARE
    obj_class _ = "Seabed area"
    obj_attrA _ = [COLOUR,NATQUA,NATSUR,WATLEV,OBJNAM,NOBJNM]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object SLCONS where
    type BaseType SLCONS = SLCONS
    obj_code _ = 122
    obj_fromCode _ = SLCONS
    obj_class _ = "Shoreline Construction"
    obj_attrA _ = [CATSLC,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,HEIGHT,HORACC,HORCLR,HORLEN,HORWID,NATCON,NOBJNM,OBJNAM,STATUS,VERACC,VERDAT,VERLEN,WATLEV]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object SISTAT where
    type BaseType SISTAT = SISTAT
    obj_code _ = 123
    obj_fromCode _ = SISTAT
    obj_class _ = "Signal station traffic"
    obj_attrA _ = [CATSIT,COMCHA,DATEND,DATSTA,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object SISTAW where
    type BaseType SISTAW = SISTAW
    obj_code _ = 124
    obj_fromCode _ = SISTAW
    obj_class _ = "Signal station warning"
    obj_attrA _ = [CATSIW,COMCHA,DATEND,DATSTA,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object SILTNK where
    type BaseType SILTNK = SILTNK
    obj_code _ = 125
    obj_fromCode _ = SILTNK
    obj_class _ = "Silo / tank"
    obj_attrA _ = [BUISHP,CATSIL,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,ELEVAT,HEIGHT,NATCON,NOBJNM,OBJNAM,PRODCT,STATUS,VERACC,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object SLOTOP where
    type BaseType SLOTOP = SLOTOP
    obj_code _ = 126
    obj_fromCode _ = SLOTOP
    obj_class _ = "Slope topline"
    obj_attrA _ = [CATSLO,COLOUR,CONRAD,CONVIS,ELEVAT,NATCON,NATQUA,NATSUR,NOBJNM,OBJNAM,VERACC,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line]

instance Object SLOGRD where
    type BaseType SLOGRD = SLOGRD
    obj_code _ = 127
    obj_fromCode _ = SLOGRD
    obj_class _ = "Sloping ground"
    obj_attrA _ = [CATSLO,COLOUR,CONRAD,CONVIS,NATCON,NATQUA,NATSUR,NOBJNM,OBJNAM]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object SMCFAC where
    type BaseType SMCFAC = SMCFAC
    obj_code _ = 128
    obj_fromCode _ = SMCFAC
    obj_class _ = "Small craft facility"
    obj_attrA _ = [CATSCF,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object SOUNDG where
    type BaseType SOUNDG = SOUNDG
    obj_code _ = 129
    obj_fromCode _ = SOUNDG
    obj_class _ = "Sounding"
    obj_attrA _ = [EXPSOU,NOBJNM,OBJNAM,QUASOU,SOUACC,TECSOU,VERDAT,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object SPRING where
    type BaseType SPRING = SPRING
    obj_code _ = 130
    obj_fromCode _ = SPRING
    obj_class _ = "Spring"
    obj_attrA _ = [NOBJNM,OBJNAM]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object STSLNE where
    type BaseType STSLNE = STSLNE
    obj_code _ = 132
    obj_fromCode _ = STSLNE
    obj_class _ = "Straight territorial sea baseline"
    obj_attrA _ = [NATION]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line]

instance Object SUBTLN where
    type BaseType SUBTLN = SUBTLN
    obj_code _ = 133
    obj_fromCode _ = SUBTLN
    obj_class _ = "Submarine transit lane"
    obj_attrA _ = [NOBJNM,OBJNAM,RESTRN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object SWPARE where
    type BaseType SWPARE = SWPARE
    obj_code _ = 134
    obj_fromCode _ = SWPARE
    obj_class _ = "Swept Area"
    obj_attrA _ = [DRVAL1,QUASOU,SOUACC,TECSOU,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object TESARE where
    type BaseType TESARE = TESARE
    obj_code _ = 135
    obj_fromCode _ = TESARE
    obj_class _ = "Territorial sea area"
    obj_attrA _ = [NATION,RESTRN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object TS_PRH where
    type BaseType TS_PRH = TS_PRH
    obj_code _ = 136
    obj_fromCode _ = TS_PRH
    obj_class _ = "Tidal stream - harmonic prediction"
    obj_attrA _ = [NOBJNM,OBJNAM,T_MTOD,T_VAHC,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object TS_PNH where
    type BaseType TS_PNH = TS_PNH
    obj_code _ = 137
    obj_fromCode _ = TS_PNH
    obj_class _ = "Tidal stream - non-harmonic prediction"
    obj_attrA _ = [NOBJNM,OBJNAM,T_MTOD,T_THDF,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object TS_PAD where
    type BaseType TS_PAD = TS_PAD
    obj_code _ = 138
    obj_fromCode _ = TS_PAD
    obj_class _ = "Tidal stream panel data"
    obj_attrA _ = [NOBJNM,OBJNAM,TS_TSP]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object TS_TIS where
    type BaseType TS_TIS = TS_TIS
    obj_code _ = 139
    obj_fromCode _ = TS_TIS
    obj_class _ = "Tidal stream - time series"
    obj_attrA _ = [NOBJNM,OBJNAM,STATUS,TIMEND,TIMSTA,T_TINT,TS_TSV]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object T_HMON where
    type BaseType T_HMON = T_HMON
    obj_code _ = 140
    obj_fromCode _ = T_HMON
    obj_class _ = "Tide - harmonic prediction"
    obj_attrA _ = [NOBJNM,OBJNAM,T_ACWL,T_MTOD,T_VAHC,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object T_NHMN where
    type BaseType T_NHMN = T_NHMN
    obj_code _ = 141
    obj_fromCode _ = T_NHMN
    obj_class _ = "Tide - non-harmonic prediction"
    obj_attrA _ = [NOBJNM,OBJNAM,T_ACWL,T_MTOD,T_THDF,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object T_TIMS where
    type BaseType T_TIMS = T_TIMS
    obj_code _ = 142
    obj_fromCode _ = T_TIMS
    obj_class _ = "Tidal stream - time series"
    obj_attrA _ = [NOBJNM,OBJNAM,T_HWLW,T_TINT,T_TSVL,TIMEND,TIMSTA,STATUS,T_ACWL]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object TIDEWY where
    type BaseType TIDEWY = TIDEWY
    obj_code _ = 143
    obj_fromCode _ = TIDEWY
    obj_class _ = "Tideway"
    obj_attrA _ = [NOBJNM,OBJNAM]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line,Area]

instance Object TOPMAR where
    type BaseType TOPMAR = TOPMAR
    obj_code _ = 144
    obj_fromCode _ = TOPMAR
    obj_class _ = "Top mark"
    obj_attrA _ = [COLOUR,COLPAT,HEIGHT,MARSYS,STATUS,TOPSHP,VERACC,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object TSELNE where
    type BaseType TSELNE = TSELNE
    obj_code _ = 145
    obj_fromCode _ = TSELNE
    obj_class _ = "Traffic Separation Line"
    obj_attrA _ = [CATTSS,DATEND,DATSTA,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line]

instance Object TSSBND where
    type BaseType TSSBND = TSSBND
    obj_code _ = 146
    obj_fromCode _ = TSSBND
    obj_class _ = "Traffic Separation Scheme  Boundary"
    obj_attrA _ = [CATTSS,DATEND,DATSTA,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line]

instance Object TSSCRS where
    type BaseType TSSCRS = TSSCRS
    obj_code _ = 147
    obj_fromCode _ = TSSCRS
    obj_class _ = "Traffic Separation Scheme Crossing"
    obj_attrA _ = [CATTSS,DATEND,DATSTA,RESTRN,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object TSSLPT where
    type BaseType TSSLPT = TSSLPT
    obj_code _ = 148
    obj_fromCode _ = TSSLPT
    obj_class _ = "Traffic Separation Scheme  Lane part"
    obj_attrA _ = [CATTSS,DATEND,DATSTA,ORIENT,RESTRN,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object TSSRON where
    type BaseType TSSRON = TSSRON
    obj_code _ = 149
    obj_fromCode _ = TSSRON
    obj_class _ = "Traffic Separation Scheme  Roundabout"
    obj_attrA _ = [CATTSS,DATEND,DATSTA,RESTRN,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object TSEZNE where
    type BaseType TSEZNE = TSEZNE
    obj_code _ = 150
    obj_fromCode _ = TSEZNE
    obj_class _ = "Traffic Separation Zone"
    obj_attrA _ = [CATTSS,DATEND,DATSTA,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object TUNNEL where
    type BaseType TUNNEL = TUNNEL
    obj_code _ = 151
    obj_fromCode _ = TUNNEL
    obj_class _ = "Tunnel"
    obj_attrA _ = [BURDEP,CONDTN,HORACC,HORCLR,NOBJNM,OBJNAM,STATUS,VERACC,VERCLR]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object TWRTPT where
    type BaseType TWRTPT = TWRTPT
    obj_code _ = 152
    obj_fromCode _ = TWRTPT
    obj_class _ = "Two-way route  part"
    obj_attrA _ = [CATTRK,DATEND,DATSTA,DRVAL1,DRVAL2,ORIENT,QUASOU,SOUACC,STATUS,TECSOU,TRAFIC,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object UWTROC where
    type BaseType UWTROC = UWTROC
    obj_code _ = 153
    obj_fromCode _ = UWTROC
    obj_class _ = "Underwater rock / awash rock"
    obj_attrA _ = [EXPSOU,NATSUR,NATQUA,NOBJNM,OBJNAM,QUASOU,SOUACC,STATUS,TECSOU,VALSOU,VERDAT,WATLEV]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object UNSARE where
    type BaseType UNSARE = UNSARE
    obj_code _ = 154
    obj_fromCode _ = UNSARE
    obj_class _ = "Unsurveyed area"
    obj_attrA _ = []
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object VEGATN where
    type BaseType VEGATN = VEGATN
    obj_code _ = 155
    obj_fromCode _ = VEGATN
    obj_class _ = "Vegetation"
    obj_attrA _ = [CATVEG,CONVIS,ELEVAT,HEIGHT,NOBJNM,OBJNAM,VERACC,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object WATTUR where
    type BaseType WATTUR = WATTUR
    obj_code _ = 156
    obj_fromCode _ = WATTUR
    obj_class _ = "Water turbulence"
    obj_attrA _ = [CATWAT,NOBJNM,OBJNAM]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object WATFAL where
    type BaseType WATFAL = WATFAL
    obj_code _ = 157
    obj_fromCode _ = WATFAL
    obj_class _ = "Waterfall"
    obj_attrA _ = [CONVIS,NOBJNM,OBJNAM,VERACC,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line]

instance Object WEDKLP where
    type BaseType WEDKLP = WEDKLP
    obj_code _ = 158
    obj_fromCode _ = WEDKLP
    obj_class _ = "Weed/Kelp"
    obj_attrA _ = [CATWED,NOBJNM,OBJNAM]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object WRECKS where
    type BaseType WRECKS = WRECKS
    obj_code _ = 159
    obj_fromCode _ = WRECKS
    obj_class _ = "Wreck"
    obj_attrA _ = [CATWRK,CONRAD,CONVIS,EXPSOU,HEIGHT,NOBJNM,OBJNAM,QUASOU,SOUACC,STATUS,TECSOU,VALSOU,VERACC,VERDAT,VERLEN,WATLEV]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object M_ACCY where
    type BaseType M_ACCY = M_ACCY
    obj_code _ = 300
    obj_fromCode _ = M_ACCY
    obj_class _ = "Accuracy of data"
    obj_attrA _ = [HORACC,POSACC,SOUACC,VERACC]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Meta
    obj_primitive _ = [Area]

instance Object M_CSCL where
    type BaseType M_CSCL = M_CSCL
    obj_code _ = 301
    obj_fromCode _ = M_CSCL
    obj_class _ = "Compilation scale of data"
    obj_attrA _ = [CSCALE]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Meta
    obj_primitive _ = [Area]

instance Object M_COVR where
    type BaseType M_COVR = M_COVR
    obj_code _ = 302
    obj_fromCode _ = M_COVR
    obj_class _ = "Coverage"
    obj_attrA _ = [CATCOV]
    obj_attrB _ = [INFORM,NINFOM]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Meta
    obj_primitive _ = [Area]

instance Object M_HDAT where
    type BaseType M_HDAT = M_HDAT
    obj_code _ = 303
    obj_fromCode _ = M_HDAT
    obj_class _ = "Horizontal datum of data"
    obj_attrA _ = [HORDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Meta
    obj_primitive _ = [Area]

instance Object M_HOPA where
    type BaseType M_HOPA = M_HOPA
    obj_code _ = 304
    obj_fromCode _ = M_HOPA
    obj_class _ = "Horizontal datum shift parameters"
    obj_attrA _ = [HORDAT,SHIPAM]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Meta
    obj_primitive _ = [Area]

instance Object M_NPUB where
    type BaseType M_NPUB = M_NPUB
    obj_code _ = 305
    obj_fromCode _ = M_NPUB
    obj_class _ = "Nautical publication information"
    obj_attrA _ = []
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,PUBREF,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Meta
    obj_primitive _ = [Area]

instance Object M_NSYS where
    type BaseType M_NSYS = M_NSYS
    obj_code _ = 306
    obj_fromCode _ = M_NSYS
    obj_class _ = "Navigational system of marks"
    obj_attrA _ = [MARSYS,ORIENT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Meta
    obj_primitive _ = [Area]

instance Object M_PROD where
    type BaseType M_PROD = M_PROD
    obj_code _ = 307
    obj_fromCode _ = M_PROD
    obj_class _ = "Production information"
    obj_attrA _ = [AGENCY,CPDATE,NATION,NMDATE,PRCTRY]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Meta
    obj_primitive _ = [Area]

instance Object M_QUAL where
    type BaseType M_QUAL = M_QUAL
    obj_code _ = 308
    obj_fromCode _ = M_QUAL
    obj_class _ = "Quality of data"
    obj_attrA _ = [CATQUA,CATZOC,DRVAL1,DRVAL2,POSACC,SOUACC,SUREND,SURSTA,TECSOU,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Meta
    obj_primitive _ = [Area]

instance Object M_SDAT where
    type BaseType M_SDAT = M_SDAT
    obj_code _ = 309
    obj_fromCode _ = M_SDAT
    obj_class _ = "Sounding datum"
    obj_attrA _ = [VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Meta
    obj_primitive _ = [Area]

instance Object M_SREL where
    type BaseType M_SREL = M_SREL
    obj_code _ = 310
    obj_fromCode _ = M_SREL
    obj_class _ = "Survey reliability"
    obj_attrA _ = [QUAPOS,QUASOU,SCVAL1,SCVAL2,SDISMN,SDISMX,SURATH,SUREND,SURSTA,SURTYP,TECSOU]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Meta
    obj_primitive _ = [Area]

instance Object M_UNIT where
    type BaseType M_UNIT = M_UNIT
    obj_code _ = 311
    obj_fromCode _ = M_UNIT
    obj_class _ = "Units of measurement of data"
    obj_attrA _ = [DUNITS,HUNITS,PUNITS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Meta
    obj_primitive _ = [Area]

instance Object M_VDAT where
    type BaseType M_VDAT = M_VDAT
    obj_code _ = 312
    obj_fromCode _ = M_VDAT
    obj_class _ = "Vertical datum of data"
    obj_attrA _ = [VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Meta
    obj_primitive _ = [Area]

instance Object C_AGGR where
    type BaseType C_AGGR = C_AGGR
    obj_code _ = 400
    obj_fromCode _ = C_AGGR
    obj_class _ = "Aggregation"
    obj_attrA _ = [NOBJNM,OBJNAM]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Collection
    obj_primitive _ = []

instance Object C_ASSO where
    type BaseType C_ASSO = C_ASSO
    obj_code _ = 401
    obj_fromCode _ = C_ASSO
    obj_class _ = "Association"
    obj_attrA _ = [NOBJNM,OBJNAM]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Collection
    obj_primitive _ = []

instance Object C_STAC where
    type BaseType C_STAC = C_STAC
    obj_code _ = 402
    obj_fromCode _ = C_STAC
    obj_class _ = "Stacked on/stacked under"
    obj_attrA _ = []
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Collection
    obj_primitive _ = []

instance Object AREAS where
    type BaseType AREAS = AREAS
    obj_code _ = 500
    obj_fromCode _ = AREAS
    obj_class _ = "Cartographic area"
    obj_attrA _ = [COLOUR,ORIENT,SCODE,TINTS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Cartographic
    obj_primitive _ = []

instance Object LINES where
    type BaseType LINES = LINES
    obj_code _ = 501
    obj_fromCode _ = LINES
    obj_class _ = "Cartographic line"
    obj_attrA _ = [SCODE]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Cartographic
    obj_primitive _ = []

instance Object CSYMB where
    type BaseType CSYMB = CSYMB
    obj_code _ = 502
    obj_fromCode _ = CSYMB
    obj_class _ = "Cartographic symbol"
    obj_attrA _ = [ORIENT,SCALE,SCODE]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Cartographic
    obj_primitive _ = []

instance Object COMPS where
    type BaseType COMPS = COMPS
    obj_code _ = 503
    obj_fromCode _ = COMPS
    obj_class _ = "Compass"
    obj_attrA _ = [CSIZE,RYRMGV,VALACM,VALMAG]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Cartographic
    obj_primitive _ = []

instance Object TEXTS where
    type BaseType TEXTS = TEXTS
    obj_code _ = 504
    obj_fromCode _ = TEXTS
    obj_class _ = "Text"
    obj_attrA _ = [CHARS,COLOUR,JUSTH,JUSTV,NTXST,SPACE,TXSTR]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Cartographic
    obj_primitive _ = []

