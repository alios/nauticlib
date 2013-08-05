{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}


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

module Data.S57.Objects
    ( Object (..)
    , ClassTag (..)
    , GeoPrimitive (..)
    , ObjectT (..)
    ) where

import           Data.Data
import           Data.S57.Attributes
import           Data.SafeCopy       (base, deriveSafeCopy)
import           Data.Typeable

data ClassTag
    = Meta -- ^ Feature 'Object' which contains information about other objects
    | Cartographic -- ^ Feature 'Object' which contains information about the cartographic representation (including text) of real world entities
    | Geo -- ^ Feature 'Object' which contains information which carries the descriptive characteristics of a real world entity
    | Collection -- ^ Feature 'Object' which describes the relationship between other 'Object's
    deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''ClassTag)

data GeoPrimitive
    = Point
    | Line
    | Area
    deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''GeoPrimitive)


class Object t where
    obj_code :: t -> Integer
    obj_fromCode :: Integer -> t
    obj_class :: t -> String
    obj_attrA :: t -> [AttributeT]
    obj_attrB :: t -> [AttributeT]
    obj_attrC :: t -> [AttributeT]
    obj_classTag :: t -> ClassTag
    obj_primitive :: t -> [GeoPrimitive]



data ObjectT
    = ADMARE
    | AIRARE
    | ACHBRT
    | ACHARE
    | BCNCAR
    | BCNISD
    | BCNLAT
    | BCNSAW
    | BCNSPP
    | BERTHS
    | BRIDGE
    | BUISGL
    | BUAARE
    | BOYCAR
    | BOYINB
    | BOYISD
    | BOYLAT
    | BOYSAW
    | BOYSPP
    | CBLARE
    | CBLOHD
    | CBLSUB
    | CANALS
    | CTSARE
    | CAUSWY
    | CTNARE
    | CHKPNT
    | CGUSTA
    | COALNE
    | CONZNE
    | COSARE
    | CTRPNT
    | CONVYR
    | CRANES
    | CURENT
    | CUSZNE
    | DAMCON
    | DAYMAR
    | DWRTCL
    | DWRTPT
    | DEPARE
    | DEPCNT
    | DISMAR
    | DOCARE
    | DRGARE
    | DRYDOC
    | DMPGRD
    | DYKCON
    | EXEZNE
    | FAIRWY
    | FNCLNE
    | FERYRT
    | FSHZNE
    | FSHFAC
    | FSHGRD
    | FLODOC
    | FOGSIG
    | FORSTC
    | FRPARE
    | GATCON
    | GRIDRN
    | HRBARE
    | HRBFAC
    | HULKES
    | ICEARE
    | ICNARE
    | ISTZNE
    | LAKARE
    | LNDARE
    | LNDELV
    | LNDRGN
    | LNDMRK
    | LIGHTS
    | LITFLT
    | LITVES
    | LOCMAG
    | LOKBSN
    | LOGPON
    | MAGVAR
    | MARCUL
    | MIPARE
    | MORFAC
    | NAVLNE
    | OBSTRN
    | OFSPLF
    | OSPARE
    | OILBAR
    | PILPNT
    | PILBOP
    | PIPARE
    | PIPOHD
    | PIPSOL
    | PONTON
    | PRCARE
    | PRDARE
    | PYLONS
    | RADLNE
    | RADRNG
    | RADRFL
    | RADSTA
    | RTPBCN
    | RDOCAL
    | RDOSTA
    | RAILWY
    | RAPIDS
    | RCRTCL
    | RECTRC
    | RCTLPT
    | RSCSTA
    | RESARE
    | RETRFL
    | RIVERS
    | ROADWY
    | RUNWAY
    | SNDWAV
    | SEAARE
    | SPLARE
    | SBDARE
    | SLCONS
    | SISTAT
    | SISTAW
    | SILTNK
    | SLOTOP
    | SLOGRD
    | SMCFAC
    | SOUNDG
    | SPRING
    | STSLNE
    | SUBTLN
    | SWPARE
    | TESARE
    | TS_PRH
    | TS_PNH
    | TS_PAD
    | TS_TIS
    | T_HMON
    | T_NHMN
    | T_TIMS
    | TIDEWY
    | TOPMAR
    | TSELNE
    | TSSBND
    | TSSCRS
    | TSSLPT
    | TSSRON
    | TSEZNE
    | TUNNEL
    | TWRTPT
    | UWTROC
    | UNSARE
    | VEGATN
    | WATTUR
    | WATFAL
    | WEDKLP
    | WRECKS
    | M_ACCY
    | M_CSCL
    | M_COVR
    | M_HDAT
    | M_HOPA
    | M_NPUB
    | M_NSYS
    | M_PROD
    | M_QUAL
    | M_SDAT
    | M_SREL
    | M_UNIT
    | M_VDAT
    | C_AGGR
    | C_ASSO
    | C_STAC
    | AREAS
    | LINES
    | CSYMB
    | COMPS
    | TEXTS
        deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''ObjectT)



instance Object ObjectT where
    obj_classTag ADMARE = Geo
    obj_classTag AIRARE = Geo
    obj_classTag ACHBRT = Geo
    obj_classTag ACHARE = Geo
    obj_classTag BCNCAR = Geo
    obj_classTag BCNISD = Geo
    obj_classTag BCNLAT = Geo
    obj_classTag BCNSAW = Geo
    obj_classTag BCNSPP = Geo
    obj_classTag BERTHS = Geo
    obj_classTag BRIDGE = Geo
    obj_classTag BUISGL = Geo
    obj_classTag BUAARE = Geo
    obj_classTag BOYCAR = Geo
    obj_classTag BOYINB = Geo
    obj_classTag BOYISD = Geo
    obj_classTag BOYLAT = Geo
    obj_classTag BOYSAW = Geo
    obj_classTag BOYSPP = Geo
    obj_classTag CBLARE = Geo
    obj_classTag CBLOHD = Geo
    obj_classTag CBLSUB = Geo
    obj_classTag CANALS = Geo
    obj_classTag CTSARE = Geo
    obj_classTag CAUSWY = Geo
    obj_classTag CTNARE = Geo
    obj_classTag CHKPNT = Geo
    obj_classTag CGUSTA = Geo
    obj_classTag COALNE = Geo
    obj_classTag CONZNE = Geo
    obj_classTag COSARE = Geo
    obj_classTag CTRPNT = Geo
    obj_classTag CONVYR = Geo
    obj_classTag CRANES = Geo
    obj_classTag CURENT = Geo
    obj_classTag CUSZNE = Geo
    obj_classTag DAMCON = Geo
    obj_classTag DAYMAR = Geo
    obj_classTag DWRTCL = Geo
    obj_classTag DWRTPT = Geo
    obj_classTag DEPARE = Geo
    obj_classTag DEPCNT = Geo
    obj_classTag DISMAR = Geo
    obj_classTag DOCARE = Geo
    obj_classTag DRGARE = Geo
    obj_classTag DRYDOC = Geo
    obj_classTag DMPGRD = Geo
    obj_classTag DYKCON = Geo
    obj_classTag EXEZNE = Geo
    obj_classTag FAIRWY = Geo
    obj_classTag FNCLNE = Geo
    obj_classTag FERYRT = Geo
    obj_classTag FSHZNE = Geo
    obj_classTag FSHFAC = Geo
    obj_classTag FSHGRD = Geo
    obj_classTag FLODOC = Geo
    obj_classTag FOGSIG = Geo
    obj_classTag FORSTC = Geo
    obj_classTag FRPARE = Geo
    obj_classTag GATCON = Geo
    obj_classTag GRIDRN = Geo
    obj_classTag HRBARE = Geo
    obj_classTag HRBFAC = Geo
    obj_classTag HULKES = Geo
    obj_classTag ICEARE = Geo
    obj_classTag ICNARE = Geo
    obj_classTag ISTZNE = Geo
    obj_classTag LAKARE = Geo
    obj_classTag LNDARE = Geo
    obj_classTag LNDELV = Geo
    obj_classTag LNDRGN = Geo
    obj_classTag LNDMRK = Geo
    obj_classTag LIGHTS = Geo
    obj_classTag LITFLT = Geo
    obj_classTag LITVES = Geo
    obj_classTag LOCMAG = Geo
    obj_classTag LOKBSN = Geo
    obj_classTag LOGPON = Geo
    obj_classTag MAGVAR = Geo
    obj_classTag MARCUL = Geo
    obj_classTag MIPARE = Geo
    obj_classTag MORFAC = Geo
    obj_classTag NAVLNE = Geo
    obj_classTag OBSTRN = Geo
    obj_classTag OFSPLF = Geo
    obj_classTag OSPARE = Geo
    obj_classTag OILBAR = Geo
    obj_classTag PILPNT = Geo
    obj_classTag PILBOP = Geo
    obj_classTag PIPARE = Geo
    obj_classTag PIPOHD = Geo
    obj_classTag PIPSOL = Geo
    obj_classTag PONTON = Geo
    obj_classTag PRCARE = Geo
    obj_classTag PRDARE = Geo
    obj_classTag PYLONS = Geo
    obj_classTag RADLNE = Geo
    obj_classTag RADRNG = Geo
    obj_classTag RADRFL = Geo
    obj_classTag RADSTA = Geo
    obj_classTag RTPBCN = Geo
    obj_classTag RDOCAL = Geo
    obj_classTag RDOSTA = Geo
    obj_classTag RAILWY = Geo
    obj_classTag RAPIDS = Geo
    obj_classTag RCRTCL = Geo
    obj_classTag RECTRC = Geo
    obj_classTag RCTLPT = Geo
    obj_classTag RSCSTA = Geo
    obj_classTag RESARE = Geo
    obj_classTag RETRFL = Geo
    obj_classTag RIVERS = Geo
    obj_classTag ROADWY = Geo
    obj_classTag RUNWAY = Geo
    obj_classTag SNDWAV = Geo
    obj_classTag SEAARE = Geo
    obj_classTag SPLARE = Geo
    obj_classTag SBDARE = Geo
    obj_classTag SLCONS = Geo
    obj_classTag SISTAT = Geo
    obj_classTag SISTAW = Geo
    obj_classTag SILTNK = Geo
    obj_classTag SLOTOP = Geo
    obj_classTag SLOGRD = Geo
    obj_classTag SMCFAC = Geo
    obj_classTag SOUNDG = Geo
    obj_classTag SPRING = Geo
    obj_classTag STSLNE = Geo
    obj_classTag SUBTLN = Geo
    obj_classTag SWPARE = Geo
    obj_classTag TESARE = Geo
    obj_classTag TS_PRH = Geo
    obj_classTag TS_PNH = Geo
    obj_classTag TS_PAD = Geo
    obj_classTag TS_TIS = Geo
    obj_classTag T_HMON = Geo
    obj_classTag T_NHMN = Geo
    obj_classTag T_TIMS = Geo
    obj_classTag TIDEWY = Geo
    obj_classTag TOPMAR = Geo
    obj_classTag TSELNE = Geo
    obj_classTag TSSBND = Geo
    obj_classTag TSSCRS = Geo
    obj_classTag TSSLPT = Geo
    obj_classTag TSSRON = Geo
    obj_classTag TSEZNE = Geo
    obj_classTag TUNNEL = Geo
    obj_classTag TWRTPT = Geo
    obj_classTag UWTROC = Geo
    obj_classTag UNSARE = Geo
    obj_classTag VEGATN = Geo
    obj_classTag WATTUR = Geo
    obj_classTag WATFAL = Geo
    obj_classTag WEDKLP = Geo
    obj_classTag WRECKS = Geo
    obj_classTag M_ACCY = Meta
    obj_classTag M_CSCL = Meta
    obj_classTag M_COVR = Meta
    obj_classTag M_HDAT = Meta
    obj_classTag M_HOPA = Meta
    obj_classTag M_NPUB = Meta
    obj_classTag M_NSYS = Meta
    obj_classTag M_PROD = Meta
    obj_classTag M_QUAL = Meta
    obj_classTag M_SDAT = Meta
    obj_classTag M_SREL = Meta
    obj_classTag M_UNIT = Meta
    obj_classTag M_VDAT = Meta
    obj_classTag C_AGGR = Collection
    obj_classTag C_ASSO = Collection
    obj_classTag C_STAC = Collection
    obj_classTag AREAS = Cartographic
    obj_classTag LINES = Cartographic
    obj_classTag CSYMB = Cartographic
    obj_classTag COMPS = Cartographic
    obj_classTag TEXTS = Cartographic
    obj_primitive ADMARE = [Area]
    obj_primitive AIRARE = [Point,Area]
    obj_primitive ACHBRT = [Point,Area]
    obj_primitive ACHARE = [Point,Area]
    obj_primitive BCNCAR = [Point]
    obj_primitive BCNISD = [Point]
    obj_primitive BCNLAT = [Point]
    obj_primitive BCNSAW = [Point]
    obj_primitive BCNSPP = [Point]
    obj_primitive BERTHS = [Point,Line,Area]
    obj_primitive BRIDGE = [Point,Line,Area]
    obj_primitive BUISGL = [Point,Area]
    obj_primitive BUAARE = [Point,Area]
    obj_primitive BOYCAR = [Point]
    obj_primitive BOYINB = [Point]
    obj_primitive BOYISD = [Point]
    obj_primitive BOYLAT = [Point]
    obj_primitive BOYSAW = [Point]
    obj_primitive BOYSPP = [Point]
    obj_primitive CBLARE = [Area]
    obj_primitive CBLOHD = [Line]
    obj_primitive CBLSUB = [Line]
    obj_primitive CANALS = [Line,Area]
    obj_primitive CTSARE = [Point,Area]
    obj_primitive CAUSWY = [Line,Area]
    obj_primitive CTNARE = [Point,Area]
    obj_primitive CHKPNT = [Point,Area]
    obj_primitive CGUSTA = [Point]
    obj_primitive COALNE = [Line]
    obj_primitive CONZNE = [Area]
    obj_primitive COSARE = [Area]
    obj_primitive CTRPNT = [Point]
    obj_primitive CONVYR = [Line,Area]
    obj_primitive CRANES = [Point,Area]
    obj_primitive CURENT = [Point]
    obj_primitive CUSZNE = [Area]
    obj_primitive DAMCON = [Point,Line,Area]
    obj_primitive DAYMAR = [Point]
    obj_primitive DWRTCL = [Line]
    obj_primitive DWRTPT = [Area]
    obj_primitive DEPARE = [Line,Area]
    obj_primitive DEPCNT = [Line]
    obj_primitive DISMAR = [Point]
    obj_primitive DOCARE = [Area]
    obj_primitive DRGARE = [Area]
    obj_primitive DRYDOC = [Area]
    obj_primitive DMPGRD = [Point,Area]
    obj_primitive DYKCON = [Line,Area]
    obj_primitive EXEZNE = [Area]
    obj_primitive FAIRWY = [Area]
    obj_primitive FNCLNE = [Line]
    obj_primitive FERYRT = [Line,Area]
    obj_primitive FSHZNE = [Area]
    obj_primitive FSHFAC = [Point,Line,Area]
    obj_primitive FSHGRD = [Area]
    obj_primitive FLODOC = [Line,Area]
    obj_primitive FOGSIG = [Point]
    obj_primitive FORSTC = [Point,Line,Area]
    obj_primitive FRPARE = [Area]
    obj_primitive GATCON = [Point,Line,Area]
    obj_primitive GRIDRN = [Point,Area]
    obj_primitive HRBARE = [Area]
    obj_primitive HRBFAC = [Point,Area]
    obj_primitive HULKES = [Point,Area]
    obj_primitive ICEARE = [Area]
    obj_primitive ICNARE = [Point,Area]
    obj_primitive ISTZNE = [Area]
    obj_primitive LAKARE = [Area]
    obj_primitive LNDARE = [Point,Line,Area]
    obj_primitive LNDELV = [Point,Line]
    obj_primitive LNDRGN = [Point,Area]
    obj_primitive LNDMRK = [Point,Line,Area]
    obj_primitive LIGHTS = [Point]
    obj_primitive LITFLT = [Point]
    obj_primitive LITVES = [Point]
    obj_primitive LOCMAG = [Point,Line,Area]
    obj_primitive LOKBSN = [Area]
    obj_primitive LOGPON = [Point,Area]
    obj_primitive MAGVAR = [Point,Line,Area]
    obj_primitive MARCUL = [Point,Line,Area]
    obj_primitive MIPARE = [Point,Area]
    obj_primitive MORFAC = [Point,Line,Area]
    obj_primitive NAVLNE = [Line]
    obj_primitive OBSTRN = [Point,Line,Area]
    obj_primitive OFSPLF = [Point,Area]
    obj_primitive OSPARE = [Area]
    obj_primitive OILBAR = [Line]
    obj_primitive PILPNT = [Point]
    obj_primitive PILBOP = [Point,Area]
    obj_primitive PIPARE = [Point,Area]
    obj_primitive PIPOHD = [Line]
    obj_primitive PIPSOL = [Point,Line]
    obj_primitive PONTON = [Line,Area]
    obj_primitive PRCARE = [Point,Area]
    obj_primitive PRDARE = [Point,Area]
    obj_primitive PYLONS = [Point,Area]
    obj_primitive RADLNE = [Line]
    obj_primitive RADRNG = [Area]
    obj_primitive RADRFL = [Point]
    obj_primitive RADSTA = [Point]
    obj_primitive RTPBCN = [Point]
    obj_primitive RDOCAL = [Point,Line]
    obj_primitive RDOSTA = [Point]
    obj_primitive RAILWY = [Line]
    obj_primitive RAPIDS = [Point,Line,Area]
    obj_primitive RCRTCL = [Line]
    obj_primitive RECTRC = [Line,Area]
    obj_primitive RCTLPT = [Point,Area]
    obj_primitive RSCSTA = [Point]
    obj_primitive RESARE = [Area]
    obj_primitive RETRFL = [Point]
    obj_primitive RIVERS = [Line,Area]
    obj_primitive ROADWY = [Point,Line,Area]
    obj_primitive RUNWAY = [Point,Line,Area]
    obj_primitive SNDWAV = [Point,Line,Area]
    obj_primitive SEAARE = [Point,Area]
    obj_primitive SPLARE = [Point,Area]
    obj_primitive SBDARE = [Point,Line,Area]
    obj_primitive SLCONS = [Point,Line,Area]
    obj_primitive SISTAT = [Point]
    obj_primitive SISTAW = [Point]
    obj_primitive SILTNK = [Point,Area]
    obj_primitive SLOTOP = [Line]
    obj_primitive SLOGRD = [Point,Area]
    obj_primitive SMCFAC = [Point,Area]
    obj_primitive SOUNDG = [Point]
    obj_primitive SPRING = [Point]
    obj_primitive STSLNE = [Line]
    obj_primitive SUBTLN = [Area]
    obj_primitive SWPARE = [Area]
    obj_primitive TESARE = [Area]
    obj_primitive TS_PRH = [Point,Area]
    obj_primitive TS_PNH = [Point,Area]
    obj_primitive TS_PAD = [Point,Area]
    obj_primitive TS_TIS = [Point,Area]
    obj_primitive T_HMON = [Point,Area]
    obj_primitive T_NHMN = [Point,Area]
    obj_primitive T_TIMS = [Point,Area]
    obj_primitive TIDEWY = [Line,Area]
    obj_primitive TOPMAR = [Point]
    obj_primitive TSELNE = [Line]
    obj_primitive TSSBND = [Line]
    obj_primitive TSSCRS = [Area]
    obj_primitive TSSLPT = [Area]
    obj_primitive TSSRON = [Area]
    obj_primitive TSEZNE = [Area]
    obj_primitive TUNNEL = [Point,Line,Area]
    obj_primitive TWRTPT = [Area]
    obj_primitive UWTROC = [Point]
    obj_primitive UNSARE = [Area]
    obj_primitive VEGATN = [Point,Line,Area]
    obj_primitive WATTUR = [Point,Line,Area]
    obj_primitive WATFAL = [Point,Line]
    obj_primitive WEDKLP = [Point,Area]
    obj_primitive WRECKS = [Point,Area]
    obj_primitive M_ACCY = [Area]
    obj_primitive M_CSCL = [Area]
    obj_primitive M_COVR = [Area]
    obj_primitive M_HDAT = [Area]
    obj_primitive M_HOPA = [Area]
    obj_primitive M_NPUB = [Area]
    obj_primitive M_NSYS = [Area]
    obj_primitive M_PROD = [Area]
    obj_primitive M_QUAL = [Area]
    obj_primitive M_SDAT = [Area]
    obj_primitive M_SREL = [Area]
    obj_primitive M_UNIT = [Area]
    obj_primitive M_VDAT = [Area]
    obj_primitive C_AGGR = []
    obj_primitive C_ASSO = []
    obj_primitive C_STAC = []
    obj_primitive AREAS = []
    obj_primitive LINES = []
    obj_primitive CSYMB = []
    obj_primitive COMPS = []
    obj_primitive TEXTS = []
    obj_attrC ADMARE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC AIRARE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC ACHBRT = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC ACHARE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC BCNCAR = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC BCNISD = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC BCNLAT = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC BCNSAW = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC BCNSPP = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC BERTHS = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC BRIDGE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC BUISGL = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC BUAARE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC BOYCAR = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC BOYINB = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC BOYISD = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC BOYLAT = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC BOYSAW = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC BOYSPP = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC CBLARE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC CBLOHD = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC CBLSUB = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC CANALS = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC CTSARE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC CAUSWY = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC CTNARE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC CHKPNT = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC CGUSTA = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC COALNE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC CONZNE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC COSARE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC CTRPNT = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC CONVYR = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC CRANES = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC CURENT = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC CUSZNE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC DAMCON = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC DAYMAR = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC DWRTCL = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC DWRTPT = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC DEPARE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC DEPCNT = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC DISMAR = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC DOCARE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC DRGARE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC DRYDOC = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC DMPGRD = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC DYKCON = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC EXEZNE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC FAIRWY = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC FNCLNE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC FERYRT = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC FSHZNE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC FSHFAC = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC FSHGRD = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC FLODOC = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC FOGSIG = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC FORSTC = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC FRPARE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC GATCON = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC GRIDRN = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC HRBARE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC HRBFAC = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC HULKES = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC ICEARE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC ICNARE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC ISTZNE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC LAKARE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC LNDARE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC LNDELV = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC LNDRGN = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC LNDMRK = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC LIGHTS = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC LITFLT = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC LITVES = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC LOCMAG = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC LOKBSN = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC LOGPON = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC MAGVAR = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC MARCUL = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC MIPARE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC MORFAC = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC NAVLNE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC OBSTRN = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC OFSPLF = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC OSPARE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC OILBAR = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC PILPNT = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC PILBOP = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC PIPARE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC PIPOHD = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC PIPSOL = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC PONTON = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC PRCARE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC PRDARE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC PYLONS = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC RADLNE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC RADRNG = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC RADRFL = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC RADSTA = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC RTPBCN = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC RDOCAL = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC RDOSTA = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC RAILWY = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC RAPIDS = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC RCRTCL = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC RECTRC = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC RCTLPT = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC RSCSTA = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC RESARE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC RETRFL = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC RIVERS = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC ROADWY = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC RUNWAY = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC SNDWAV = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC SEAARE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC SPLARE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC SBDARE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC SLCONS = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC SISTAT = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC SISTAW = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC SILTNK = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC SLOTOP = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC SLOGRD = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC SMCFAC = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC SOUNDG = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC SPRING = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC STSLNE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC SUBTLN = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC SWPARE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC TESARE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC TS_PRH = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC TS_PNH = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC TS_PAD = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC TS_TIS = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC T_HMON = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC T_NHMN = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC T_TIMS = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC TIDEWY = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC TOPMAR = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC TSELNE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC TSSBND = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC TSSCRS = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC TSSLPT = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC TSSRON = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC TSEZNE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC TUNNEL = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC TWRTPT = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC UWTROC = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC UNSARE = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC VEGATN = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC WATTUR = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC WATFAL = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC WEDKLP = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC WRECKS = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC M_ACCY = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC M_CSCL = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC M_COVR = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC M_HDAT = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC M_HOPA = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC M_NPUB = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC M_NSYS = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC M_PROD = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC M_QUAL = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC M_SDAT = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC M_SREL = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC M_UNIT = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC M_VDAT = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC C_AGGR = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC C_ASSO = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC C_STAC = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC AREAS = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC LINES = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC CSYMB = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC COMPS = [RECDAT,RECIND,SORDAT,SORIND]
    obj_attrC TEXTS = [RECDAT,RECIND,SORDAT,SORIND]

    obj_attrB ADMARE = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB AIRARE = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB ACHBRT = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB ACHARE = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB BCNCAR = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB BCNISD = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB BCNLAT = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB BCNSAW = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB BCNSPP = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB BERTHS = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB BRIDGE = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB BUISGL = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB BUAARE = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB BOYCAR = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB BOYINB = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB BOYISD = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB BOYLAT = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB BOYSAW = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB BOYSPP = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB CBLARE = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB CBLOHD = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB CBLSUB = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB CANALS = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB CTSARE = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB CAUSWY = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB CTNARE = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB CHKPNT = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB CGUSTA = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB COALNE = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB CONZNE = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB COSARE = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB CTRPNT = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB CONVYR = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB CRANES = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB CURENT = [INFORM,NINFOM,SCAMAX,SCAMIN]
    obj_attrB CUSZNE = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB DAMCON = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB DAYMAR = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB DWRTCL = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB DWRTPT = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB DEPARE = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB DEPCNT = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB DISMAR = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB DOCARE = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB DRGARE = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB DRYDOC = [INFORM,NINFOM,SCAMAX,SCAMIN]
    obj_attrB DMPGRD = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB DYKCON = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB EXEZNE = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB FAIRWY = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB FNCLNE = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB FERYRT = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB FSHZNE = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB FSHFAC = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB FSHGRD = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB FLODOC = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB FOGSIG = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB FORSTC = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB FRPARE = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB GATCON = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB GRIDRN = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB HRBARE = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB HRBFAC = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB HULKES = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB ICEARE = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB ICNARE = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB ISTZNE = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB LAKARE = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB LNDARE = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB LNDELV = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB LNDRGN = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB LNDMRK = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB LIGHTS = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB LITFLT = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB LITVES = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB LOCMAG = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB LOKBSN = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB LOGPON = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB MAGVAR = [INFORM,NINFOM,SCAMAX,SCAMIN]
    obj_attrB MARCUL = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB MIPARE = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB MORFAC = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB NAVLNE = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB OBSTRN = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB OFSPLF = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB OSPARE = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB OILBAR = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB PILPNT = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB PILBOP = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB PIPARE = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB PIPOHD = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB PIPSOL = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB PONTON = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB PRCARE = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB PRDARE = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB PYLONS = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB RADLNE = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB RADRNG = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB RADRFL = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB RADSTA = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB RTPBCN = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB RDOCAL = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB RDOSTA = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB RAILWY = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB RAPIDS = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB RCRTCL = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB RECTRC = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB RCTLPT = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB RSCSTA = [INFORM,NINFOM,SCAMAX,SCAMIN]
    obj_attrB RESARE = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB RETRFL = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB RIVERS = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB ROADWY = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB RUNWAY = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB SNDWAV = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB SEAARE = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB SPLARE = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC,VALDCO]
    obj_attrB SBDARE = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB SLCONS = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB SISTAT = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB SISTAW = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB SILTNK = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB SLOTOP = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB SLOGRD = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB SMCFAC = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB SOUNDG = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB SPRING = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB STSLNE = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB SUBTLN = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB SWPARE = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB TESARE = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB TS_PRH = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB TS_PNH = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB TS_PAD = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB TS_TIS = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB T_HMON = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB T_NHMN = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB T_TIMS = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB TIDEWY = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB TOPMAR = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB TSELNE = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB TSSBND = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB TSSCRS = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB TSSLPT = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB TSSRON = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB TSEZNE = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB TUNNEL = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB TWRTPT = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB UWTROC = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB UNSARE = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB VEGATN = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB WATTUR = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB WATFAL = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB WEDKLP = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB WRECKS = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB M_ACCY = [INFORM,NINFOM,NTXTDS,TXTDSC]
    obj_attrB M_CSCL = [INFORM,NINFOM,NTXTDS,TXTDSC]
    obj_attrB M_COVR = [INFORM,NINFOM]
    obj_attrB M_HDAT = [INFORM,NINFOM,NTXTDS,TXTDSC]
    obj_attrB M_HOPA = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB M_NPUB = [INFORM,NINFOM,NTXTDS,PICREP,PUBREF,TXTDSC]
    obj_attrB M_NSYS = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB M_PROD = [INFORM,NINFOM,NTXTDS,TXTDSC]
    obj_attrB M_QUAL = [INFORM,NINFOM,NTXTDS,TXTDSC]
    obj_attrB M_SDAT = [INFORM,NINFOM,NTXTDS,TXTDSC]
    obj_attrB M_SREL = [INFORM,NINFOM,NTXTDS,TXTDSC]
    obj_attrB M_UNIT = [INFORM,NINFOM,NTXTDS,TXTDSC]
    obj_attrB M_VDAT = [INFORM,NINFOM,NTXTDS,TXTDSC]
    obj_attrB C_AGGR = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB C_ASSO = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB C_STAC = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB AREAS = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB LINES = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB CSYMB = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB COMPS = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrB TEXTS = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]

    obj_attrA ADMARE = [JRSDTN,NATION,NOBJNM,OBJNAM]
    obj_attrA AIRARE = [CATAIR,CONDTN,CONVIS,NOBJNM,OBJNAM,STATUS]
    obj_attrA ACHBRT = [CATACH,DATEND,DATSTA,NOBJNM,OBJNAM,PEREND,PERSTA,RADIUS,STATUS]
    obj_attrA ACHARE = [CATACH,DATEND,DATSTA,NOBJNM,OBJNAM,PEREND,PERSTA,RESTRN,STATUS]
    obj_attrA BCNCAR = [BCNSHP,CATCAM,COLOUR,COLPAT,CONDTN,CONVIS,CONRAD,DATEND,DATSTA,ELEVAT,HEIGHT,MARSYS,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERDAT,VERLEN]
    obj_attrA BCNISD = [BCNSHP,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,ELEVAT,HEIGHT,MARSYS,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERDAT,VERLEN]
    obj_attrA BCNLAT = [BCNSHP,CATLAM,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,ELEVAT,HEIGHT,MARSYS,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERDAT,VERLEN]
    obj_attrA BCNSAW = [BCNSHP,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,ELEVAT,HEIGHT,MARSYS,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERDAT,VERLEN]
    obj_attrA BCNSPP = [BCNSHP,CATSPM,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,ELEVAT,HEIGHT,MARSYS,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERDAT,VERLEN]
    obj_attrA BERTHS = [DATEND,DATSTA,DRVAL1,NOBJNM,OBJNAM,PEREND,PERSTA,QUASOU,SOUACC,STATUS,VERDAT]
    obj_attrA BRIDGE = [CATBRG,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,HORACC,HORCLR,NATCON,NOBJNM,OBJNAM,VERACC,VERCCL,VERCLR,VERCOP,VERDAT]
    obj_attrA BUISGL = [BUISHP,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,ELEVAT,FUNCTN,HEIGHT,NATCON,NOBJNM,OBJNAM,STATUS,VERACC,VERDAT,VERLEN]
    obj_attrA BUAARE = [CATBUA,CONDTN,CONRAD,CONVIS,HEIGHT,NOBJNM,OBJNAM,VERACC,VERDAT]
    obj_attrA BOYCAR = [BOYSHP,CATCAM,COLOUR,COLPAT,CONRAD,DATEND,DATSTA,MARSYS,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERLEN]
    obj_attrA BOYINB = [BOYSHP,CATINB,COLOUR,COLPAT,CONRAD,DATEND,DATSTA,MARSYS,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,PRODCT,STATUS,VERACC,VERLEN]
    obj_attrA BOYISD = [BOYSHP,COLOUR,COLPAT,CONRAD,DATEND,DATSTA,MARSYS,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERLEN]
    obj_attrA BOYLAT = [BOYSHP,CATLAM,COLOUR,COLPAT,CONRAD,DATEND,DATSTA,MARSYS,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERLEN]
    obj_attrA BOYSAW = [BOYSHP,COLOUR,COLPAT,CONRAD,DATEND,DATSTA,MARSYS,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERLEN]
    obj_attrA BOYSPP = [BOYSHP,CATSPM,COLOUR,COLPAT,CONRAD,DATEND,DATSTA,MARSYS,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERLEN]
    obj_attrA CBLARE = [CATCBL,DATEND,DATSTA,NOBJNM,OBJNAM,RESTRN,STATUS]
    obj_attrA CBLOHD = [CATCBL,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,ICEFAC,NOBJNM,OBJNAM,STATUS,VERACC,VERCLR,VERCSA,VERDAT]
    obj_attrA CBLSUB = [BURDEP,CATCBL,CONDTN,DATEND,DATSTA,NOBJNM,OBJNAM,STATUS,VERDAT]
    obj_attrA CANALS = [CATCAN,CONDTN,DATEND,DATSTA,HORACC,HORCLR,HORWID,NOBJNM,OBJNAM,STATUS]
    obj_attrA CTSARE = [DATEND,DATSTA,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS]
    obj_attrA CAUSWY = [CONDTN,NATCON,NOBJNM,OBJNAM,STATUS,WATLEV]
    obj_attrA CTNARE = [DATEND,DATSTA,PEREND,PERSTA]
    obj_attrA CHKPNT = [CATCHP,NOBJNM,OBJNAM,STATUS]
    obj_attrA CGUSTA = [DATEND,DATSTA,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS]
    obj_attrA COALNE = [CATCOA,COLOUR,CONRAD,CONVIS,ELEVAT,NOBJNM,OBJNAM,VERACC,VERDAT]
    obj_attrA CONZNE = [DATEND,DATSTA,NATION,STATUS]
    obj_attrA COSARE = [NATION,NOBJNM,OBJNAM]
    obj_attrA CTRPNT = [CATCTR,DATEND,DATSTA,ELEVAT,NOBJNM,OBJNAM,VERACC,VERDAT]
    obj_attrA CONVYR = [CATCON,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,HEIGHT,LIFCAP,NOBJNM,OBJNAM,PRODCT,STATUS,VERACC,VERCLR,VERDAT,VERLEN]
    obj_attrA CRANES = [CATCRN,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,HEIGHT,LIFCAP,NOBJNM,OBJNAM,ORIENT,RADIUS,STATUS,VERACC,VERCLR,VERDAT,VERLEN]
    obj_attrA CURENT = [CURVEL,DATEND,DATSTA,NOBJNM,OBJNAM,ORIENT,PEREND,PERSTA]
    obj_attrA CUSZNE = [NATION]
    obj_attrA DAMCON = [CATDAM,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,HEIGHT,NATCON,NOBJNM,OBJNAM,VERACC,VERDAT,VERLEN]
    obj_attrA DAYMAR = [CATSPM,COLOUR,COLPAT,DATEND,DATSTA,ELEVAT,HEIGHT,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,TOPSHP,VERACC,VERDAT,VERLEN]
    obj_attrA DWRTCL = [CATTRK,DATEND,DATSTA,DRVAL1,DRVAL2,NOBJNM,OBJNAM,ORIENT,QUASOU,SOUACC,STATUS,TECSOU,TRAFIC,VERDAT]
    obj_attrA DWRTPT = [DATEND,DATSTA,DRVAL1,DRVAL2,NOBJNM,OBJNAM,ORIENT,QUASOU,SOUACC,STATUS,TECSOU,TRAFIC,VERDAT,RESTRN]
    obj_attrA DEPARE = [DRVAL1,DRVAL2,QUASOU,SOUACC,VERDAT]
    obj_attrA DEPCNT = [VALDCO,VERDAT]
    obj_attrA DISMAR = [CATDIS,DATEND,DATSTA,NOBJNM,OBJNAM]
    obj_attrA DOCARE = [CATDOC,CONDTN,DATEND,DATSTA,HORACC,HORCLR,NOBJNM,OBJNAM,STATUS]
    obj_attrA DRGARE = [DRVAL1,DRVAL2,NOBJNM,OBJNAM,QUASOU,RESTRN,SOUACC,TECSOU,VERDAT]
    obj_attrA DRYDOC = [CONDTN,HORACC,HORCLR,HORLEN,HORWID,NOBJNM,OBJNAM,STATUS,DRVAL1,QUASOU,SOUACC,VERDAT]
    obj_attrA DMPGRD = [CATDPG,NOBJNM,OBJNAM,RESTRN,STATUS]
    obj_attrA DYKCON = [CONDTN,CONRAD,DATEND,DATSTA,HEIGHT,NATCON,VERACC,VERDAT,VERLEN]
    obj_attrA EXEZNE = [NATION]
    obj_attrA FAIRWY = [DATEND,DATSTA,DRVAL1,NOBJNM,OBJNAM,ORIENT,QUASOU,RESTRN,SOUACC,STATUS,TRAFIC,VERDAT]
    obj_attrA FNCLNE = [CATFNC,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,ELEVAT,HEIGHT,NATCON,NOBJNM,OBJNAM,STATUS,VERACC,VERDAT,VERLEN]
    obj_attrA FERYRT = [CATFRY,DATEND,DATSTA,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS]
    obj_attrA FSHZNE = [NATION,NOBJNM,OBJNAM,STATUS]
    obj_attrA FSHFAC = [CATFIF,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERLEN]
    obj_attrA FSHGRD = [NOBJNM,OBJNAM,PEREND,PERSTA,STATUS]
    obj_attrA FLODOC = [COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,DRVAL1,HORACC,HORCLR,HORLEN,HORWID,LIFCAP,NOBJNM,OBJNAM,STATUS,VERACC,VERLEN,VERDAT]
    obj_attrA FOGSIG = [CATFOG,DATEND,DATSTA,NOBJNM,OBJNAM,SIGFRQ,SIGGEN,SIGGRP,SIGPER,SIGSEQ,STATUS,VALMXR]
    obj_attrA FORSTC = [CATFOR,CONDTN,CONRAD,CONVIS,HEIGHT,NATCON,NOBJNM,OBJNAM,VERACC,VERDAT,VERLEN]
    obj_attrA FRPARE = [NOBJNM,OBJNAM,STATUS]
    obj_attrA GATCON = [CATGAT,CONDTN,DRVAL1,HORACC,HORCLR,NATCON,NOBJNM,OBJNAM,QUASOU,SOUACC,STATUS,VERACC,VERCLR,VERDAT]
    obj_attrA GRIDRN = [HORACC,HORLEN,HORWID,NATCON,NOBJNM,OBJNAM,STATUS,VERACC,VERLEN,WATLEV]
    obj_attrA HRBARE = [NOBJNM,OBJNAM,STATUS]
    obj_attrA HRBFAC = [CATHAF,CONDTN,DATEND,DATSTA,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS]
    obj_attrA HULKES = [CATHLK,COLOUR,COLPAT,CONRAD,CONVIS,HORACC,HORLEN,HORWID,NOBJNM,OBJNAM,VERACC,VERLEN,CONDTN]
    obj_attrA ICEARE = [CATICE,CONVIS,ELEVAT,HEIGHT,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERDAT,VERLEN]
    obj_attrA ICNARE = [NOBJNM,OBJNAM,PEREND,PERSTA,RESTRN,STATUS]
    obj_attrA ISTZNE = [CATTSS,DATEND,DATSTA,RESTRN,STATUS]
    obj_attrA LAKARE = [ELEVAT,NOBJNM,OBJNAM,VERACC,VERDAT]
    obj_attrA LNDARE = [CONDTN,NOBJNM,OBJNAM,STATUS]
    obj_attrA LNDELV = [CONVIS,ELEVAT,NOBJNM,OBJNAM,VERACC,VERDAT]
    obj_attrA LNDRGN = [CATLND,NATQUA,NATSUR,NOBJNM,OBJNAM,WATLEV]
    obj_attrA LNDMRK = [CATLMK,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,ELEVAT,FUNCTN,HEIGHT,NATCON,NOBJNM,OBJNAM,STATUS,VERACC,VERDAT,VERLEN]
    obj_attrA LIGHTS = [CATLIT,COLOUR,DATEND,DATSTA,EXCLIT,HEIGHT,LITCHR,LITVIS,MARSYS,MLTYLT,NOBJNM,OBJNAM,ORIENT,PEREND,PERSTA,SECTR1,SECTR2,SIGGRP,SIGPER,SIGSEQ,STATUS,VERACC,VALNMR,VERDAT]
    obj_attrA LITFLT = [COLOUR,COLPAT,CONRAD,CONVIS,DATEND,DATSTA,HORACC,HORLEN,HORWID,MARSYS,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERLEN]
    obj_attrA LITVES = [COLOUR,COLPAT,CONRAD,CONVIS,DATEND,DATSTA,HORACC,HORLEN,HORWID,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERLEN]
    obj_attrA LOCMAG = [NOBJNM,OBJNAM,VALLMA]
    obj_attrA LOKBSN = [DATEND,DATSTA,HORACC,HORCLR,HORLEN,HORWID,NOBJNM,OBJNAM,STATUS]
    obj_attrA LOGPON = [NOBJNM,OBJNAM,STATUS]
    obj_attrA MAGVAR = [DATEND,DATSTA,RYRMGV,VALACM,VALMAG]
    obj_attrA MARCUL = [CATMFA,DATEND,DATSTA,EXPSOU,NOBJNM,OBJNAM,PEREND,PERSTA,QUASOU,RESTRN,SOUACC,STATUS,VALSOU,VERACC,VERDAT,VERLEN,WATLEV]
    obj_attrA MIPARE = [CATMPA,DATEND,DATSTA,NOBJNM,OBJNAM,PEREND,PERSTA,RESTRN,STATUS]
    obj_attrA MORFAC = [BOYSHP,CATMOR,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,HEIGHT,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERDAT,VERLEN,WATLEV]
    obj_attrA NAVLNE = [CATNAV,DATEND,DATSTA,ORIENT,PEREND,PERSTA,STATUS]
    obj_attrA OBSTRN = [CATOBS,CONDTN,EXPSOU,HEIGHT,NATCON,NATQUA,NOBJNM,OBJNAM,PRODCT,QUASOU,SOUACC,STATUS,TECSOU,VALSOU,VERACC,VERDAT,VERLEN,WATLEV,NATSUR]
    obj_attrA OFSPLF = [CATOFP,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,HEIGHT,NATCON,NOBJNM,OBJNAM,PRODCT,STATUS,VERACC,VERDAT,VERLEN]
    obj_attrA OSPARE = [CATPRA,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,HEIGHT,NOBJNM,OBJNAM,PRODCT,RESTRN,STATUS,VERACC,VERLEN]
    obj_attrA OILBAR = [CATOLB,CONDTN,DATEND,DATSTA,NOBJNM,OBJNAM,STATUS]
    obj_attrA PILPNT = [CATPLE,COLOUR,COLPAT,CONDTN,CONVIS,DATEND,DATSTA,HEIGHT,NOBJNM,OBJNAM,VERACC,VERDAT,VERLEN]
    obj_attrA PILBOP = [CATPIL,COMCHA,DATEND,DATSTA,NOBJNM,NPLDST,OBJNAM,PEREND,PERSTA,PILDST,STATUS]
    obj_attrA PIPARE = [CONDTN,DATEND,DATSTA,NOBJNM,OBJNAM,PRODCT,RESTRN,STATUS,CATPIP]
    obj_attrA PIPOHD = [CATPIP,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,NOBJNM,OBJNAM,PRODCT,STATUS,VERACC,VERCLR,VERDAT]
    obj_attrA PIPSOL = [BURDEP,CATPIP,CONDTN,DATEND,DATSTA,DRVAL1,DRVAL2,NOBJNM,OBJNAM,PRODCT,STATUS,VERACC,VERLEN,VERDAT]
    obj_attrA PONTON = [CONDTN,CONRAD,CONVIS,DATEND,DATSTA,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERLEN]
    obj_attrA PRCARE = [DATEND,DATSTA,RESTRN,STATUS]
    obj_attrA PRDARE = [CATPRA,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,ELEVAT,HEIGHT,NOBJNM,OBJNAM,PRODCT,STATUS,VERACC,VERDAT,VERLEN]
    obj_attrA PYLONS = [CATPYL,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,HEIGHT,NATCON,NOBJNM,OBJNAM,VERACC,VERDAT,VERLEN,WATLEV]
    obj_attrA RADLNE = [NOBJNM,OBJNAM,ORIENT,STATUS]
    obj_attrA RADRNG = [COMCHA,DATEND,DATSTA,NOBJNM,OBJNAM,STATUS]
    obj_attrA RADRFL = [HEIGHT,STATUS,VERACC,VERDAT]
    obj_attrA RADSTA = [CATRAS,DATEND,DATSTA,HEIGHT,NOBJNM,OBJNAM,STATUS,VERACC,VALMXR,VERDAT]
    obj_attrA RTPBCN = [CATRTB,DATEND,DATSTA,NOBJNM,OBJNAM,RADWAL,SECTR1,SECTR2,SIGGRP,SIGSEQ,STATUS,VALMXR]
    obj_attrA RDOCAL = [COMCHA,DATEND,DATSTA,NOBJNM,OBJNAM,ORIENT,PEREND,PERSTA,STATUS,TRAFIC]
    obj_attrA RDOSTA = [CALSGN,CATROS,COMCHA,DATEND,DATSTA,ESTRNG,NOBJNM,OBJNAM,ORIENT,PEREND,PERSTA,SIGFRQ,STATUS]
    obj_attrA RAILWY = [CONDTN,HEIGHT,NOBJNM,OBJNAM,STATUS,VERACC]
    obj_attrA RAPIDS = [NOBJNM,OBJNAM,VERACC,VERLEN]
    obj_attrA RCRTCL = [CATTRK,DATEND,DATSTA,DRVAL1,DRVAL2,NOBJNM,OBJNAM,ORIENT,PEREND,PERSTA,QUASOU,SOUACC,STATUS,TECSOU,TRAFIC,VERDAT]
    obj_attrA RECTRC = [CATTRK,DATEND,DATSTA,DRVAL1,DRVAL2,NOBJNM,OBJNAM,ORIENT,PEREND,PERSTA,QUASOU,SOUACC,STATUS,TECSOU,TRAFIC,VERDAT]
    obj_attrA RCTLPT = [DATEND,DATSTA,ORIENT,STATUS]
    obj_attrA RSCSTA = [CATRSC,DATEND,DATSTA,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS]
    obj_attrA RESARE = [CATREA,DATEND,DATSTA,NOBJNM,OBJNAM,PEREND,PERSTA,RESTRN,STATUS]
    obj_attrA RETRFL = [COLOUR,COLPAT,HEIGHT,MARSYS,STATUS,VERACC,VERDAT]
    obj_attrA RIVERS = [NOBJNM,OBJNAM,STATUS]
    obj_attrA ROADWY = [CATROD,CONDTN,NATCON,NOBJNM,OBJNAM,STATUS]
    obj_attrA RUNWAY = [CATRUN,CONDTN,CONVIS,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS]
    obj_attrA SNDWAV = [VERACC,VERLEN]
    obj_attrA SEAARE = [CATSEA,NOBJNM,OBJNAM]
    obj_attrA SPLARE = [NOBJNM,OBJNAM,PEREND,PERSTA,RESTRN,STATUS]
    obj_attrA SBDARE = [COLOUR,NATQUA,NATSUR,WATLEV,OBJNAM,NOBJNM]
    obj_attrA SLCONS = [CATSLC,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,HEIGHT,HORACC,HORCLR,HORLEN,HORWID,NATCON,NOBJNM,OBJNAM,STATUS,VERACC,VERDAT,VERLEN,WATLEV]
    obj_attrA SISTAT = [CATSIT,COMCHA,DATEND,DATSTA,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS]
    obj_attrA SISTAW = [CATSIW,COMCHA,DATEND,DATSTA,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS]
    obj_attrA SILTNK = [BUISHP,CATSIL,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,ELEVAT,HEIGHT,NATCON,NOBJNM,OBJNAM,PRODCT,STATUS,VERACC,VERDAT,VERLEN]
    obj_attrA SLOTOP = [CATSLO,COLOUR,CONRAD,CONVIS,ELEVAT,NATCON,NATQUA,NATSUR,NOBJNM,OBJNAM,VERACC,VERDAT]
    obj_attrA SLOGRD = [CATSLO,COLOUR,CONRAD,CONVIS,NATCON,NATQUA,NATSUR,NOBJNM,OBJNAM]
    obj_attrA SMCFAC = [CATSCF,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS]
    obj_attrA SOUNDG = [EXPSOU,NOBJNM,OBJNAM,QUASOU,SOUACC,TECSOU,VERDAT,STATUS]
    obj_attrA SPRING = [NOBJNM,OBJNAM]
    obj_attrA STSLNE = [NATION]
    obj_attrA SUBTLN = [NOBJNM,OBJNAM,RESTRN]
    obj_attrA SWPARE = [DRVAL1,QUASOU,SOUACC,TECSOU,VERDAT]
    obj_attrA TESARE = [NATION,RESTRN]
    obj_attrA TS_PRH = [NOBJNM,OBJNAM,T_MTOD,T_VAHC,STATUS]
    obj_attrA TS_PNH = [NOBJNM,OBJNAM,T_MTOD,T_THDF,STATUS]
    obj_attrA TS_PAD = [NOBJNM,OBJNAM,TS_TSP]
    obj_attrA TS_TIS = [NOBJNM,OBJNAM,STATUS,TIMEND,TIMSTA,T_TINT,TS_TSV]
    obj_attrA T_HMON = [NOBJNM,OBJNAM,T_ACWL,T_MTOD,T_VAHC,STATUS]
    obj_attrA T_NHMN = [NOBJNM,OBJNAM,T_ACWL,T_MTOD,T_THDF,STATUS]
    obj_attrA T_TIMS = [NOBJNM,OBJNAM,T_HWLW,T_TINT,T_TSVL,TIMEND,TIMSTA,STATUS,T_ACWL]
    obj_attrA TIDEWY = [NOBJNM,OBJNAM]
    obj_attrA TOPMAR = [COLOUR,COLPAT,HEIGHT,MARSYS,STATUS,TOPSHP,VERACC,VERDAT,VERLEN]
    obj_attrA TSELNE = [CATTSS,DATEND,DATSTA,STATUS]
    obj_attrA TSSBND = [CATTSS,DATEND,DATSTA,STATUS]
    obj_attrA TSSCRS = [CATTSS,DATEND,DATSTA,RESTRN,STATUS]
    obj_attrA TSSLPT = [CATTSS,DATEND,DATSTA,ORIENT,RESTRN,STATUS]
    obj_attrA TSSRON = [CATTSS,DATEND,DATSTA,RESTRN,STATUS]
    obj_attrA TSEZNE = [CATTSS,DATEND,DATSTA,STATUS]
    obj_attrA TUNNEL = [BURDEP,CONDTN,HORACC,HORCLR,NOBJNM,OBJNAM,STATUS,VERACC,VERCLR]
    obj_attrA TWRTPT = [CATTRK,DATEND,DATSTA,DRVAL1,DRVAL2,ORIENT,QUASOU,SOUACC,STATUS,TECSOU,TRAFIC,VERDAT]
    obj_attrA UWTROC = [EXPSOU,NATSUR,NATQUA,NOBJNM,OBJNAM,QUASOU,SOUACC,STATUS,TECSOU,VALSOU,VERDAT,WATLEV]
    obj_attrA UNSARE = []
    obj_attrA VEGATN = [CATVEG,CONVIS,ELEVAT,HEIGHT,NOBJNM,OBJNAM,VERACC,VERDAT,VERLEN]
    obj_attrA WATTUR = [CATWAT,NOBJNM,OBJNAM]
    obj_attrA WATFAL = [CONVIS,NOBJNM,OBJNAM,VERACC,VERLEN]
    obj_attrA WEDKLP = [CATWED,NOBJNM,OBJNAM]
    obj_attrA WRECKS = [CATWRK,CONRAD,CONVIS,EXPSOU,HEIGHT,NOBJNM,OBJNAM,QUASOU,SOUACC,STATUS,TECSOU,VALSOU,VERACC,VERDAT,VERLEN,WATLEV]
    obj_attrA M_ACCY = [HORACC,POSACC,SOUACC,VERACC]
    obj_attrA M_CSCL = [CSCALE]
    obj_attrA M_COVR = [CATCOV]
    obj_attrA M_HDAT = [HORDAT]
    obj_attrA M_HOPA = [HORDAT,SHIPAM]
    obj_attrA M_NPUB = []
    obj_attrA M_NSYS = [MARSYS,ORIENT]
    obj_attrA M_PROD = [AGENCY,CPDATE,NATION,NMDATE,PRCTRY]
    obj_attrA M_QUAL = [CATQUA,CATZOC,DRVAL1,DRVAL2,POSACC,SOUACC,SUREND,SURSTA,TECSOU,VERDAT]
    obj_attrA M_SDAT = [VERDAT]
    obj_attrA M_SREL = [QUAPOS,QUASOU,SCVAL1,SCVAL2,SDISMN,SDISMX,SURATH,SUREND,SURSTA,SURTYP,TECSOU]
    obj_attrA M_UNIT = [DUNITS,HUNITS,PUNITS]
    obj_attrA M_VDAT = [VERDAT]
    obj_attrA C_AGGR = [NOBJNM,OBJNAM]
    obj_attrA C_ASSO = [NOBJNM,OBJNAM]
    obj_attrA C_STAC = []
    obj_attrA AREAS = [COLOUR,ORIENT,SCODE,TINTS]
    obj_attrA LINES = [SCODE]
    obj_attrA CSYMB = [ORIENT,SCALE,SCODE]
    obj_attrA COMPS = [CSIZE,RYRMGV,VALACM,VALMAG]
    obj_attrA TEXTS = [CHARS,COLOUR,JUSTH,JUSTV,NTXST,SPACE,TXSTR]
    obj_class ADMARE = "Administration area (Named)"
    obj_class AIRARE = "Airport / airfield"
    obj_class ACHBRT = "Anchor berth"
    obj_class ACHARE = "Anchorage area"
    obj_class BCNCAR = "Beacon cardinal"
    obj_class BCNISD = "Beacon isolated danger"
    obj_class BCNLAT = "Beacon lateral"
    obj_class BCNSAW = "Beacon safe water"
    obj_class BCNSPP = "Beacon special purpose/general"
    obj_class BERTHS = "Berth"
    obj_class BRIDGE = "Bridge"
    obj_class BUISGL = "Building single"
    obj_class BUAARE = "Built-up area"
    obj_class BOYCAR = "Buoy cardinal"
    obj_class BOYINB = "Buoy installation"
    obj_class BOYISD = "Buoy isolated danger"
    obj_class BOYLAT = "Buoy lateral"
    obj_class BOYSAW = "Buoy safe water"
    obj_class BOYSPP = "Buoy special purpose/general"
    obj_class CBLARE = "Cable area"
    obj_class CBLOHD = "Cable overhead"
    obj_class CBLSUB = "Cable submarine"
    obj_class CANALS = "Canal"
    obj_class CTSARE = "Cargo transshipment area"
    obj_class CAUSWY = "Causeway"
    obj_class CTNARE = "Caution area"
    obj_class CHKPNT = "Checkpoint"
    obj_class CGUSTA = "Coastguard station"
    obj_class COALNE = "Coastline"
    obj_class CONZNE = "Contiguous zone"
    obj_class COSARE = "Continental shelf area"
    obj_class CTRPNT = "Control point"
    obj_class CONVYR = "Conveyor"
    obj_class CRANES = "Crane"
    obj_class CURENT = "Current - non - gravitational"
    obj_class CUSZNE = "Custom zone"
    obj_class DAMCON = "Dam"
    obj_class DAYMAR = "Daymark"
    obj_class DWRTCL = "Deep water route centerline"
    obj_class DWRTPT = "Deep water route part"
    obj_class DEPARE = "Depth area"
    obj_class DEPCNT = "Depth contour"
    obj_class DISMAR = "Distance mark"
    obj_class DOCARE = "Dock area"
    obj_class DRGARE = "Dredged area"
    obj_class DRYDOC = "Dry dock"
    obj_class DMPGRD = "Dumping ground"
    obj_class DYKCON = "Dyke"
    obj_class EXEZNE = "Exclusive Economic Zone"
    obj_class FAIRWY = "Fairway"
    obj_class FNCLNE = "Fence/wall"
    obj_class FERYRT = "Ferry route"
    obj_class FSHZNE = "Fishery zone"
    obj_class FSHFAC = "Fishing facility"
    obj_class FSHGRD = "Fishing ground"
    obj_class FLODOC = "Floating dock"
    obj_class FOGSIG = "Fog signal"
    obj_class FORSTC = "Fortified structure"
    obj_class FRPARE = "Free port area"
    obj_class GATCON = "Gate"
    obj_class GRIDRN = "Gridiron"
    obj_class HRBARE = "Harbour area (administrative)"
    obj_class HRBFAC = "Harbour facility"
    obj_class HULKES = "Hulk"
    obj_class ICEARE = "Ice area"
    obj_class ICNARE = "Incineration area"
    obj_class ISTZNE = "Inshore traffic zone"
    obj_class LAKARE = "Lake"
    obj_class LNDARE = "Land area"
    obj_class LNDELV = "Land elevation"
    obj_class LNDRGN = "Land region"
    obj_class LNDMRK = "Landmark"
    obj_class LIGHTS = "Light"
    obj_class LITFLT = "Light float"
    obj_class LITVES = "Light vessel"
    obj_class LOCMAG = "Local magnetic anomaly"
    obj_class LOKBSN = "Lock basin"
    obj_class LOGPON = "Log pond"
    obj_class MAGVAR = "Magnetic variation"
    obj_class MARCUL = "Marine farm/culture"
    obj_class MIPARE = "Military practice area"
    obj_class MORFAC = "Mooring/warping facility"
    obj_class NAVLNE = "Navigation line"
    obj_class OBSTRN = "Obstruction"
    obj_class OFSPLF = "Offshore platform"
    obj_class OSPARE = "Offshore production area"
    obj_class OILBAR = "Oil barrier"
    obj_class PILPNT = "Pile"
    obj_class PILBOP = "Pilot boarding place"
    obj_class PIPARE = "Pipeline area"
    obj_class PIPOHD = "Pipeline overhead"
    obj_class PIPSOL = "Pipeline submarine/on land"
    obj_class PONTON = "Pontoon"
    obj_class PRCARE = "Precautionary area"
    obj_class PRDARE = "Production / storage area"
    obj_class PYLONS = "Pylon/bridge support"
    obj_class RADLNE = "Radar line"
    obj_class RADRNG = "Radar range"
    obj_class RADRFL = "Radar reflector"
    obj_class RADSTA = "Radar station"
    obj_class RTPBCN = "Radar transponder beacon"
    obj_class RDOCAL = "Radio calling-in point"
    obj_class RDOSTA = "Radio station"
    obj_class RAILWY = "Railway"
    obj_class RAPIDS = "Rapids"
    obj_class RCRTCL = "Recommended route centerline"
    obj_class RECTRC = "Recommended track"
    obj_class RCTLPT = "Recommended Traffic Lane Part"
    obj_class RSCSTA = "Rescue station"
    obj_class RESARE = "Restricted area"
    obj_class RETRFL = "Retro-reflector"
    obj_class RIVERS = "River"
    obj_class ROADWY = "Road"
    obj_class RUNWAY = "Runway"
    obj_class SNDWAV = "Sand waves"
    obj_class SEAARE = "Sea area / named water area"
    obj_class SPLARE = "Sea-plane landing area"
    obj_class SBDARE = "Seabed area"
    obj_class SLCONS = "Shoreline Construction"
    obj_class SISTAT = "Signal station traffic"
    obj_class SISTAW = "Signal station warning"
    obj_class SILTNK = "Silo / tank"
    obj_class SLOTOP = "Slope topline"
    obj_class SLOGRD = "Sloping ground"
    obj_class SMCFAC = "Small craft facility"
    obj_class SOUNDG = "Sounding"
    obj_class SPRING = "Spring"
    obj_class STSLNE = "Straight territorial sea baseline"
    obj_class SUBTLN = "Submarine transit lane"
    obj_class SWPARE = "Swept Area"
    obj_class TESARE = "Territorial sea area"
    obj_class TS_PRH = "Tidal stream - harmonic prediction"
    obj_class TS_PNH = "Tidal stream - non-harmonic prediction"
    obj_class TS_PAD = "Tidal stream panel data"
    obj_class TS_TIS = "Tidal stream - time series"
    obj_class T_HMON = "Tide - harmonic prediction"
    obj_class T_NHMN = "Tide - non-harmonic prediction"
    obj_class T_TIMS = "Tidal stream - time series"
    obj_class TIDEWY = "Tideway"
    obj_class TOPMAR = "Top mark"
    obj_class TSELNE = "Traffic Separation Line"
    obj_class TSSBND = "Traffic Separation Scheme  Boundary"
    obj_class TSSCRS = "Traffic Separation Scheme Crossing"
    obj_class TSSLPT = "Traffic Separation Scheme  Lane part"
    obj_class TSSRON = "Traffic Separation Scheme  Roundabout"
    obj_class TSEZNE = "Traffic Separation Zone"
    obj_class TUNNEL = "Tunnel"
    obj_class TWRTPT = "Two-way route  part"
    obj_class UWTROC = "Underwater rock / awash rock"
    obj_class UNSARE = "Unsurveyed area"
    obj_class VEGATN = "Vegetation"
    obj_class WATTUR = "Water turbulence"
    obj_class WATFAL = "Waterfall"
    obj_class WEDKLP = "Weed/Kelp"
    obj_class WRECKS = "Wreck"
    obj_class M_ACCY = "Accuracy of data"
    obj_class M_CSCL = "Compilation scale of data"
    obj_class M_COVR = "Coverage"
    obj_class M_HDAT = "Horizontal datum of data"
    obj_class M_HOPA = "Horizontal datum shift parameters"
    obj_class M_NPUB = "Nautical publication information"
    obj_class M_NSYS = "Navigational system of marks"
    obj_class M_PROD = "Production information"
    obj_class M_QUAL = "Quality of data"
    obj_class M_SDAT = "Sounding datum"
    obj_class M_SREL = "Survey reliability"
    obj_class M_UNIT = "Units of measurement of data"
    obj_class M_VDAT = "Vertical datum of data"
    obj_class C_AGGR = "Aggregation"
    obj_class C_ASSO = "Association"
    obj_class C_STAC = "Stacked on/stacked under"
    obj_class AREAS = "Cartographic area"
    obj_class LINES = "Cartographic line"
    obj_class CSYMB = "Cartographic symbol"
    obj_class COMPS = "Compass"
    obj_class TEXTS = "Text"
    obj_code ADMARE = 1
    obj_code AIRARE = 2
    obj_code ACHBRT = 3
    obj_code ACHARE = 4
    obj_code BCNCAR = 5
    obj_code BCNISD = 6
    obj_code BCNLAT = 7
    obj_code BCNSAW = 8
    obj_code BCNSPP = 9
    obj_code BERTHS = 10
    obj_code BRIDGE = 11
    obj_code BUISGL = 12
    obj_code BUAARE = 13
    obj_code BOYCAR = 14
    obj_code BOYINB = 15
    obj_code BOYISD = 16
    obj_code BOYLAT = 17
    obj_code BOYSAW = 18
    obj_code BOYSPP = 19
    obj_code CBLARE = 20
    obj_code CBLOHD = 21
    obj_code CBLSUB = 22
    obj_code CANALS = 23
    obj_code CTSARE = 25
    obj_code CAUSWY = 26
    obj_code CTNARE = 27
    obj_code CHKPNT = 28
    obj_code CGUSTA = 29
    obj_code COALNE = 30
    obj_code CONZNE = 31
    obj_code COSARE = 32
    obj_code CTRPNT = 33
    obj_code CONVYR = 34
    obj_code CRANES = 35
    obj_code CURENT = 36
    obj_code CUSZNE = 37
    obj_code DAMCON = 38
    obj_code DAYMAR = 39
    obj_code DWRTCL = 40
    obj_code DWRTPT = 41
    obj_code DEPARE = 42
    obj_code DEPCNT = 43
    obj_code DISMAR = 44
    obj_code DOCARE = 45
    obj_code DRGARE = 46
    obj_code DRYDOC = 47
    obj_code DMPGRD = 48
    obj_code DYKCON = 49
    obj_code EXEZNE = 50
    obj_code FAIRWY = 51
    obj_code FNCLNE = 52
    obj_code FERYRT = 53
    obj_code FSHZNE = 54
    obj_code FSHFAC = 55
    obj_code FSHGRD = 56
    obj_code FLODOC = 57
    obj_code FOGSIG = 58
    obj_code FORSTC = 59
    obj_code FRPARE = 60
    obj_code GATCON = 61
    obj_code GRIDRN = 62
    obj_code HRBARE = 63
    obj_code HRBFAC = 64
    obj_code HULKES = 65
    obj_code ICEARE = 66
    obj_code ICNARE = 67
    obj_code ISTZNE = 68
    obj_code LAKARE = 69
    obj_code LNDARE = 71
    obj_code LNDELV = 72
    obj_code LNDRGN = 73
    obj_code LNDMRK = 74
    obj_code LIGHTS = 75
    obj_code LITFLT = 76
    obj_code LITVES = 77
    obj_code LOCMAG = 78
    obj_code LOKBSN = 79
    obj_code LOGPON = 80
    obj_code MAGVAR = 81
    obj_code MARCUL = 82
    obj_code MIPARE = 83
    obj_code MORFAC = 84
    obj_code NAVLNE = 85
    obj_code OBSTRN = 86
    obj_code OFSPLF = 87
    obj_code OSPARE = 88
    obj_code OILBAR = 89
    obj_code PILPNT = 90
    obj_code PILBOP = 91
    obj_code PIPARE = 92
    obj_code PIPOHD = 93
    obj_code PIPSOL = 94
    obj_code PONTON = 95
    obj_code PRCARE = 96
    obj_code PRDARE = 97
    obj_code PYLONS = 98
    obj_code RADLNE = 99
    obj_code RADRNG = 100
    obj_code RADRFL = 101
    obj_code RADSTA = 102
    obj_code RTPBCN = 103
    obj_code RDOCAL = 104
    obj_code RDOSTA = 105
    obj_code RAILWY = 106
    obj_code RAPIDS = 107
    obj_code RCRTCL = 108
    obj_code RECTRC = 109
    obj_code RCTLPT = 110
    obj_code RSCSTA = 111
    obj_code RESARE = 112
    obj_code RETRFL = 113
    obj_code RIVERS = 114
    obj_code ROADWY = 116
    obj_code RUNWAY = 117
    obj_code SNDWAV = 118
    obj_code SEAARE = 119
    obj_code SPLARE = 120
    obj_code SBDARE = 121
    obj_code SLCONS = 122
    obj_code SISTAT = 123
    obj_code SISTAW = 124
    obj_code SILTNK = 125
    obj_code SLOTOP = 126
    obj_code SLOGRD = 127
    obj_code SMCFAC = 128
    obj_code SOUNDG = 129
    obj_code SPRING = 130
    obj_code STSLNE = 132
    obj_code SUBTLN = 133
    obj_code SWPARE = 134
    obj_code TESARE = 135
    obj_code TS_PRH = 136
    obj_code TS_PNH = 137
    obj_code TS_PAD = 138
    obj_code TS_TIS = 139
    obj_code T_HMON = 140
    obj_code T_NHMN = 141
    obj_code T_TIMS = 142
    obj_code TIDEWY = 143
    obj_code TOPMAR = 144
    obj_code TSELNE = 145
    obj_code TSSBND = 146
    obj_code TSSCRS = 147
    obj_code TSSLPT = 148
    obj_code TSSRON = 149
    obj_code TSEZNE = 150
    obj_code TUNNEL = 151
    obj_code TWRTPT = 152
    obj_code UWTROC = 153
    obj_code UNSARE = 154
    obj_code VEGATN = 155
    obj_code WATTUR = 156
    obj_code WATFAL = 157
    obj_code WEDKLP = 158
    obj_code WRECKS = 159
    obj_code M_ACCY = 300
    obj_code M_CSCL = 301
    obj_code M_COVR = 302
    obj_code M_HDAT = 303
    obj_code M_HOPA = 304
    obj_code M_NPUB = 305
    obj_code M_NSYS = 306
    obj_code M_PROD = 307
    obj_code M_QUAL = 308
    obj_code M_SDAT = 309
    obj_code M_SREL = 310
    obj_code M_UNIT = 311
    obj_code M_VDAT = 312
    obj_code C_AGGR = 400
    obj_code C_ASSO = 401
    obj_code C_STAC = 402
    obj_code AREAS = 500
    obj_code LINES = 501
    obj_code CSYMB = 502
    obj_code COMPS = 503
    obj_code TEXTS = 504
    obj_fromCode 1 = ADMARE
    obj_fromCode 2 = AIRARE
    obj_fromCode 3 = ACHBRT
    obj_fromCode 4 = ACHARE
    obj_fromCode 5 = BCNCAR
    obj_fromCode 6 = BCNISD
    obj_fromCode 7 = BCNLAT
    obj_fromCode 8 = BCNSAW
    obj_fromCode 9 = BCNSPP
    obj_fromCode 10 = BERTHS
    obj_fromCode 11 = BRIDGE
    obj_fromCode 12 = BUISGL
    obj_fromCode 13 = BUAARE
    obj_fromCode 14 = BOYCAR
    obj_fromCode 15 = BOYINB
    obj_fromCode 16 = BOYISD
    obj_fromCode 17 = BOYLAT
    obj_fromCode 18 = BOYSAW
    obj_fromCode 19 = BOYSPP
    obj_fromCode 20 = CBLARE
    obj_fromCode 21 = CBLOHD
    obj_fromCode 22 = CBLSUB
    obj_fromCode 23 = CANALS
    obj_fromCode 25 = CTSARE
    obj_fromCode 26 = CAUSWY
    obj_fromCode 27 = CTNARE
    obj_fromCode 28 = CHKPNT
    obj_fromCode 29 = CGUSTA
    obj_fromCode 30 = COALNE
    obj_fromCode 31 = CONZNE
    obj_fromCode 32 = COSARE
    obj_fromCode 33 = CTRPNT
    obj_fromCode 34 = CONVYR
    obj_fromCode 35 = CRANES
    obj_fromCode 36 = CURENT
    obj_fromCode 37 = CUSZNE
    obj_fromCode 38 = DAMCON
    obj_fromCode 39 = DAYMAR
    obj_fromCode 40 = DWRTCL
    obj_fromCode 41 = DWRTPT
    obj_fromCode 42 = DEPARE
    obj_fromCode 43 = DEPCNT
    obj_fromCode 44 = DISMAR
    obj_fromCode 45 = DOCARE
    obj_fromCode 46 = DRGARE
    obj_fromCode 47 = DRYDOC
    obj_fromCode 48 = DMPGRD
    obj_fromCode 49 = DYKCON
    obj_fromCode 50 = EXEZNE
    obj_fromCode 51 = FAIRWY
    obj_fromCode 52 = FNCLNE
    obj_fromCode 53 = FERYRT
    obj_fromCode 54 = FSHZNE
    obj_fromCode 55 = FSHFAC
    obj_fromCode 56 = FSHGRD
    obj_fromCode 57 = FLODOC
    obj_fromCode 58 = FOGSIG
    obj_fromCode 59 = FORSTC
    obj_fromCode 60 = FRPARE
    obj_fromCode 61 = GATCON
    obj_fromCode 62 = GRIDRN
    obj_fromCode 63 = HRBARE
    obj_fromCode 64 = HRBFAC
    obj_fromCode 65 = HULKES
    obj_fromCode 66 = ICEARE
    obj_fromCode 67 = ICNARE
    obj_fromCode 68 = ISTZNE
    obj_fromCode 69 = LAKARE
    obj_fromCode 71 = LNDARE
    obj_fromCode 72 = LNDELV
    obj_fromCode 73 = LNDRGN
    obj_fromCode 74 = LNDMRK
    obj_fromCode 75 = LIGHTS
    obj_fromCode 76 = LITFLT
    obj_fromCode 77 = LITVES
    obj_fromCode 78 = LOCMAG
    obj_fromCode 79 = LOKBSN
    obj_fromCode 80 = LOGPON
    obj_fromCode 81 = MAGVAR
    obj_fromCode 82 = MARCUL
    obj_fromCode 83 = MIPARE
    obj_fromCode 84 = MORFAC
    obj_fromCode 85 = NAVLNE
    obj_fromCode 86 = OBSTRN
    obj_fromCode 87 = OFSPLF
    obj_fromCode 88 = OSPARE
    obj_fromCode 89 = OILBAR
    obj_fromCode 90 = PILPNT
    obj_fromCode 91 = PILBOP
    obj_fromCode 92 = PIPARE
    obj_fromCode 93 = PIPOHD
    obj_fromCode 94 = PIPSOL
    obj_fromCode 95 = PONTON
    obj_fromCode 96 = PRCARE
    obj_fromCode 97 = PRDARE
    obj_fromCode 98 = PYLONS
    obj_fromCode 99 = RADLNE
    obj_fromCode 100 = RADRNG
    obj_fromCode 101 = RADRFL
    obj_fromCode 102 = RADSTA
    obj_fromCode 103 = RTPBCN
    obj_fromCode 104 = RDOCAL
    obj_fromCode 105 = RDOSTA
    obj_fromCode 106 = RAILWY
    obj_fromCode 107 = RAPIDS
    obj_fromCode 108 = RCRTCL
    obj_fromCode 109 = RECTRC
    obj_fromCode 110 = RCTLPT
    obj_fromCode 111 = RSCSTA
    obj_fromCode 112 = RESARE
    obj_fromCode 113 = RETRFL
    obj_fromCode 114 = RIVERS
    obj_fromCode 116 = ROADWY
    obj_fromCode 117 = RUNWAY
    obj_fromCode 118 = SNDWAV
    obj_fromCode 119 = SEAARE
    obj_fromCode 120 = SPLARE
    obj_fromCode 121 = SBDARE
    obj_fromCode 122 = SLCONS
    obj_fromCode 123 = SISTAT
    obj_fromCode 124 = SISTAW
    obj_fromCode 125 = SILTNK
    obj_fromCode 126 = SLOTOP
    obj_fromCode 127 = SLOGRD
    obj_fromCode 128 = SMCFAC
    obj_fromCode 129 = SOUNDG
    obj_fromCode 130 = SPRING
    obj_fromCode 132 = STSLNE
    obj_fromCode 133 = SUBTLN
    obj_fromCode 134 = SWPARE
    obj_fromCode 135 = TESARE
    obj_fromCode 136 = TS_PRH
    obj_fromCode 137 = TS_PNH
    obj_fromCode 138 = TS_PAD
    obj_fromCode 139 = TS_TIS
    obj_fromCode 140 = T_HMON
    obj_fromCode 141 = T_NHMN
    obj_fromCode 142 = T_TIMS
    obj_fromCode 143 = TIDEWY
    obj_fromCode 144 = TOPMAR
    obj_fromCode 145 = TSELNE
    obj_fromCode 146 = TSSBND
    obj_fromCode 147 = TSSCRS
    obj_fromCode 148 = TSSLPT
    obj_fromCode 149 = TSSRON
    obj_fromCode 150 = TSEZNE
    obj_fromCode 151 = TUNNEL
    obj_fromCode 152 = TWRTPT
    obj_fromCode 153 = UWTROC
    obj_fromCode 154 = UNSARE
    obj_fromCode 155 = VEGATN
    obj_fromCode 156 = WATTUR
    obj_fromCode 157 = WATFAL
    obj_fromCode 158 = WEDKLP
    obj_fromCode 159 = WRECKS
    obj_fromCode 300 = M_ACCY
    obj_fromCode 301 = M_CSCL
    obj_fromCode 302 = M_COVR
    obj_fromCode 303 = M_HDAT
    obj_fromCode 304 = M_HOPA
    obj_fromCode 305 = M_NPUB
    obj_fromCode 306 = M_NSYS
    obj_fromCode 307 = M_PROD
    obj_fromCode 308 = M_QUAL
    obj_fromCode 309 = M_SDAT
    obj_fromCode 310 = M_SREL
    obj_fromCode 311 = M_UNIT
    obj_fromCode 312 = M_VDAT
    obj_fromCode 400 = C_AGGR
    obj_fromCode 401 = C_ASSO
    obj_fromCode 402 = C_STAC
    obj_fromCode 500 = AREAS
    obj_fromCode 501 = LINES
    obj_fromCode 502 = CSYMB
    obj_fromCode 503 = COMPS
    obj_fromCode 504 = TEXTS
    obj_fromCode i = error $ "obj_fromCode: unknown object code/label: " ++ show i
