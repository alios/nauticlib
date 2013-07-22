{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
    obj_code :: t -> Integer
    obj_class :: t -> String
    obj_attrA :: t -> [AttributeT]
    obj_attrB :: t -> [AttributeT]
    obj_attrC :: t -> [AttributeT]
    obj_classTag :: t -> ClassTag
    obj_primitive :: t -> [Primitive]

instance Object ADMARE where
    obj_code _ = 1
    obj_class _ = "Administration area (Named)"
    obj_attrA _ = [JRSDTN,NATION,NOBJNM,OBJNAM]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object AIRARE where
    obj_code _ = 2
    obj_class _ = "Airport / airfield"
    obj_attrA _ = [CATAIR,CONDTN,CONVIS,NOBJNM,OBJNAM,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object ACHBRT where
    obj_code _ = 3
    obj_class _ = "Anchor berth"
    obj_attrA _ = [CATACH,DATEND,DATSTA,NOBJNM,OBJNAM,PEREND,PERSTA,RADIUS,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object ACHARE where
    obj_code _ = 4
    obj_class _ = "Anchorage area"
    obj_attrA _ = [CATACH,DATEND,DATSTA,NOBJNM,OBJNAM,PEREND,PERSTA,RESTRN,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object BCNCAR where
    obj_code _ = 5
    obj_class _ = "Beacon cardinal"
    obj_attrA _ = [BCNSHP,CATCAM,COLOUR,COLPAT,CONDTN,CONVIS,CONRAD,DATEND,DATSTA,ELEVAT,HEIGHT,MARSYS,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object BCNISD where
    obj_code _ = 6
    obj_class _ = "Beacon isolated danger"
    obj_attrA _ = [BCNSHP,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,ELEVAT,HEIGHT,MARSYS,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object BCNLAT where
    obj_code _ = 7
    obj_class _ = "Beacon lateral"
    obj_attrA _ = [BCNSHP,CATLAM,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,ELEVAT,HEIGHT,MARSYS,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object BCNSAW where
    obj_code _ = 8
    obj_class _ = "Beacon safe water"
    obj_attrA _ = [BCNSHP,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,ELEVAT,HEIGHT,MARSYS,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object BCNSPP where
    obj_code _ = 9
    obj_class _ = "Beacon special purpose/general"
    obj_attrA _ = [BCNSHP,CATSPM,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,ELEVAT,HEIGHT,MARSYS,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object BERTHS where
    obj_code _ = 10
    obj_class _ = "Berth"
    obj_attrA _ = [DATEND,DATSTA,DRVAL1,NOBJNM,OBJNAM,PEREND,PERSTA,QUASOU,SOUACC,STATUS,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object BRIDGE where
    obj_code _ = 11
    obj_class _ = "Bridge"
    obj_attrA _ = [CATBRG,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,HORACC,HORCLR,NATCON,NOBJNM,OBJNAM,VERACC,VERCCL,VERCLR,VERCOP,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object BUISGL where
    obj_code _ = 12
    obj_class _ = "Building single"
    obj_attrA _ = [BUISHP,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,ELEVAT,FUNCTN,HEIGHT,NATCON,NOBJNM,OBJNAM,STATUS,VERACC,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object BUAARE where
    obj_code _ = 13
    obj_class _ = "Built-up area"
    obj_attrA _ = [CATBUA,CONDTN,CONRAD,CONVIS,HEIGHT,NOBJNM,OBJNAM,VERACC,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object BOYCAR where
    obj_code _ = 14
    obj_class _ = "Buoy cardinal"
    obj_attrA _ = [BOYSHP,CATCAM,COLOUR,COLPAT,CONRAD,DATEND,DATSTA,MARSYS,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object BOYINB where
    obj_code _ = 15
    obj_class _ = "Buoy installation"
    obj_attrA _ = [BOYSHP,CATINB,COLOUR,COLPAT,CONRAD,DATEND,DATSTA,MARSYS,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,PRODCT,STATUS,VERACC,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object BOYISD where
    obj_code _ = 16
    obj_class _ = "Buoy isolated danger"
    obj_attrA _ = [BOYSHP,COLOUR,COLPAT,CONRAD,DATEND,DATSTA,MARSYS,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object BOYLAT where
    obj_code _ = 17
    obj_class _ = "Buoy lateral"
    obj_attrA _ = [BOYSHP,CATLAM,COLOUR,COLPAT,CONRAD,DATEND,DATSTA,MARSYS,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object BOYSAW where
    obj_code _ = 18
    obj_class _ = "Buoy safe water"
    obj_attrA _ = [BOYSHP,COLOUR,COLPAT,CONRAD,DATEND,DATSTA,MARSYS,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object BOYSPP where
    obj_code _ = 19
    obj_class _ = "Buoy special purpose/general"
    obj_attrA _ = [BOYSHP,CATSPM,COLOUR,COLPAT,CONRAD,DATEND,DATSTA,MARSYS,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object CBLARE where
    obj_code _ = 20
    obj_class _ = "Cable area"
    obj_attrA _ = [CATCBL,DATEND,DATSTA,NOBJNM,OBJNAM,RESTRN,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object CBLOHD where
    obj_code _ = 21
    obj_class _ = "Cable overhead"
    obj_attrA _ = [CATCBL,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,ICEFAC,NOBJNM,OBJNAM,STATUS,VERACC,VERCLR,VERCSA,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line]

instance Object CBLSUB where
    obj_code _ = 22
    obj_class _ = "Cable submarine"
    obj_attrA _ = [BURDEP,CATCBL,CONDTN,DATEND,DATSTA,NOBJNM,OBJNAM,STATUS,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line]

instance Object CANALS where
    obj_code _ = 23
    obj_class _ = "Canal"
    obj_attrA _ = [CATCAN,CONDTN,DATEND,DATSTA,HORACC,HORCLR,HORWID,NOBJNM,OBJNAM,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line,Area]

instance Object CTSARE where
    obj_code _ = 25
    obj_class _ = "Cargo transshipment area"
    obj_attrA _ = [DATEND,DATSTA,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object CAUSWY where
    obj_code _ = 26
    obj_class _ = "Causeway"
    obj_attrA _ = [CONDTN,NATCON,NOBJNM,OBJNAM,STATUS,WATLEV]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line,Area]

instance Object CTNARE where
    obj_code _ = 27
    obj_class _ = "Caution area"
    obj_attrA _ = [DATEND,DATSTA,PEREND,PERSTA]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object CHKPNT where
    obj_code _ = 28
    obj_class _ = "Checkpoint"
    obj_attrA _ = [CATCHP,NOBJNM,OBJNAM,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object CGUSTA where
    obj_code _ = 29
    obj_class _ = "Coastguard station"
    obj_attrA _ = [DATEND,DATSTA,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object COALNE where
    obj_code _ = 30
    obj_class _ = "Coastline"
    obj_attrA _ = [CATCOA,COLOUR,CONRAD,CONVIS,ELEVAT,NOBJNM,OBJNAM,VERACC,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line]

instance Object CONZNE where
    obj_code _ = 31
    obj_class _ = "Contiguous zone"
    obj_attrA _ = [DATEND,DATSTA,NATION,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object COSARE where
    obj_code _ = 32
    obj_class _ = "Continental shelf area"
    obj_attrA _ = [NATION,NOBJNM,OBJNAM]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object CTRPNT where
    obj_code _ = 33
    obj_class _ = "Control point"
    obj_attrA _ = [CATCTR,DATEND,DATSTA,ELEVAT,NOBJNM,OBJNAM,VERACC,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object CONVYR where
    obj_code _ = 34
    obj_class _ = "Conveyor"
    obj_attrA _ = [CATCON,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,HEIGHT,LIFCAP,NOBJNM,OBJNAM,PRODCT,STATUS,VERACC,VERCLR,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line,Area]

instance Object CRANES where
    obj_code _ = 35
    obj_class _ = "Crane"
    obj_attrA _ = [CATCRN,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,HEIGHT,LIFCAP,NOBJNM,OBJNAM,ORIENT,RADIUS,STATUS,VERACC,VERCLR,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object CURENT where
    obj_code _ = 36
    obj_class _ = "Current - non - gravitational"
    obj_attrA _ = [CURVEL,DATEND,DATSTA,NOBJNM,OBJNAM,ORIENT,PEREND,PERSTA]
    obj_attrB _ = [INFORM,NINFOM,SCAMAX,SCAMIN]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object CUSZNE where
    obj_code _ = 37
    obj_class _ = "Custom zone"
    obj_attrA _ = [NATION]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object DAMCON where
    obj_code _ = 38
    obj_class _ = "Dam"
    obj_attrA _ = [CATDAM,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,HEIGHT,NATCON,NOBJNM,OBJNAM,VERACC,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object DAYMAR where
    obj_code _ = 39
    obj_class _ = "Daymark"
    obj_attrA _ = [CATSPM,COLOUR,COLPAT,DATEND,DATSTA,ELEVAT,HEIGHT,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,TOPSHP,VERACC,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object DWRTCL where
    obj_code _ = 40
    obj_class _ = "Deep water route centerline"
    obj_attrA _ = [CATTRK,DATEND,DATSTA,DRVAL1,DRVAL2,NOBJNM,OBJNAM,ORIENT,QUASOU,SOUACC,STATUS,TECSOU,TRAFIC,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line]

instance Object DWRTPT where
    obj_code _ = 41
    obj_class _ = "Deep water route part"
    obj_attrA _ = [DATEND,DATSTA,DRVAL1,DRVAL2,NOBJNM,OBJNAM,ORIENT,QUASOU,SOUACC,STATUS,TECSOU,TRAFIC,VERDAT,RESTRN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object DEPARE where
    obj_code _ = 42
    obj_class _ = "Depth area"
    obj_attrA _ = [DRVAL1,DRVAL2,QUASOU,SOUACC,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line,Area]

instance Object DEPCNT where
    obj_code _ = 43
    obj_class _ = "Depth contour"
    obj_attrA _ = [VALDCO,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line]

instance Object DISMAR where
    obj_code _ = 44
    obj_class _ = "Distance mark"
    obj_attrA _ = [CATDIS,DATEND,DATSTA,NOBJNM,OBJNAM]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object DOCARE where
    obj_code _ = 45
    obj_class _ = "Dock area"
    obj_attrA _ = [CATDOC,CONDTN,DATEND,DATSTA,HORACC,HORCLR,NOBJNM,OBJNAM,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object DRGARE where
    obj_code _ = 46
    obj_class _ = "Dredged area"
    obj_attrA _ = [DRVAL1,DRVAL2,NOBJNM,OBJNAM,QUASOU,RESTRN,SOUACC,TECSOU,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object DRYDOC where
    obj_code _ = 47
    obj_class _ = "Dry dock"
    obj_attrA _ = [CONDTN,HORACC,HORCLR,HORLEN,HORWID,NOBJNM,OBJNAM,STATUS,DRVAL1,QUASOU,SOUACC,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,SCAMAX,SCAMIN]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object DMPGRD where
    obj_code _ = 48
    obj_class _ = "Dumping ground"
    obj_attrA _ = [CATDPG,NOBJNM,OBJNAM,RESTRN,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object DYKCON where
    obj_code _ = 49
    obj_class _ = "Dyke"
    obj_attrA _ = [CONDTN,CONRAD,DATEND,DATSTA,HEIGHT,NATCON,VERACC,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line,Area]

instance Object EXEZNE where
    obj_code _ = 50
    obj_class _ = "Exclusive Economic Zone"
    obj_attrA _ = [NATION]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object FAIRWY where
    obj_code _ = 51
    obj_class _ = "Fairway"
    obj_attrA _ = [DATEND,DATSTA,DRVAL1,NOBJNM,OBJNAM,ORIENT,QUASOU,RESTRN,SOUACC,STATUS,TRAFIC,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object FNCLNE where
    obj_code _ = 52
    obj_class _ = "Fence/wall"
    obj_attrA _ = [CATFNC,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,ELEVAT,HEIGHT,NATCON,NOBJNM,OBJNAM,STATUS,VERACC,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line]

instance Object FERYRT where
    obj_code _ = 53
    obj_class _ = "Ferry route"
    obj_attrA _ = [CATFRY,DATEND,DATSTA,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line,Area]

instance Object FSHZNE where
    obj_code _ = 54
    obj_class _ = "Fishery zone"
    obj_attrA _ = [NATION,NOBJNM,OBJNAM,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object FSHFAC where
    obj_code _ = 55
    obj_class _ = "Fishing facility"
    obj_attrA _ = [CATFIF,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object FSHGRD where
    obj_code _ = 56
    obj_class _ = "Fishing ground"
    obj_attrA _ = [NOBJNM,OBJNAM,PEREND,PERSTA,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object FLODOC where
    obj_code _ = 57
    obj_class _ = "Floating dock"
    obj_attrA _ = [COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,DRVAL1,HORACC,HORCLR,HORLEN,HORWID,LIFCAP,NOBJNM,OBJNAM,STATUS,VERACC,VERLEN,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line,Area]

instance Object FOGSIG where
    obj_code _ = 58
    obj_class _ = "Fog signal"
    obj_attrA _ = [CATFOG,DATEND,DATSTA,NOBJNM,OBJNAM,SIGFRQ,SIGGEN,SIGGRP,SIGPER,SIGSEQ,STATUS,VALMXR]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object FORSTC where
    obj_code _ = 59
    obj_class _ = "Fortified structure"
    obj_attrA _ = [CATFOR,CONDTN,CONRAD,CONVIS,HEIGHT,NATCON,NOBJNM,OBJNAM,VERACC,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object FRPARE where
    obj_code _ = 60
    obj_class _ = "Free port area"
    obj_attrA _ = [NOBJNM,OBJNAM,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object GATCON where
    obj_code _ = 61
    obj_class _ = "Gate"
    obj_attrA _ = [CATGAT,CONDTN,DRVAL1,HORACC,HORCLR,NATCON,NOBJNM,OBJNAM,QUASOU,SOUACC,STATUS,VERACC,VERCLR,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object GRIDRN where
    obj_code _ = 62
    obj_class _ = "Gridiron"
    obj_attrA _ = [HORACC,HORLEN,HORWID,NATCON,NOBJNM,OBJNAM,STATUS,VERACC,VERLEN,WATLEV]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object HRBARE where
    obj_code _ = 63
    obj_class _ = "Harbour area (administrative)"
    obj_attrA _ = [NOBJNM,OBJNAM,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object HRBFAC where
    obj_code _ = 64
    obj_class _ = "Harbour facility"
    obj_attrA _ = [CATHAF,CONDTN,DATEND,DATSTA,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object HULKES where
    obj_code _ = 65
    obj_class _ = "Hulk"
    obj_attrA _ = [CATHLK,COLOUR,COLPAT,CONRAD,CONVIS,HORACC,HORLEN,HORWID,NOBJNM,OBJNAM,VERACC,VERLEN,CONDTN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object ICEARE where
    obj_code _ = 66
    obj_class _ = "Ice area"
    obj_attrA _ = [CATICE,CONVIS,ELEVAT,HEIGHT,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object ICNARE where
    obj_code _ = 67
    obj_class _ = "Incineration area"
    obj_attrA _ = [NOBJNM,OBJNAM,PEREND,PERSTA,RESTRN,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object ISTZNE where
    obj_code _ = 68
    obj_class _ = "Inshore traffic zone"
    obj_attrA _ = [CATTSS,DATEND,DATSTA,RESTRN,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object LAKARE where
    obj_code _ = 69
    obj_class _ = "Lake"
    obj_attrA _ = [ELEVAT,NOBJNM,OBJNAM,VERACC,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object LNDARE where
    obj_code _ = 71
    obj_class _ = "Land area"
    obj_attrA _ = [CONDTN,NOBJNM,OBJNAM,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object LNDELV where
    obj_code _ = 72
    obj_class _ = "Land elevation"
    obj_attrA _ = [CONVIS,ELEVAT,NOBJNM,OBJNAM,VERACC,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line]

instance Object LNDRGN where
    obj_code _ = 73
    obj_class _ = "Land region"
    obj_attrA _ = [CATLND,NATQUA,NATSUR,NOBJNM,OBJNAM,WATLEV]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object LNDMRK where
    obj_code _ = 74
    obj_class _ = "Landmark"
    obj_attrA _ = [CATLMK,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,ELEVAT,FUNCTN,HEIGHT,NATCON,NOBJNM,OBJNAM,STATUS,VERACC,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object LIGHTS where
    obj_code _ = 75
    obj_class _ = "Light"
    obj_attrA _ = [CATLIT,COLOUR,DATEND,DATSTA,EXCLIT,HEIGHT,LITCHR,LITVIS,MARSYS,MLTYLT,NOBJNM,OBJNAM,ORIENT,PEREND,PERSTA,SECTR1,SECTR2,SIGGRP,SIGPER,SIGSEQ,STATUS,VERACC,VALNMR,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object LITFLT where
    obj_code _ = 76
    obj_class _ = "Light float"
    obj_attrA _ = [COLOUR,COLPAT,CONRAD,CONVIS,DATEND,DATSTA,HORACC,HORLEN,HORWID,MARSYS,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object LITVES where
    obj_code _ = 77
    obj_class _ = "Light vessel"
    obj_attrA _ = [COLOUR,COLPAT,CONRAD,CONVIS,DATEND,DATSTA,HORACC,HORLEN,HORWID,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object LOCMAG where
    obj_code _ = 78
    obj_class _ = "Local magnetic anomaly"
    obj_attrA _ = [NOBJNM,OBJNAM,VALLMA]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object LOKBSN where
    obj_code _ = 79
    obj_class _ = "Lock basin"
    obj_attrA _ = [DATEND,DATSTA,HORACC,HORCLR,HORLEN,HORWID,NOBJNM,OBJNAM,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object LOGPON where
    obj_code _ = 80
    obj_class _ = "Log pond"
    obj_attrA _ = [NOBJNM,OBJNAM,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object MAGVAR where
    obj_code _ = 81
    obj_class _ = "Magnetic variation"
    obj_attrA _ = [DATEND,DATSTA,RYRMGV,VALACM,VALMAG]
    obj_attrB _ = [INFORM,NINFOM,SCAMAX,SCAMIN]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object MARCUL where
    obj_code _ = 82
    obj_class _ = "Marine farm/culture"
    obj_attrA _ = [CATMFA,DATEND,DATSTA,EXPSOU,NOBJNM,OBJNAM,PEREND,PERSTA,QUASOU,RESTRN,SOUACC,STATUS,VALSOU,VERACC,VERDAT,VERLEN,WATLEV]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object MIPARE where
    obj_code _ = 83
    obj_class _ = "Military practice area"
    obj_attrA _ = [CATMPA,DATEND,DATSTA,NOBJNM,OBJNAM,PEREND,PERSTA,RESTRN,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object MORFAC where
    obj_code _ = 84
    obj_class _ = "Mooring/warping facility"
    obj_attrA _ = [BOYSHP,CATMOR,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,HEIGHT,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERDAT,VERLEN,WATLEV]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object NAVLNE where
    obj_code _ = 85
    obj_class _ = "Navigation line"
    obj_attrA _ = [CATNAV,DATEND,DATSTA,ORIENT,PEREND,PERSTA,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line]

instance Object OBSTRN where
    obj_code _ = 86
    obj_class _ = "Obstruction"
    obj_attrA _ = [CATOBS,CONDTN,EXPSOU,HEIGHT,NATCON,NATQUA,NOBJNM,OBJNAM,PRODCT,QUASOU,SOUACC,STATUS,TECSOU,VALSOU,VERACC,VERDAT,VERLEN,WATLEV,NATSUR]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object OFSPLF where
    obj_code _ = 87
    obj_class _ = "Offshore platform"
    obj_attrA _ = [CATOFP,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,HEIGHT,NATCON,NOBJNM,OBJNAM,PRODCT,STATUS,VERACC,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object OSPARE where
    obj_code _ = 88
    obj_class _ = "Offshore production area"
    obj_attrA _ = [CATPRA,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,HEIGHT,NOBJNM,OBJNAM,PRODCT,RESTRN,STATUS,VERACC,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object OILBAR where
    obj_code _ = 89
    obj_class _ = "Oil barrier"
    obj_attrA _ = [CATOLB,CONDTN,DATEND,DATSTA,NOBJNM,OBJNAM,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line]

instance Object PILPNT where
    obj_code _ = 90
    obj_class _ = "Pile"
    obj_attrA _ = [CATPLE,COLOUR,COLPAT,CONDTN,CONVIS,DATEND,DATSTA,HEIGHT,NOBJNM,OBJNAM,VERACC,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object PILBOP where
    obj_code _ = 91
    obj_class _ = "Pilot boarding place"
    obj_attrA _ = [CATPIL,COMCHA,DATEND,DATSTA,NOBJNM,NPLDST,OBJNAM,PEREND,PERSTA,PILDST,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object PIPARE where
    obj_code _ = 92
    obj_class _ = "Pipeline area"
    obj_attrA _ = [CONDTN,DATEND,DATSTA,NOBJNM,OBJNAM,PRODCT,RESTRN,STATUS,CATPIP]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object PIPOHD where
    obj_code _ = 93
    obj_class _ = "Pipeline overhead"
    obj_attrA _ = [CATPIP,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,NOBJNM,OBJNAM,PRODCT,STATUS,VERACC,VERCLR,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line]

instance Object PIPSOL where
    obj_code _ = 94
    obj_class _ = "Pipeline submarine/on land"
    obj_attrA _ = [BURDEP,CATPIP,CONDTN,DATEND,DATSTA,DRVAL1,DRVAL2,NOBJNM,OBJNAM,PRODCT,STATUS,VERACC,VERLEN,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line]

instance Object PONTON where
    obj_code _ = 95
    obj_class _ = "Pontoon"
    obj_attrA _ = [CONDTN,CONRAD,CONVIS,DATEND,DATSTA,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS,VERACC,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line,Area]

instance Object PRCARE where
    obj_code _ = 96
    obj_class _ = "Precautionary area"
    obj_attrA _ = [DATEND,DATSTA,RESTRN,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object PRDARE where
    obj_code _ = 97
    obj_class _ = "Production / storage area"
    obj_attrA _ = [CATPRA,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,ELEVAT,HEIGHT,NOBJNM,OBJNAM,PRODCT,STATUS,VERACC,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object PYLONS where
    obj_code _ = 98
    obj_class _ = "Pylon/bridge support"
    obj_attrA _ = [CATPYL,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,HEIGHT,NATCON,NOBJNM,OBJNAM,VERACC,VERDAT,VERLEN,WATLEV]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object RADLNE where
    obj_code _ = 99
    obj_class _ = "Radar line"
    obj_attrA _ = [NOBJNM,OBJNAM,ORIENT,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line]

instance Object RADRNG where
    obj_code _ = 100
    obj_class _ = "Radar range"
    obj_attrA _ = [COMCHA,DATEND,DATSTA,NOBJNM,OBJNAM,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object RADRFL where
    obj_code _ = 101
    obj_class _ = "Radar reflector"
    obj_attrA _ = [HEIGHT,STATUS,VERACC,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object RADSTA where
    obj_code _ = 102
    obj_class _ = "Radar station"
    obj_attrA _ = [CATRAS,DATEND,DATSTA,HEIGHT,NOBJNM,OBJNAM,STATUS,VERACC,VALMXR,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object RTPBCN where
    obj_code _ = 103
    obj_class _ = "Radar transponder beacon"
    obj_attrA _ = [CATRTB,DATEND,DATSTA,NOBJNM,OBJNAM,RADWAL,SECTR1,SECTR2,SIGGRP,SIGSEQ,STATUS,VALMXR]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object RDOCAL where
    obj_code _ = 104
    obj_class _ = "Radio calling-in point"
    obj_attrA _ = [COMCHA,DATEND,DATSTA,NOBJNM,OBJNAM,ORIENT,PEREND,PERSTA,STATUS,TRAFIC]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line]

instance Object RDOSTA where
    obj_code _ = 105
    obj_class _ = "Radio station"
    obj_attrA _ = [CALSGN,CATROS,COMCHA,DATEND,DATSTA,ESTRNG,NOBJNM,OBJNAM,ORIENT,PEREND,PERSTA,SIGFRQ,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object RAILWY where
    obj_code _ = 106
    obj_class _ = "Railway"
    obj_attrA _ = [CONDTN,HEIGHT,NOBJNM,OBJNAM,STATUS,VERACC]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line]

instance Object RAPIDS where
    obj_code _ = 107
    obj_class _ = "Rapids"
    obj_attrA _ = [NOBJNM,OBJNAM,VERACC,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object RCRTCL where
    obj_code _ = 108
    obj_class _ = "Recommended route centerline"
    obj_attrA _ = [CATTRK,DATEND,DATSTA,DRVAL1,DRVAL2,NOBJNM,OBJNAM,ORIENT,PEREND,PERSTA,QUASOU,SOUACC,STATUS,TECSOU,TRAFIC,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line]

instance Object RECTRC where
    obj_code _ = 109
    obj_class _ = "Recommended track"
    obj_attrA _ = [CATTRK,DATEND,DATSTA,DRVAL1,DRVAL2,NOBJNM,OBJNAM,ORIENT,PEREND,PERSTA,QUASOU,SOUACC,STATUS,TECSOU,TRAFIC,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line,Area]

instance Object RCTLPT where
    obj_code _ = 110
    obj_class _ = "Recommended Traffic Lane Part"
    obj_attrA _ = [DATEND,DATSTA,ORIENT,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object RSCSTA where
    obj_code _ = 111
    obj_class _ = "Rescue station"
    obj_attrA _ = [CATRSC,DATEND,DATSTA,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS]
    obj_attrB _ = [INFORM,NINFOM,SCAMAX,SCAMIN]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object RESARE where
    obj_code _ = 112
    obj_class _ = "Restricted area"
    obj_attrA _ = [CATREA,DATEND,DATSTA,NOBJNM,OBJNAM,PEREND,PERSTA,RESTRN,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object RETRFL where
    obj_code _ = 113
    obj_class _ = "Retro-reflector"
    obj_attrA _ = [COLOUR,COLPAT,HEIGHT,MARSYS,STATUS,VERACC,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object RIVERS where
    obj_code _ = 114
    obj_class _ = "River"
    obj_attrA _ = [NOBJNM,OBJNAM,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line,Area]

instance Object ROADWY where
    obj_code _ = 116
    obj_class _ = "Road"
    obj_attrA _ = [CATROD,CONDTN,NATCON,NOBJNM,OBJNAM,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object RUNWAY where
    obj_code _ = 117
    obj_class _ = "Runway"
    obj_attrA _ = [CATRUN,CONDTN,CONVIS,NATCON,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object SNDWAV where
    obj_code _ = 118
    obj_class _ = "Sand waves"
    obj_attrA _ = [VERACC,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object SEAARE where
    obj_code _ = 119
    obj_class _ = "Sea area / named water area"
    obj_attrA _ = [CATSEA,NOBJNM,OBJNAM]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object SPLARE where
    obj_code _ = 120
    obj_class _ = "Sea-plane landing area"
    obj_attrA _ = [NOBJNM,OBJNAM,PEREND,PERSTA,RESTRN,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC,VALDCO]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object SBDARE where
    obj_code _ = 121
    obj_class _ = "Seabed area"
    obj_attrA _ = [COLOUR,NATQUA,NATSUR,WATLEV,OBJNAM,NOBJNM]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object SLCONS where
    obj_code _ = 122
    obj_class _ = "Shoreline Construction"
    obj_attrA _ = [CATSLC,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,DATEND,DATSTA,HEIGHT,HORACC,HORCLR,HORLEN,HORWID,NATCON,NOBJNM,OBJNAM,STATUS,VERACC,VERDAT,VERLEN,WATLEV]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object SISTAT where
    obj_code _ = 123
    obj_class _ = "Signal station traffic"
    obj_attrA _ = [CATSIT,COMCHA,DATEND,DATSTA,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object SISTAW where
    obj_code _ = 124
    obj_class _ = "Signal station warning"
    obj_attrA _ = [CATSIW,COMCHA,DATEND,DATSTA,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object SILTNK where
    obj_code _ = 125
    obj_class _ = "Silo / tank"
    obj_attrA _ = [BUISHP,CATSIL,COLOUR,COLPAT,CONDTN,CONRAD,CONVIS,ELEVAT,HEIGHT,NATCON,NOBJNM,OBJNAM,PRODCT,STATUS,VERACC,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object SLOTOP where
    obj_code _ = 126
    obj_class _ = "Slope topline"
    obj_attrA _ = [CATSLO,COLOUR,CONRAD,CONVIS,ELEVAT,NATCON,NATQUA,NATSUR,NOBJNM,OBJNAM,VERACC,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line]

instance Object SLOGRD where
    obj_code _ = 127
    obj_class _ = "Sloping ground"
    obj_attrA _ = [CATSLO,COLOUR,CONRAD,CONVIS,NATCON,NATQUA,NATSUR,NOBJNM,OBJNAM]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object SMCFAC where
    obj_code _ = 128
    obj_class _ = "Small craft facility"
    obj_attrA _ = [CATSCF,NOBJNM,OBJNAM,PEREND,PERSTA,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object SOUNDG where
    obj_code _ = 129
    obj_class _ = "Sounding"
    obj_attrA _ = [EXPSOU,NOBJNM,OBJNAM,QUASOU,SOUACC,TECSOU,VERDAT,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object SPRING where
    obj_code _ = 130
    obj_class _ = "Spring"
    obj_attrA _ = [NOBJNM,OBJNAM]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object STSLNE where
    obj_code _ = 132
    obj_class _ = "Straight territorial sea baseline"
    obj_attrA _ = [NATION]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line]

instance Object SUBTLN where
    obj_code _ = 133
    obj_class _ = "Submarine transit lane"
    obj_attrA _ = [NOBJNM,OBJNAM,RESTRN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object SWPARE where
    obj_code _ = 134
    obj_class _ = "Swept Area"
    obj_attrA _ = [DRVAL1,QUASOU,SOUACC,TECSOU,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object TESARE where
    obj_code _ = 135
    obj_class _ = "Territorial sea area"
    obj_attrA _ = [NATION,RESTRN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object TS_PRH where
    obj_code _ = 136
    obj_class _ = "Tidal stream - harmonic prediction"
    obj_attrA _ = [NOBJNM,OBJNAM,T_MTOD,T_VAHC,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object TS_PNH where
    obj_code _ = 137
    obj_class _ = "Tidal stream - non-harmonic prediction"
    obj_attrA _ = [NOBJNM,OBJNAM,T_MTOD,T_THDF,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object TS_PAD where
    obj_code _ = 138
    obj_class _ = "Tidal stream panel data"
    obj_attrA _ = [NOBJNM,OBJNAM,TS_TSP]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object TS_TIS where
    obj_code _ = 139
    obj_class _ = "Tidal stream - time series"
    obj_attrA _ = [NOBJNM,OBJNAM,STATUS,TIMEND,TIMSTA,T_TINT,TS_TSV]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object T_HMON where
    obj_code _ = 140
    obj_class _ = "Tide - harmonic prediction"
    obj_attrA _ = [NOBJNM,OBJNAM,T_ACWL,T_MTOD,T_VAHC,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object T_NHMN where
    obj_code _ = 141
    obj_class _ = "Tide - non-harmonic prediction"
    obj_attrA _ = [NOBJNM,OBJNAM,T_ACWL,T_MTOD,T_THDF,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object T_TIMS where
    obj_code _ = 142
    obj_class _ = "Tidal stream - time series"
    obj_attrA _ = [NOBJNM,OBJNAM,T_HWLW,T_TINT,T_TSVL,TIMEND,TIMSTA,STATUS,T_ACWL]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object TIDEWY where
    obj_code _ = 143
    obj_class _ = "Tideway"
    obj_attrA _ = [NOBJNM,OBJNAM]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line,Area]

instance Object TOPMAR where
    obj_code _ = 144
    obj_class _ = "Top mark"
    obj_attrA _ = [COLOUR,COLPAT,HEIGHT,MARSYS,STATUS,TOPSHP,VERACC,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object TSELNE where
    obj_code _ = 145
    obj_class _ = "Traffic Separation Line"
    obj_attrA _ = [CATTSS,DATEND,DATSTA,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line]

instance Object TSSBND where
    obj_code _ = 146
    obj_class _ = "Traffic Separation Scheme  Boundary"
    obj_attrA _ = [CATTSS,DATEND,DATSTA,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Line]

instance Object TSSCRS where
    obj_code _ = 147
    obj_class _ = "Traffic Separation Scheme Crossing"
    obj_attrA _ = [CATTSS,DATEND,DATSTA,RESTRN,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object TSSLPT where
    obj_code _ = 148
    obj_class _ = "Traffic Separation Scheme  Lane part"
    obj_attrA _ = [CATTSS,DATEND,DATSTA,ORIENT,RESTRN,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object TSSRON where
    obj_code _ = 149
    obj_class _ = "Traffic Separation Scheme  Roundabout"
    obj_attrA _ = [CATTSS,DATEND,DATSTA,RESTRN,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object TSEZNE where
    obj_code _ = 150
    obj_class _ = "Traffic Separation Zone"
    obj_attrA _ = [CATTSS,DATEND,DATSTA,STATUS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object TUNNEL where
    obj_code _ = 151
    obj_class _ = "Tunnel"
    obj_attrA _ = [BURDEP,CONDTN,HORACC,HORCLR,NOBJNM,OBJNAM,STATUS,VERACC,VERCLR]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object TWRTPT where
    obj_code _ = 152
    obj_class _ = "Two-way route  part"
    obj_attrA _ = [CATTRK,DATEND,DATSTA,DRVAL1,DRVAL2,ORIENT,QUASOU,SOUACC,STATUS,TECSOU,TRAFIC,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object UWTROC where
    obj_code _ = 153
    obj_class _ = "Underwater rock / awash rock"
    obj_attrA _ = [EXPSOU,NATSUR,NATQUA,NOBJNM,OBJNAM,QUASOU,SOUACC,STATUS,TECSOU,VALSOU,VERDAT,WATLEV]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point]

instance Object UNSARE where
    obj_code _ = 154
    obj_class _ = "Unsurveyed area"
    obj_attrA _ = []
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Area]

instance Object VEGATN where
    obj_code _ = 155
    obj_class _ = "Vegetation"
    obj_attrA _ = [CATVEG,CONVIS,ELEVAT,HEIGHT,NOBJNM,OBJNAM,VERACC,VERDAT,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object WATTUR where
    obj_code _ = 156
    obj_class _ = "Water turbulence"
    obj_attrA _ = [CATWAT,NOBJNM,OBJNAM]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line,Area]

instance Object WATFAL where
    obj_code _ = 157
    obj_class _ = "Waterfall"
    obj_attrA _ = [CONVIS,NOBJNM,OBJNAM,VERACC,VERLEN]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Line]

instance Object WEDKLP where
    obj_code _ = 158
    obj_class _ = "Weed/Kelp"
    obj_attrA _ = [CATWED,NOBJNM,OBJNAM]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object WRECKS where
    obj_code _ = 159
    obj_class _ = "Wreck"
    obj_attrA _ = [CATWRK,CONRAD,CONVIS,EXPSOU,HEIGHT,NOBJNM,OBJNAM,QUASOU,SOUACC,STATUS,TECSOU,VALSOU,VERACC,VERDAT,VERLEN,WATLEV]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Geo
    obj_primitive _ = [Point,Area]

instance Object M_ACCY where
    obj_code _ = 300
    obj_class _ = "Accuracy of data"
    obj_attrA _ = [HORACC,POSACC,SOUACC,VERACC]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Meta
    obj_primitive _ = [Area]

instance Object M_CSCL where
    obj_code _ = 301
    obj_class _ = "Compilation scale of data"
    obj_attrA _ = [CSCALE]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Meta
    obj_primitive _ = [Area]

instance Object M_COVR where
    obj_code _ = 302
    obj_class _ = "Coverage"
    obj_attrA _ = [CATCOV]
    obj_attrB _ = [INFORM,NINFOM]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Meta
    obj_primitive _ = [Area]

instance Object M_HDAT where
    obj_code _ = 303
    obj_class _ = "Horizontal datum of data"
    obj_attrA _ = [HORDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Meta
    obj_primitive _ = [Area]

instance Object M_HOPA where
    obj_code _ = 304
    obj_class _ = "Horizontal datum shift parameters"
    obj_attrA _ = [HORDAT,SHIPAM]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Meta
    obj_primitive _ = [Area]

instance Object M_NPUB where
    obj_code _ = 305
    obj_class _ = "Nautical publication information"
    obj_attrA _ = []
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,PUBREF,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Meta
    obj_primitive _ = [Area]

instance Object M_NSYS where
    obj_code _ = 306
    obj_class _ = "Navigational system of marks"
    obj_attrA _ = [MARSYS,ORIENT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Meta
    obj_primitive _ = [Area]

instance Object M_PROD where
    obj_code _ = 307
    obj_class _ = "Production information"
    obj_attrA _ = [AGENCY,CPDATE,NATION,NMDATE,PRCTRY]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Meta
    obj_primitive _ = [Area]

instance Object M_QUAL where
    obj_code _ = 308
    obj_class _ = "Quality of data"
    obj_attrA _ = [CATQUA,CATZOC,DRVAL1,DRVAL2,POSACC,SOUACC,SUREND,SURSTA,TECSOU,VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Meta
    obj_primitive _ = [Area]

instance Object M_SDAT where
    obj_code _ = 309
    obj_class _ = "Sounding datum"
    obj_attrA _ = [VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Meta
    obj_primitive _ = [Area]

instance Object M_SREL where
    obj_code _ = 310
    obj_class _ = "Survey reliability"
    obj_attrA _ = [QUAPOS,QUASOU,SCVAL1,SCVAL2,SDISMN,SDISMX,SURATH,SUREND,SURSTA,SURTYP,TECSOU]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Meta
    obj_primitive _ = [Area]

instance Object M_UNIT where
    obj_code _ = 311
    obj_class _ = "Units of measurement of data"
    obj_attrA _ = [DUNITS,HUNITS,PUNITS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Meta
    obj_primitive _ = [Area]

instance Object M_VDAT where
    obj_code _ = 312
    obj_class _ = "Vertical datum of data"
    obj_attrA _ = [VERDAT]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Meta
    obj_primitive _ = [Area]

instance Object C_AGGR where
    obj_code _ = 400
    obj_class _ = "Aggregation"
    obj_attrA _ = [NOBJNM,OBJNAM]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Collection
    obj_primitive _ = []

instance Object C_ASSO where
    obj_code _ = 401
    obj_class _ = "Association"
    obj_attrA _ = [NOBJNM,OBJNAM]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Collection
    obj_primitive _ = []

instance Object C_STAC where
    obj_code _ = 402
    obj_class _ = "Stacked on/stacked under"
    obj_attrA _ = []
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Collection
    obj_primitive _ = []

instance Object AREAS where
    obj_code _ = 500
    obj_class _ = "Cartographic area"
    obj_attrA _ = [COLOUR,ORIENT,SCODE,TINTS]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Cartographic
    obj_primitive _ = []

instance Object LINES where
    obj_code _ = 501
    obj_class _ = "Cartographic line"
    obj_attrA _ = [SCODE]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Cartographic
    obj_primitive _ = []

instance Object CSYMB where
    obj_code _ = 502
    obj_class _ = "Cartographic symbol"
    obj_attrA _ = [ORIENT,SCALE,SCODE]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Cartographic
    obj_primitive _ = []

instance Object COMPS where
    obj_code _ = 503
    obj_class _ = "Compass"
    obj_attrA _ = [CSIZE,RYRMGV,VALACM,VALMAG]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Cartographic
    obj_primitive _ = []

instance Object TEXTS where
    obj_code _ = 504
    obj_class _ = "Text"
    obj_attrA _ = [CHARS,COLOUR,JUSTH,JUSTV,NTXST,SPACE,TXSTR]
    obj_attrB _ = [INFORM,NINFOM,NTXTDS,PICREP,SCAMAX,SCAMIN,TXTDSC]
    obj_attrC _ = [RECDAT,RECIND,SORDAT,SORIND]
    obj_classTag _ = Cartographic
    obj_primitive _ = []


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
