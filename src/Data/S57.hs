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
  module Data.S57.RecordTypes
) where

import Data.S57.ISO8211
import Data.Tree
import qualified Data.Map as Map

import Data.S57.RecordTypes

-- | a S-57 Datafile
data DataFileS57 = DataFileS57 {
      df_dsid :: !(Maybe DSID),
      df_dspm :: !(Maybe DSPM),
      df_catd :: !([CATD]),
      df_frids :: !([FRID]),
      df_vrids :: !([VRID])
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
             df_catd = catds f,
             df_dspm = dspm',
             df_frids = frids f,
             df_vrids = vrids'
           }

-- | get the 'DSID' from a ISO-8211 'DataFile' 
dsid :: DataFile -> Maybe DSID
dsid df = 
 case (findRecord' "DSID" df) of
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
 case (findRecord' "DSPM" df) of
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


catds :: DataFile -> [CATD]
catds df = 
    let rs = findRecords "CATD" df
        catd dr = CATD {
               catd_rcnm = sdRecordField dr "RCNM",
               catd_rcid = sdRecordField dr "RCID",
               catd_file = sdRecordField dr "FILE",
               catd_lfil = sdRecordField dr "LFIL",
               catd_volm = sdRecordField dr "VOLM", 
               catd_impl = sdRecordField dr "IMPL", 
               catd_slat = sdRecordField dr "SLAT",
               catd_wlon = sdRecordField dr "WLON", 
               catd_nlat = sdRecordField dr "NLAT", 
               catd_elon = sdRecordField dr "ELON",
               catd_crcs = sdRecordField dr "CRCS", 
               catd_comt = sdRecordField dr "COMT",
               catd_catxs = catxs dr
             }
    in map catd rs


catxs = maybemdRecords "CATX" catx
catxs :: DataRecord -> [CATX]

catx :: Map.Map String DataFieldT -> CATX
catx m = CATX {
         catx_rcnm = mdRecordField "RCNM" m,
         catx_rcid = mdRecordField "RCID" m,
         catx_nam1 = mdRecordField "NAM1" m,
         catx_nam2 = mdRecordField "NAM2" m,
         catx_comt = mdRecordField "COMT" m
       }


frids :: DataFile -> [FRID]
frids df = map frid $ findRecords "FRID" df

frid :: DataRecord -> FRID
frid dr = FRID {
       frid_rcnm = sdRecordField dr "RCNM",
       frid_rcid = sdRecordField dr "RCID", 
       frid_prim = sdRecordField dr "PRIM", 
       frid_grup = sdRecordField dr "GRUP",
       frid_objl = sdRecordField dr "OBJL", 
       frid_rver = sdRecordField dr "RVER", 
       frid_ruin = sdRecordField dr "RUIN"
          }


vrids :: DSPM -> DataFile -> [VRID]
vrids dspm' df = map (vrid dspm') $ findRecords "VRID" df

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




