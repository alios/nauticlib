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
  module Data.S57.RecordTypes,
  DataFileS57 (..),
  s57dataFile
) where

import qualified Data.ISO8211         as ISO8211
import           Data.ISO8211.Tree
import qualified Data.Map             as Map

import           Data.S57.RecordTypes

-- | a S-57 Datafile
data DataFileS57 = DataFileS57 {
      df_dsid  :: !(Maybe DSID),
      df_dspm  :: !(Maybe DSPM),
      df_dsht  :: !(Maybe DSHT),
      df_dsac  :: !(Maybe DSAC),
      df_catds :: !([CATD]),
      df_dddfs :: !([DDDF]),
      df_dddis :: !([DDDI]),
      df_ddsis :: !([DDSI]),
      df_frids :: !([FRID]),
      df_vrids :: !([VRID])
} deriving (Eq, Show)


--
-- exported functions
--
s57dataFile :: ISO8211.DataFile -> DataFileS57
s57dataFile f =
    let dspm' = dspm f
        vrids' =
            case dspm' of
              Nothing -> []
              Just dspm'' -> vrids dspm'' f
    in DataFileS57 {
             df_dsid = dsid f,
             df_catds = catds f,
             df_dddfs = dddfs f,
             df_dddis = dddis f,
             df_ddsis = ddsis f,
             df_dspm = dspm',
             df_dsht = dsht f,
             df_dsac = dsac f,
             df_frids = frids f,
             df_vrids = vrids'
           }

-- | get the 'DSID' from a ISO-8211 'DataFile'
dsid :: ISO8211.DataFile -> Maybe DSID
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
   where dssi :: ISO8211.DataRecord -> DSSI
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

dspm :: ISO8211.DataFile -> Maybe DSPM
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
     where dspr :: ISO8211.DataRecord -> Maybe DSPR
           dspr r = case (findSubRecord' "DSPR" r) of
                      Nothing -> Nothing
                      Just dr' -> Just DSPR {
             dspr_proj = sdRecordField dr' "PROJ",
             dspr_prp1 = sdRecordField dr' "PRP1",
             dspr_prp2 = sdRecordField dr' "PRP2",
             dspr_prp3 = sdRecordField dr' "PRP3",
             dspr_prp4 = sdRecordField dr' "PRP4",
             dspr_feas = sdRecordField dr' "FEAS",
             dspr_fnor = sdRecordField dr' "FNOR",
             dspr_fpmf = sdRecordField dr' "FPMF",
             dspr_comt = sdRecordField dr' "COMT"
                                }

           dsrcs :: ISO8211.DataRecord -> [DSRC]
           dsrcs = map dsrc . findSubRecords "DSRC"

           dsrc :: ISO8211.DataRecord -> DSRC
           dsrc dr' = let fpmf,ryco,rxco :: Integer
                          fpmf = sdRecordField dr' "FPMF"
                          ryco = sdRecordField dr' "RYCO"
                          rxco = sdRecordField dr' "RXCO"
                      in DSRC {
            dsrc_rpid = sdRecordField dr' "RPID",
            dsrc_ryco = (fromInteger ryco) / (fromInteger fpmf),
            dsrc_rxco = (fromInteger rxco) / (fromInteger fpmf),
            dsrc_curp = sdRecordField dr' "CURP",
            dsrc_fpmf = fpmf,
            dsrc_rxvl = sdRecordField dr' "RXVL",
            dsrc_ryvl = sdRecordField dr' "RYVL",
            dsrc_comt = sdRecordField dr' "COMT"
                            }

dsht :: ISO8211.DataFile -> Maybe DSHT
dsht df =
 case (findRecord' "DSHT" df) of
   Nothing -> Nothing
   Just dr ->
       Just DSHT {
                  dsht_rcnm = sdRecordField dr "RCNM",
                  dsht_rcid = sdRecordField dr "RCID",
                  dsht_prco = sdRecordField dr "PRCO",
                  dsht_esdt = sdRecordField dr "ESDT",
                  dsht_lsdt = sdRecordField dr "LSDT",
                  dsht_dcrt = sdRecordField dr "DCRT",
                  dsht_codt = sdRecordField dr "CODT",
                  dsht_comt = sdRecordField dr "COMT"
                }

dsac :: ISO8211.DataFile -> Maybe DSAC
dsac df =
 case (findRecord' "DSAC" df) of
   Nothing -> Nothing
   Just dr ->
       let fpmr = fromInteger $ sdRecordField dr "FPMR"
       in Just DSAC {
                dsac_rcnm = sdRecordField dr "RCNM",
                dsac_rcid = sdRecordField dr "RCID",
                dsac_pacc = (fromInteger $ sdRecordField dr "PACC")
                            / fromInteger fpmr,
                dsac_hacc = (fromInteger $ sdRecordField dr "HACC")
                            / fromInteger fpmr,
                dsac_sacc = (fromInteger $ sdRecordField dr "SACC")
                            / fromInteger fpmr,
                dsac_fpmf = fpmr,
                dsac_comt = sdRecordField dr "RCID"
              }


catds :: ISO8211.DataFile -> [CATD]
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


dddfs :: ISO8211.DataFile -> [DDDF]
dddfs df =
    let rs = findRecords "DDDF" df
        dddf dr = DDDF {
               dddf_rcnm = sdRecordField dr "RCNM",
               dddf_rcid = sdRecordField dr "RCID",
               dddf_oora = sdRecordField dr "OORA",
               dddf_oaac = sdRecordField dr "OAAC",
               dddf_oaco = sdRecordField dr "OACO",
               dddf_oall = sdRecordField dr "OALL",
               dddf_oaty = sdRecordField dr "OATY",
               dddf_defn = sdRecordField dr "DEFN",
               dddf_auth = sdRecordField dr "AUTH",
               dddf_comt = sdRecordField dr "COMT"
                  }
    in map dddf rs

dddis :: ISO8211.DataFile -> [DDDI]
dddis df =
    let rs = findRecords "DDDI" df
        dddi dr = DDDI {
               dddi_rcnm = sdRecordField dr "RCNM",
               dddi_rcid = sdRecordField dr "RCID",
               dddi_atlb = sdRecordField dr "ATLB",
               dddi_atdo = sdRecordField dr "ATDO",
               dddi_admu = sdRecordField dr "ADMU",
               dddi_adft = sdRecordField dr "ADFT",
               dddi_auth = sdRecordField dr "AUTH",
               dddi_comt = sdRecordField dr "COMT"
                  }
    in map dddi rs

ddsis :: ISO8211.DataFile -> [DDSI]
ddsis df =
    let rs = findRecords "DDSI" df
        ddsi dr = DDSI {
               ddsi_rcnm = sdRecordField dr "RCNM",
               ddsi_rcid = sdRecordField dr "RCID",
               ddsi_oblb = sdRecordField dr "OBLB"
                  }
    in map ddsi rs

catxs = maybemdRecords "CATX" catx
catxs :: ISO8211.DataRecord -> [CATX]

catx :: Map.Map String ISO8211.DataFieldT -> CATX
catx m = CATX {
         catx_rcnm = mdRecordField "RCNM" m,
         catx_rcid = mdRecordField "RCID" m,
         catx_nam1 = mdRecordField "NAM1" m,
         catx_nam2 = mdRecordField "NAM2" m,
         catx_comt = mdRecordField "COMT" m
       }


frids :: ISO8211.DataFile -> [FRID]
frids df = map frid $ findRecords "FRID" df

frid :: ISO8211.DataRecord -> FRID
frid dr = FRID {
       frid_rcnm = sdRecordField dr "RCNM",
       frid_rcid = sdRecordField dr "RCID",
       frid_prim = sdRecordField dr "PRIM",
       frid_grup = sdRecordField dr "GRUP",
       frid_objl = sdRecordField dr "OBJL",
       frid_rver = sdRecordField dr "RVER",
       frid_ruin = sdRecordField dr "RUIN",
       frid_foid = foid dr,
       frid_attfs = attfs dr,
       frid_natfs = natfs dr,
       frid_ffpc = ffpc dr,
       frid_ffpts = ffpts dr,
       frid_fspts = fspts dr
          }
    where foid :: ISO8211.DataRecord -> FOID
          foid r =
             let dr = findSubRecord "FOID" r
             in FOID {
                      foid_agen = sdRecordField dr "AGEN",
                      foid_fidn = sdRecordField dr "FIDN",
                      foid_fids = sdRecordField dr "FIDS"
                    }
          ffpc :: ISO8211.DataRecord -> Maybe FFPC
          ffpc r =
             let dr' = findSubRecord' "FFPC" r
             in case dr' of
                  Nothing -> Nothing
                  Just dr -> Just $ FFPC {
                      ffpc_ffui = sdRecordField dr "FFUI",
                      ffpc_ffix = sdRecordField dr "FFIX",
                      ffpc_nfpt = sdRecordField dr "NFPT"
                    }
          attfs :: ISO8211.DataRecord -> [ATTF]
          attfs = maybemdRecords "ATTF" attf
          attf :: Map.Map String ISO8211.DataFieldT -> ATTF
          attf dr = ATTF {
                      attf_attl = mdRecordField "ATTL" dr,
                      attf_atvl = mdRecordField "ATVL" dr
                    }
          natfs :: ISO8211.DataRecord -> [NATF]
          natfs = maybemdRecords "NATF" natf
          natf :: Map.Map String ISO8211.DataFieldT -> NATF
          natf dr = NATF {
                      natf_attl = mdRecordField "ATTL" dr,
                      natf_atvl = mdRecordField "ATVL" dr
                    }
          ffpts :: ISO8211.DataRecord -> [FFPT]
          ffpts = maybemdRecords "FFPT" ffpt
          ffpt :: Map.Map String ISO8211.DataFieldT -> FFPT
          ffpt dr = FFPT {
                      ffpt_lnam = mdRecordField "LNAM" dr,
                      ffpt_rind = mdRecordField "RIND" dr,
                      ffpt_comt = mdRecordField "COMT" dr
                    }
          fspts :: ISO8211.DataRecord -> [FSPT]
          fspts = maybemdRecords "FSPT" fspt
          fspt :: Map.Map String ISO8211.DataFieldT -> FSPT
          fspt dr = FSPT {
                      fspt_name = mdRecordField "NAME" dr,
                      fspt_ornt = mdRecordField "ORNT" dr,
                      fspt_usag = mdRecordField "USAG" dr,
                      fspt_mask = mdRecordField "MASK" dr
                    }


vrids :: DSPM -> ISO8211.DataFile -> [VRID]
vrids dspm' df = map (vrid dspm') $ findRecords "VRID" df

vrid    :: DSPM -> ISO8211.DataRecord -> VRID
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
             vrid_sg3ds = sg3ds dspm' dr,
             vrid_arccs = arccs dspm' dr
           }

vrpc :: ISO8211.DataRecord -> Maybe VRPC
vrpc r =
    case (findSubRecord' "VRPC" r) of
      Nothing -> Nothing
      Just dr -> Just VRPC {
                  vrpc_vpui = sdRecordField dr "VPUI",
                  vrpc_vpix = sdRecordField dr "VPIX",
                  vrpc_nvpt = sdRecordField dr "NVPT"
                }

sgcc :: ISO8211.DataRecord -> Maybe SGCC
sgcc r =
    case (findSubRecord' "SGCC" r) of
      Nothing -> Nothing
      Just dr -> Just SGCC {
                  sgcc_ccui = sdRecordField dr "VPUI",
                  sgcc_ccix = sdRecordField dr "VPIX",
                  sgcc_ccnc = sdRecordField dr "NVPT"
                }


vrpts :: ISO8211.DataRecord -> [VRPT]
vrpts = maybemdRecords "VRPT" vrpt

vrpt :: Map.Map String ISO8211.DataFieldT -> VRPT
vrpt m = VRPT {
         vrpt_name = mdRecordField "NAME" m,
         vrpt_ornt = mdRecordField "ORNT" m,
         vrpt_usag = mdRecordField "USAG" m,
         vrpt_topi = mdRecordField "TOPI" m,
         vrpt_mask = mdRecordField "MASK" m
       }

attvs :: ISO8211.DataRecord -> [ATTV]
attvs = maybemdRecords "ATTV" attv

attv :: Map.Map String ISO8211.DataFieldT -> ATTV
attv m = ATTV {
         attv_attl = mdRecordField "ATTL" m,
         attv_atvl = mdRecordField "ATVL" m
       }

sg2ds :: DSPM -> ISO8211.DataRecord -> [SG2D]
sg2ds dspm' = maybemdRecords "SG2D" (sg2d dspm')

sg2d :: DSPM -> Map.Map String ISO8211.DataFieldT -> SG2D
sg2d dspm' m =
  let x, y :: Integer
      x = mdRecordField "YCOO" m
      y = mdRecordField "XCOO" m
      mf = fromInteger $ dspm_comf dspm'
  in SG2D {
      sg2d_ycoo = (fromInteger y) / mf ,
      sg2d_xcoo = (fromInteger x) / mf
         }

sg3ds :: DSPM -> ISO8211.DataRecord -> [SG3D]
sg3ds dspm' = maybemdRecords "SG3D" (sg3d dspm')

sg3d :: DSPM -> Map.Map String ISO8211.DataFieldT -> SG3D
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


arccs :: DSPM -> ISO8211.DataRecord -> [ARCC]
arccs dspm = maybemdRecords "ARCC" $ arcc dspm

arcc :: DSPM -> Map.Map String ISO8211.DataFieldT -> ARCC
arcc dspm' m =
    let fpmf = mdRecordField "FMPF" m
        reso = fromInteger $ mdRecordField "RESO" m
    in ARCC {
             arcc_atyp = mdRecordField "ATYP" m,
             arcc_surf = mdRecordField "SURF" m,
             arcc_ordr = mdRecordField "ORDR" m,
             arcc_reso = reso / (fromInteger fpmf),
             arcc_fpmf = fpmf
           }



