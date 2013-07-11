module Main(main) where

import Data.S57
import Data.S57.ISO8211

import Data.Attoparsec(parseOnly)
import qualified Data.ByteString as BS
import Text.Show.Pretty (ppShow)
import System.IO

--fn = "/home/alios/src/nauticlib/test_data/ENC_ROOT/US5FL12M/US5FL12M.000"
fn = "/home/alios/src/nauticlib/test_data/ENC_ROOT/CATALOG.031"
--fn = "/home/alios/tmp/US5TX51M.000"

enc_root = "/home/alios/src/nauticlib/test_data/ENC_ROOT/"

df = enc_root ++ "US4FL10M/US4FL10M.000"

main = do
 r <- fmap (parseOnly parseDataFile ) $ BS.readFile df
 case r of
   Left err -> print err
   Right f@(ddr, rs) -> do
          let x = df_frids
          let pf = s57dataFile $ f
          putStr . ppShow . x $ pf
--          putStr . ppShow . take 3 $ rs 
          print ""
{-          print $ dsid_uadt f
          print $ dsid_prsp f
          print $ dsid_prof f
          print $ dsid_dssi_dstr f
          print $ dsid_dssi_nall f
-}
