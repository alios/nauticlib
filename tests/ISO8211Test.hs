module Main(main) where

import Data.S57
import Data.S57.ISO8211

import Data.Attoparsec(parseOnly)
import qualified Data.ByteString as BS
import Text.Show.Pretty (ppShow)
import System.IO


main = do
 r <- fmap (parseOnly parseDataFile ) $ BS.readFile "/home/alios/tmp/US5TX51M.000"
 case r of
   Left err -> print err
   Right f -> do
          putStr . ppShow $ dsid_dssi f
          print $ dsid_uadt f
          print $ dsid_prsp f
          print $ dsid_prof f
          print $ dsid_dssi_dstr f
          print $ dsid_dssi_nall f

