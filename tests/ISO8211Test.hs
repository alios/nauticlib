module Main(main) where

import Data.S57.ISO8211
import Data.Attoparsec(parseOnly)
import qualified Data.ByteString as BS
import Text.Show.Pretty (ppShow)
import System.IO


main = do
 r <- fmap (parseOnly parseDataFile ) $ BS.readFile "/home/alios/tmp/US5TX51M.000"
 case r of
   Left err -> print err
   Right (ddr, drs) -> do
          putStr $ ppShow $ ddr
          putStr $ ppShow $ drs 

