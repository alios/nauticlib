{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TemplateHaskell           #-}

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

module Data.S57Conduit
    ( dataFile
    , dataFileIO
    , isoFile
    , isoFileIO

    ) where

import           Codec.Archive.Zip
import           Control.Monad.Trans
import           Data.ByteString         (ByteString)
import           Data.Conduit
import           Data.Conduit.Attoparsec
import           Data.Conduit.Filesystem
import qualified Data.Conduit.List       as CL
import           Data.Data
import qualified Data.ISO8211.Parser     as ISO8211
import           Data.Monoid
import           Data.S57
import           Data.SafeCopy           (base, deriveSafeCopy)
import           Data.Typeable
import           Filesystem.Path
import           Prelude                 hiding (FilePath)
--type S57DataSet = ISO8211.DataFile
--type S57DataFile = ISO8211.DataFile



dsidConduit :: (Monad m) => Conduit (ISO8211.DataFile) m DSID
dsidConduit = awaitForever $ \df -> do
  dsidConduit

{-
s57dataSetProducer :: (MonadResource m) => FilePath -> Consumer m S57DataSet
s57dataSetProducer fp =
    case (extension fp) of
      Nothing -> s57dataSetDirConduit fp
      Just "zip" -> s57dataSetZipConduit fp
      Just e ->
          fail $ "s57dataSetProducer: unknown extension " ++ show e ++ " in " ++ show fp

-}


isoFile :: MonadResource m => FilePath -> m (ISO8211.DataFile)
isoFile fp = sourceFile fp $$ dataFileConsumer
isoFileIO = runResourceT . isoFile

dataFile :: MonadResource m => FilePath -> m DataFileS57
dataFile fp = fmap s57dataFile $ isoFile fp

dataFileIO :: FilePath -> IO DataFileS57
dataFileIO = runResourceT . dataFile

dataFileConsumer :: (MonadThrow m) => Consumer ByteString m (ISO8211.DataFile)
dataFileConsumer = sinkParser $ ISO8211.parseDataFile

catalogFilename :: FilePath -> FilePath
catalogFilename dir = dir </> encRootName </> catalogName
    where encRootName = "ENC_ROOT"
          catalogName = "CATALOG.031"


data DataSetS57 = DataSetS57 {
      ds_dsid :: [DSID],
      ds_dspm :: Maybe DSPM,
      ds_dsht :: Maybe DSHT,
      ds_dsac :: Maybe DSAC
    } deriving (Eq, Show)

emptyDataSetS57 = DataSetS57 [] Nothing Nothing Nothing


-- | represents a catalog of 'DataSet's
newtype DataSetCatalog = DataSetCatalog [CATD]
    deriving (Eq, Ord, Read, Show, Data, Typeable)


instance Monoid DataSetCatalog where
    mempty = DataSetCatalog []
    mappend (DataSetCatalog  a) (DataSetCatalog b) = DataSetCatalog $ a ++ b

-- $(deriveSafeCopy 0 'base ''DataSetCatalog)

catalogC :: Monad m =>  Consumer ISO8211.DataRecord m DataSetCatalog
catalogC = catalogC' mempty
    where
      catalogC' :: Monad m => DataSetCatalog -> Consumer ISO8211.DataRecord m DataSetCatalog
      catalogC' cat@(DataSetCatalog cs) = do
        r' <- await
        case r' of
          Nothing -> return cat
          Just r -> case undefined of
                     Nothing -> fail $ "catalogC: unexpected record: " ++ show r
                     Just c ->
                         catalogC' $ DataSetCatalog (c:cs)


y :: (MonadIO m ) => ISO8211.DataDescriptiveRecord -> DataSetS57 -> Consumer ISO8211.DataRecord m DataSetS57
y = undefined
{-
y ddr ds = do
  n <- await
  case n of
    Nothing -> return ds
    Just dr ->
         let dsid_ = dsid (ddr, [dr])
             ds_ =
                 case dsid_ of
                   Just dsid_' -> ds { ds_dsid = Just dsid_' }
                   Nothing -> ds
         in y ddr ds_


x = x' Nothing emptyDataSetS57



x' :: (MonadIO m ) => Maybe DSID -> DataSetS57 -> Consumer ISO8211.DataFile m DataSetS57
x' Nothing ds = do
    awaitForever $ \(ddr, drs') ->
        let (dr, drs) = splitAt 1 drs'
        in case (dsid (ddr, [dr])) of
             Nothing -> fail $ "first record in datafile must be DSID"
             Just dsid' -> yield (ddr, drs) $= x' (Just dsid') (ds { ds_dsid = ((ds_dsid ds) ++ dsid') })
x' (Just dsid') ds = do
  n <- await
  case n of
    Nothing -> return ds
    Just (ddr, drs) -> do
           ds_ <- CL.sourceList drs $$ (y ddr ds)
           x' ds_
 -}

{-
dataSet d = do
  cat <- dataFile $ catalogFilename d
  liftIO $ print cat
-}

tfp :: FilePath
tfp = "/home/alios/src/nauticlib/test_data/"

--t = runResourceT $ dataSet tfp


{-
s57dataSetDirConduit :: (MonadResource m) => FilePath -> Consumer m S57DataSet
s57dataSetDirConduit fp = do
  catalog <- (sourceFile $ catalogFilename fp) $$ dataFileConsumer
  liftIO $ print catalog
  return undefined

s57dataSetZipConduit :: (MonadResource m) => FilePath -> Consumer m S57DataSet
s57dataSetZipConduit fp = undefined
-}


