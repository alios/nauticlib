{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

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

module Data.S57Conduit (
) where



import           Codec.Archive.Zip
import           Data.Conduit
import           Data.Conduit.Attoparsec
import           Data.Conduit.Filesystem
import           Data.S57
import qualified Data.Text               as T
import           Filesystem.Path
import           Prelude                 hiding (FilePath)

type S57DataSet = ()
type S57DataFile = ()


s57dataSetProducer :: (Monad m) => FilePath -> Producer m S57DataSet
s57dataSetProducer fp =
    case (extension fp) of
      Nothing -> s57dataSetDirConduit fp
      Just "zip" -> s57dataSetZipConduit fp
      Just e ->
          fail $ "s57dataSetProducer: unknown extension " ++ show e ++ " in " ++ show fp


s57dataSetDirConduit :: FilePath -> Producer m S57DataSet
s57dataSetDirConduit fp = undefined


s57dataSetZipConduit :: FilePath -> Producer m S57DataSet
s57dataSetZipConduit fp = undefined



