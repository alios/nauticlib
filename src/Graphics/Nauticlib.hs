{-# LANGUAGE OverloadedStrings #-}

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

module Graphics.Nauticlib
    ( renderdocs
    , Symbol (..)
    , ACHARE02 (..)
    , ACHARE51 (..)
    , ACHBRT07 (..)
    , ACHRES51 (..)
    , ACHRES61 (..)
    , ACHRES71 (..)
    ) where


import qualified Data.ByteString.Lazy        as BL
import           Data.String
import qualified Data.Text                   as T
import qualified Data.Text.Lazy.Builder      as T
import qualified Data.Text.Lazy.Encoding     as T
import           System.FilePath
import           Text.Blaze.Internal
import           Text.Blaze.Renderer.Utf8    (renderMarkup)
import           Text.Blaze.Svg11            (Svg, l, m, mkPath, (!))
import qualified Text.Blaze.Svg11            as S
import qualified Text.Blaze.Svg11.Attributes as A
import           Text.CSS.Render


class (Show c) => Symbol c where
    symPivotPoint  :: c -> (Double, Double)
    symBoundingBox :: c -> (Double, Double)
    symSvg :: c -> Svg
    symUseRef :: c -> S.Attribute
    symUseRef s = A.xlinkHref (S.toValue $ "#" ++ symId s)
    symUse :: c -> Double -> Double -> Svg
    symUse s x y = S.use ! A.x (S.toValue x) ! A.y (S.toValue y) ! symUseRef s
    symId :: c -> String
    symId = show
    symSvg s =
        let (px,py) = symPivotPoint s
            (bx,by) = symBoundingBox s
            tr = S.translate (-px) (-py)
        in (S.g ! A.transform tr
                ! A.id_ (S.toValue . symId  $ s)
                ! A.class_ (S.toValue $ symId s)
                ! A.width (S.toValue $ show bx ++ "mm")
                ! A.height (S.toValue $ show by ++ "mm")
           ) $ symSvg_ s
    symSvg_ :: c -> Svg

data ACHARE02 = ACHARE02 deriving (Show, Eq)
data ACHARE51 = ACHARE51 deriving (Show, Eq)
data ACHBRT07 = ACHBRT07 deriving (Show, Eq)
data ACHRES51 = ACHRES51 deriving (Show, Eq)
data ACHRES61 = ACHRES61 deriving (Show, Eq)
data ACHRES71 = ACHRES71 deriving (Show, Eq)
data AIRARE02 = AIRARE02 deriving (Show, Eq)


instance Symbol ACHARE02 where
    symPivotPoint  _ = (2.06, 2.63)
    symBoundingBox _ = (4.02, 5.03)
    symSvg_ s =
        S.path
             ! A.fillOpacity "0"
             ! A.class_ (lookupColorClass CHMFG)
             ! A.strokeWidth "0.3"
             ! A.strokeLinecap "round"
             ! A.d anchor
        where (bx,by) = symBoundingBox s
              bax = 0.97
              (tax,tay) = (1.56, 1.49)
              bax' = (bx / 2) - bax
              anchor = mkPath $ do
                           S.m (bx/2) 0
                           S.v by
                           S.h bax
                           S.lr (-bax') (-bax')
                           S.mr bx 0
                           S.lr (-bax') bax'
                           S.l (bx/2) by
                           S.m ((bx/2) - tax) tay
                           S.l ((bx/2) + tax) tay


instance Symbol ACHARE51 where
    symPivotPoint _ = (6.29, 7.79)
    symBoundingBox _ = (12.29, 13.04)
    symSvg_ s =
        S.path
             ! A.fillOpacity "0"
             ! A.class_ (lookupColorClass CHMGD)
             ! A.strokeWidth "0.3"
             ! A.d anchor
        where (bx,by) = symBoundingBox s
              (swx, swy) = (0.75, 1.12)
              anchor =
                  mkPath $ do
                           S.m ((bx / 2) + (swx / 2))  0
                           S.lr (swx /2) 0
                           S.lr 0 3.29
                           S.lr 3.18 0
                           S.lr 0 1.12
                           S.lr (-3.18) 0
                           S.lr 0 (by - (1.79 + 1.12 + 3.29) )

                           S.l (bx - 4)  (by - 1.79)

                           S.l (bx - 1.12) (by - 3.66)
                           S.lr 1.5 0

                           S.lr (-2) (3.66 - 1.79)

                           S.l (bx / 2) by

                           S.l 2 (by - 1.79)

                           S.l 0 (by - 3.66)
                           S.lr 1.5 0

                           S.l 4 (by - 1.79)

                           S.l ((bx / 2) - (swx / 2)) (by - 1.79)
                           S.lr 0 (negate $ by - (1.79 + 1.12 + 3.29))
                           S.lr (-3.18) 0
                           S.lr 0 (-1.12)
                           S.lr 3.18 0
                           S.lr 0 (-3.29)
                           S.z



instance Symbol ACHBRT07 where
    symPivotPoint  _ = (2.54, 2.79)
    symBoundingBox _ = (5.06, 5.06)
    symSvg_ s = S.g
             ! A.class_ (lookupColorClass CHMFG)
             ! A.strokeWidth "0.3"
             ! A.strokeLinecap "round"
             ! A.fillOpacity "0"
                   $ do
        S.path
             ! A.d anchor
        S.circle
             ! A.cx (S.toValue (bx / 2))
             ! A.cy "3.22"
             ! A.r "1.01"
        where (bx,by) = symBoundingBox s
              bax = 1.02
              (tax,tay) = (1.55, 1.54)
              bax' = (bx / 2) - bax
              anchor = mkPath $ do
                           S.m (bx/2) 0
                           S.lr 0 (3.22 - 1.01)
                           S.mr 0 2.02
                           S.l (bx/2) by
                           S.lr 1.02 0
                           S.lr (bax') (-bax')
                           S.m (bx / 2) by
                           S.lr (-1.02) 0
                           S.lr (-bax') (-bax')
                           S.m ((bx/2) - tax) tay
                           S.l ((bx/2) + tax) tay



instance Symbol ACHRES51 where
    symPivotPoint _ = (-6.40, -5.13)
    symBoundingBox _ = (14.29, 13.04)
    symSvg_ s = S.g $ do
      let (ax, ay) = symPivotPoint ACHARE51
      S.use ! A.x (S.toValue ax) ! A.y (S.toValue ay) ! symUseRef ACHARE51
      symSvg_ ACHARE51
      S.line
           ! A.fillOpacity "0"
           ! A.class_ (lookupColorClass CHMGD)
           ! A.strokeWidth "0.9"
           ! A.strokeLinecap "round"
           ! A.x1 "0.80"
           ! A.y1 "12.08"
           ! A.x2 "10.82"
           ! A.y2 "2.0"



instance Symbol ACHRES61 where
    symPivotPoint _ = (-6.40, -5.13)
    symBoundingBox _ = (14.35, 13.04)
    symSvg_ s = S.g $ do
      let (ax, ay) = symPivotPoint ACHRES51
      S.use ! A.x (S.toValue ax) ! A.y (S.toValue ay) ! symUseRef ACHRES51
      S.g ! A.class_ (lookupColorClass CHMFG)
          ! A.strokeLinecap "round" $ do
                           S.line
                            ! A.strokeWidth "0.3"
                            ! A.x1 (S.toValue mx)
                            ! A.y1 (S.toValue my)
                            ! A.x2 (S.toValue mx)
                            ! A.y2 (S.toValue (my - 3.46))
                           S.circle
                            ! A.strokeWidth "0.3"
                            ! A.cx (S.toValue mx)
                            ! A.cy (S.toValue $ by - 0.3)
                            ! A.r "0.3"
        where (bx,by) = symBoundingBox s
              mx = ((bx / 2) + 7.69)
              my = by - 1.34

instance Symbol ACHRES71 where
    symPivotPoint _ = (-4.22, -5.13)
    symBoundingBox _ = (14.47, 13.04)
    symSvg_ s = S.g $ do
      let (ax, ay) = symPivotPoint ACHRES51
      S.use ! A.x (S.toValue $ ax + 1.88) ! A.y (S.toValue ay) ! symUseRef ACHRES51

      S.path ! A.class_ (lookupColorClass CHMFG)
             ! A.fillOpacity "0"
             ! A.strokeWidth "0.3"
             ! A.strokeLinecap "round"
             ! A.d infoi

        where (bx,by) = symBoundingBox s
              mx = ((bx / 2) + 7.69)
              my = by - 1.34
              infoi = mkPath $ do
                        S.m 0 by
                        S.lr 1.88 0
                        S.m (1.88 / 2) by
                        S.lr 0 (-2.5)
                        S.lr ((-1.88) / 2) 0
                        S.m 0.5 (by - 3)
                        S.lr (-0.25) (-0.54)


instance Symbol AIRARE02 where
    symPivotPoint  _ = (3.98, 3.98)
    symBoundingBox _ = (7.96, 7.96)
    symSvg_ s =
        S.g  ! A.class_ (lookupColorClass LANDF)
             ! A.strokeWidth "0.3"
             ! A.fillOpacity "0" $ do
          S.circle ! A.cx (S.toValue (bx / 2))
                   ! A.cy (S.toValue (bx / 2))
                   ! A.r  (S.toValue ((7.96 :: Double) / 2.0))
          S.path ! A.transform tr  ! A.d airplane
        where (bx,by) = symBoundingBox s
              (ax,ay) = (6.18, 5.28)
              tr = S.translate ((bx - ax) / 2) ((by - ay) / 2)
              airplane = mkPath $ do
                           S.m (ax/2) ay
                           S.lr (2.36 / 2) 0
                           S.lr ((2.36 - 1.06) / (-2)) (-0.5)
                           S.lr 0 ((-1.79) + 0.5)
                           S.lr ((6.18 - 1.06) / 2) 0
                           S.lr ((6.18 - 1.30) / (-2)) (-1.5)
                           S.lr 0 (-1.5)
                           S.lr ((-1.30) / 2) ((-1.30) / 2)

                           S.lr ((-1.30) / 2) ((1.30) / 2)
                           S.lr 0 1.5
                           S.lr ((ax-2.36)/ (-2))  1.5
                           S.lr ((ax-1.06) / 2) 0
                           S.lr 0 (1.79 - 0.5)
                           S.lr ((2.36 - 1.06) / (-2)) 0.5
                           S.lr (2.36 / 2) 0
defs = S.toMarkup
       [ symSvg ACHARE02
       , symSvg ACHARE51
       , symSvg ACHBRT07
       , symSvg ACHRES51
       , symSvg ACHRES61
       , symSvg ACHRES71
       , symSvg AIRARE02
       ]


docTypeCSS :: T.Text -> Svg
docTypeCSS c = do
  preEscapedText "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
  xmlstylesheetcss c
  preEscapedText "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\n    \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n"

xmlstylesheet t h =
    preEscapedText $ T.concat [
                        "<?xml-stylesheet href=\"", h, "\" type=\"", t, "\"?>"]
xmlstylesheetcss = xmlstylesheet "text/css"


docTypeSvgCSS :: T.Text -> Svg -> Svg
docTypeSvgCSS c inner =
    docTypeCSS c >> (S.svg
      ! attribute "xmlns" " xmlns=\"" "http://www.w3.org/2000/svg"
      ! attribute "xmlns:xlink" " xmlns:xlink=\"" "http://www.w3.org/1999/xlink"  $ inner)




ts = renderdocs "/home/alios/tmp/" "test" $ do
  symUse ACHRES71 0 0
  symUse ACHRES61 20 0
  symUse ACHRES51 40 0
  symUse ACHBRT07 70 0
  symUse ACHARE51 90 0
  symUse ACHARE02 110 0
  symUse AIRARE02 120 0


renderdocs :: FilePath -> String -> Svg -> IO ()
renderdocs d f inner = do
  let wFile e = BL.writeFile $ d </> (f `addExtension` e)
  wFile "svg" $ renderMarkup $ svgDoc (f `addExtension` "css") inner
  wFile "css" $ cssDoc


data SymbolColor = CHMGD
                 | CHMFG
                 | LANDF
                 deriving (Eq, Show)

lookupColorClass = S.toValue . show


cssDoc :: BL.ByteString
cssDoc =
    T.encodeUtf8 . T.toLazyText $ renderBlocks
    [ ( ".CHMGD", [("stroke", "plum")
                  ,("fill", "plum")
                  ])
    , ( ".CHMFG", [("stroke", "darkmagenta")
                  ,("fill", "darkmagenta")
                  ])
    , ( ".LANDF", [("stroke", "brown")
                  ,("fill", "brown")
                  ])
    ]

svgDoc :: String -> Svg -> Svg
svgDoc f inner = do
  docTypeSvgCSS (fromString f)
       ! A.version "1.1"
       ! A.width "800"
       ! A.height "600"
       ! A.viewbox "-10 -10 150 50" $ do
           S.defs defs
           inner
