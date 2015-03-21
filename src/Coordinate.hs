module Coordinate where

-- friends
import Types
import CUtil
import Foreign (CFloat)


----------------------------------------------------------------------------------------------------
data BackendToWorld = BackendToWorld { backendPtToWorldPt     :: (Int, Int) -> R2
                                     , backendNormPtToWorldPt :: (CFloat, CFloat) -> R2 }

----------------------------------------------------------------------------------------------------
backendToWorld ::  (Int, Int) -> BackendToWorld
backendToWorld (w,h) =
  BackendToWorld { backendPtToWorldPt = \(x,y) -> R2 ((fromIntegral x)  * scale + left)
                                                      (top - (fromIntegral y) * scale)
                 , backendNormPtToWorldPt = \(fx,fy) -> R2 ((cf2d fx * w' * scale) + left)
                                                           (top - (cf2d fy * h' * scale))
                 }
  where
    bds      = orthoBounds (w,h)
    left     = orthoLeft bds
    top      = orthoTop bds

    scale    = 1/(screenScale bds)
    w'       = fromIntegral w
    h'       = fromIntegral h
    cf2d     = cFloatToDouble

----------------------------------------------------------------------------------------------------
--
-- Calculates the bounds of the visible screen in terms of world co-ordinates.
--
orthoBounds :: (Int, Int) -> OrthoBounds
orthoBounds (w,h) =
  OrthoBounds { orthoLeft   =  worldLeft   - (lbhm / scale)
              , orthoRight  =  worldRight  + (lbhm / scale)
              , orthoBottom =  worldBottom - (lbvm / scale)
              , orthoTop    =  worldTop    + (lbvm / scale)
              , screenScale =  scale
              }
  where
    aspectRatio :: Double
    aspectRatio = w'/h'
    w', h' :: Double
    w' = fromIntegral w
    h' = fromIntegral h
    scale = realToFrac $ lbsh / worldHeight
    letterBoxScreenHeight = if aspectRatio > worldAspectRatio
                             then h' else h' * (aspectRatio/worldAspectRatio)
    lbsh = letterBoxScreenHeight
    letterBoxVertMargin = (h' - lbsh)/2
    lbvm = letterBoxVertMargin

    letterBoxScreenWidth = if aspectRatio > worldAspectRatio
                            then w' * (worldAspectRatio/aspectRatio) else w'
    lbsw = letterBoxScreenWidth
    letterBoxHorizMargin = (w' - lbsw)/2
    lbhm = letterBoxHorizMargin
