module HipM (
  HipM, HipSpace,
  -- HipM smart constructors
  hipStep,
  addHipCirc,
  getHipCircPos,
  getHipCircVel,
  setHipCircPos,
  setHipCircVel,
  setHipCircPosVel,
  setHipCircRadius,
  addHipStaticPoly,
  setGravity,
  setDamping,
  removeHipCirc,
  --
  runHipMIO
) where

import           Control.Monad.Free
import qualified Physics.Hipmunk as H
import qualified Physics.Hipmunk.Unsafe as H
import qualified Data.StateVar as H
import           Data.StateVar (($=))

-- friends
import Types



data HipScript next =
    HipStep          !Double next
  | AddHipCirc       !Double !R2 (HipCirc -> next)
  | GetHipCircPos    !HipCirc (R2 -> next)
  | GetHipCircVel    !HipCirc (R2 -> next)
  | SetHipCircPos    !HipCirc R2 next
  | SetHipCircVel    !HipCirc R2 next
  | SetHipCircRadius !HipCirc !Double next
  | AddHipStaticPoly ![R2] next
  | RemoveHipCirc    !HipCirc next
  | SetGravity       !R2 next
  | SetDamping       !Double next

type HipM  = Free HipScript

instance Functor HipScript where
  fmap f _hs = case _hs of
    HipStep dt hs             -> HipStep dt (f hs)
    AddHipCirc a b g          -> AddHipCirc a b (f . g)
    GetHipCircPos a g         -> GetHipCircPos a (f . g)
    GetHipCircVel a g         -> GetHipCircVel a (f . g)
    SetHipCircPos a b hs      -> SetHipCircPos a b (f hs)
    SetHipCircVel a b hs      -> SetHipCircVel a b (f hs)
    SetHipCircRadius a b hs   -> SetHipCircRadius a b (f hs)
    AddHipStaticPoly a hs     -> AddHipStaticPoly a (f hs)
    RemoveHipCirc a hs        -> RemoveHipCirc a (f hs)
    SetGravity v hs           -> SetGravity v (f hs)
    SetDamping d hs           -> SetDamping d (f hs)

----------------------------------------------------------------------------------------------------
-- Smart constructors for HipM
hipStep :: Double -> HipM ()
hipStep dt = Impure (HipStep dt (Pure ()))

addHipCirc :: Double -> R2 -> HipM HipCirc
addHipCirc r pos = Impure (AddHipCirc r pos Pure)

getHipCircPos :: HipCirc -> HipM R2
getHipCircPos c = Impure (GetHipCircPos c Pure)

getHipCircVel :: HipCirc -> HipM R2
getHipCircVel c = Impure (GetHipCircVel c Pure)

setHipCircPos :: HipCirc -> R2 -> HipM ()
setHipCircPos c p = Impure (SetHipCircPos c p (Pure ()))

setHipCircVel :: HipCirc -> R2 -> HipM ()
setHipCircVel c v = Impure (SetHipCircVel c v (Pure ()))

setHipCircPosVel :: HipCirc -> R2 -> R2 -> HipM ()
setHipCircPosVel c p v = setHipCircPos c p >> setHipCircVel c v

setHipCircRadius :: HipCirc -> Double -> HipM ()
setHipCircRadius c r = Impure (SetHipCircRadius c r (Pure ()))

addHipStaticPoly :: [R2] -> HipM ()
addHipStaticPoly pts = Impure (AddHipStaticPoly pts (Pure ()))

removeHipCirc :: HipCirc -> HipM ()
removeHipCirc c = Impure (RemoveHipCirc c (Pure ()))

setGravity :: R2 -> HipM ()
setGravity v = Impure (SetGravity v (Pure ()))

setDamping :: Double -> HipM ()
setDamping d = Impure (SetDamping d (Pure ()))

runHipMIO :: HipSpace -> HipM a -> IO a
runHipMIO space = go
  where
    go :: HipM a -> IO a
    go _p = case _p of
      (Impure (HipStep dt p))                 -> H.step space dt >> go p
      (Impure (AddHipCirc r pos f))           -> runAddHipCirc r pos >>= go . f
      (Impure (GetHipCircPos hipCirc f))      -> runGetHipCircPos hipCirc >>= go . f
      (Impure (GetHipCircVel hipCirc f))      -> runGetHipCircVel hipCirc >>= go . f
      (Impure (SetHipCircPos hipCirc pos p))  -> runSetHipCircPos hipCirc pos >> go p
      (Impure (SetHipCircVel hipCirc vel p))  -> runSetHipCircVel hipCirc vel >> go p
      (Impure (SetHipCircRadius hipCirc r p)) -> runSetHipCircRadius hipCirc r >> go p
      (Impure (AddHipStaticPoly pts p))       -> runAddHipStaticPoly pts >> go p
      (Impure (RemoveHipCirc hipCirc p))      -> runRemoveHipCirc hipCirc >> go p
      (Impure (SetGravity v p))               -> runSetGravity space v >> go p
      (Impure (SetDamping d p))               -> runSetDamping space d >> go p
      Pure x                                  -> return x

    runAddHipCirc :: Double -> R2 -> IO HipCirc
    runAddHipCirc r (R2 x y) = do
      let pos = H.Vector x y
      b <- H.newBody 1 H.infinity
      H.spaceAdd space b
      H.position b $= pos
      s <- H.newShape b (H.Circle r) (H.Vector 0 0) -- (0,0) offset
      H.spaceAdd space s
      return $ HipCirc s

    runGetHipCircPos :: HipCirc -> IO R2
    runGetHipCircPos (HipCirc s) = do
      H.Vector x y <- H.get $ H.position $ H.body s
      return $ R2 x y

    runGetHipCircVel :: HipCirc -> IO R2
    runGetHipCircVel (HipCirc s) = do
      H.Vector x y <- H.get $ H.velocity $ H.body s
      return $ R2 x y

    runSetHipCircPos :: HipCirc -> R2 -> IO ()
    runSetHipCircPos (HipCirc s) (R2 x y) = do
      let pos = H.Vector x y
          b   = H.body s
      H.position b $= pos
      return ()

    runSetHipCircVel :: HipCirc -> R2 -> IO ()
    runSetHipCircVel (HipCirc s) (R2 vx vy) = do
      let vel = H.Vector vx vy
          b   = H.body s
      H.velocity b $= vel
      return ()

    runSetHipCircRadius :: HipCirc -> Double -> IO ()
    runSetHipCircRadius (HipCirc s) r = do
      H.unsafeShapeRedefine s (H.Circle r) (H.Vector 0 0) -- (0,0) offset

    runAddHipStaticPoly :: [R2] -> IO ()
    runAddHipStaticPoly pts = do
      b <- H.newBody H.infinity H.infinity
      -- Don't add the body to the space. It's static.
      let pts' = map r2ToPos pts
      s <- H.newShape b (H.Polygon pts') (H.Vector 0 0)
      when (not (H.isClockwise pts')) $ do
        error "Points aren't clockwise!"
      H.spaceAdd space (H.Static s)
      return ()
      where
        r2ToPos :: R2 -> H.Vector
        r2ToPos (R2 x y) = H.Vector x y

    runRemoveHipCirc :: HipCirc -> IO ()
    runRemoveHipCirc (HipCirc s) = do
      H.spaceRemove space (H.body s)
      H.spaceRemove space s

    runSetGravity :: H.Space -> R2 -> IO ()
    runSetGravity space (R2 x y) = H.gravity space $= H.Vector x y

    runSetDamping :: H.Space -> Double -> IO ()
    runSetDamping space d = H.damping space $= d
