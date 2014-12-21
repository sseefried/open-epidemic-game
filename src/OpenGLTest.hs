{-# LANGUAGE ScopedTypeVariables #-}
module Main where


import qualified Graphics.UI.SDL          as S
import qualified Graphics.UI.SDL.Surface  as S
import qualified Graphics.UI.SDL.Video    as S
import qualified Graphics.UI.SDL.Keycode  as SK
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.Cairo as C


import           Data.IORef
import           Data.Time
import           Text.Printf
import           Control.Monad
import           Control.Monad.Random
import           System.Exit
import           Foreign.C.Types (CUChar)
import           Foreign.Marshal.Alloc (mallocBytes)
import           Foreign.Ptr (Ptr, castPtr)
import           Foreign.C.Types (CFloat)
import           Data.Vector (Vector)
import qualified Data.Vector as V


-- friends
import           Graphics

germFramesPerSecond = 30
germSeconds = 20

numFrames :: Int
numFrames = germFramesPerSecond * germSeconds

----------------------------------------------------------------------------------------------------
powOfTwo = 9
w, h :: Int
w = 2^powOfTwo
h = w
----------------------------------------------------------------------------------------------------
w', h' :: Double
w' = fromIntegral w
h' = fromIntegral h

w'' :: GL.GLsizei
w'' = fromIntegral w

----------------------------------------------------------------------------------------------------
data State = State { stWindow      :: S.Window
                   , stContext     :: S.GLContext
                   , stTextureObj  :: GL.TextureObject
                   , stTime        :: Double
                   , stFrames      :: Int
                   , stStartTime   :: UTCTime
                 }
----------------------------------------------------------------------------------------------------
textureWidths = map (2^) [powOfTwo, powOfTwo - 1 ..0]


time :: String -> IO a -> IO a
time msg io = do
  t <- getCurrentTime
  r <- io
  t' <- getCurrentTime
  let diff = ((du2d $ diffUTCTime t' t) :: Double)
  printf "%s: %.2fs (%.1f Hz) \n" msg diff (1/diff)
  return r


----------------------------------------------------------------------------------------------------
main :: IO ()
main = do
  S.init [S.InitVideo]
  -- These attributes need to be set before the window is created.
  mapM_ setAttrib [ {-(S.GLDoubleBuffer, 1),-} (S.GLDepthSize, 24) ]
  window   <- S.createWindow "Test" (S.Position 0 0) (S.Size w h) [S.WindowShown, S.WindowOpengl]
  context <- S.glCreateContext window

  --S.glSetSwapInterval S.SynchronizedUpdates
  S.glSetSwapInterval S.ImmediateUpdates
  germGfx <- fmap (\g -> g { germGfxSpikes = 3 }) $ evalRandIO $ randomGermGfx

  putStrLn $ show $ germGfxSpikes germGfx

  -- put germ in the buffer
  let drawToBuffer x buf t =
        C.withImageSurfaceForData buf C.FormatARGB32 x x (x*4) $ \surface ->
           C.renderWith surface $ do
             let x' = fromIntegral x
             C.setSourceRGBA 1 1 1 1
             C.rectangle 0 0 (fromIntegral x) (fromIntegral x)
             C.fill
             drawGerm germGfx (x,x) (x'/2,x'/2) (x'/2) t

--  GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
  GL.texture GL.Texture2D $= GL.Enabled

  [textureObj :: GL.TextureObject] <- GL.genObjectNames 1

  time "Drawing" $ do
      GL.textureBinding GL.Texture2D $= Just textureObj
      GL.depthFunc $= Just GL.Less
      GL.shadeModel $= GL.Flat
      GL.textureFilter GL.Texture2D $= ((GL.Linear', Just GL.Linear'), GL.Linear')
      GL.textureFunction $= GL.Decal
      forM_ (zip textureWidths [0..]) $ \(x,i) -> do
        let x' = fromIntegral x
        buffer <- mallocBytes (x*x*4)
        drawToBuffer x buffer 0
        GL.texImage2D GL.Texture2D GL.NoProxy i GL.RGBA' (GL.TextureSize2D x' x') 0
          (GL.PixelData GL.RGBA GL.UnsignedByte buffer)

  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GL.perspective 60 (fromIntegral w / fromIntegral h) 0 60000
  GL.matrixMode $= GL.Modelview 0
  GL.loadIdentity
  t <- getCurrentTime
  ----
  stRef <- newIORef $ State { stWindow      = window
                            , stContext     = context
                            , stTextureObj  = textureObj
                            , stTime        = 0
                            , stFrames      = 0
                            , stStartTime   = t
                            }
  mainLoop stRef
  where
    setAttrib = uncurry S.glSetAttribute

----------------------------------------------------------------------------------------------------
checkForQuit :: IO Bool
checkForQuit = do
  mbEv <- S.pollEvent
  case mbEv of
    Just ev -> handleKey ev
    Nothing -> return False
  where
    handleKey ev =
      return (case S.eventData ev of
                  S.Quit -> True
                  S.Keyboard { S.keyMovement = S.KeyDown, S.keySym = S.Keysym _ SK.Q _ } ->
                    True
                  _ -> False)

du2d :: NominalDiffTime -> Double
du2d f = (fromRational . toRational) f

----------------------------------------------------------------------------------------------------
loop :: IO a -> IO a
loop io = io >> loop io

----------------------------------------------------------------------------------------------------
mainLoop :: IORef State -> IO ()
mainLoop stRef = loop $ do
  st <- readIORef stRef
  GL.clearColor $= GL.Color4 1 1 1 1
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]

  let texCoord2f = GL.texCoord :: GL.TexCoord2 GL.GLfloat -> IO ()
      vertex3f   = GL.vertex   :: GL.Vertex3 GL.GLfloat   -> IO ()

  let maxZoom = 2
  let square x = x * x
  let siner = (((sin $ stTime st) + 1)/2) -- varies between 0 and 1
  let a = (fromRational . toRational) $ 1/ (square (1 + maxZoom * siner))
  let a = 1
  let dx = 0.05 * sin (fromIntegral (stFrames st) / 5)
  let idx = stFrames st `mod` numFrames
  --GL.textureBinding GL.Texture2D $= Just ((stTextureObjs st) V.! idx)

  forM [0..4000] $ \_ -> do
    GL.renderPrimitive GL.TriangleStrip $ do
      texCoord2f (GL.TexCoord2 (0.00-dx) (0.50))   ; vertex3f (GL.Vertex3 (-a  ) (0    )  0)
      texCoord2f (GL.TexCoord2 (0.25-dx) (1.00+dx)); vertex3f (GL.Vertex3 (-a/2) (a    )  0)
      texCoord2f (GL.TexCoord2 (0.50) (0.50))      ; vertex3f (GL.Vertex3 ( 0  ) (0    )  0)
      texCoord2f (GL.TexCoord2 (0.75   ) (1.00+dx)); vertex3f (GL.Vertex3 ( a/2) (a    )  0)
      texCoord2f (GL.TexCoord2 (1.00   ) (0.50))   ; vertex3f (GL.Vertex3 ( a  ) (0    )  0)
      texCoord2f (GL.TexCoord2 (1.00   ) (0.50))   ; vertex3f (GL.Vertex3 ( a  ) (0    )  0)
      texCoord2f (GL.TexCoord2 (0.75+dx) (0.00))   ; vertex3f (GL.Vertex3 ( a/2) (-a   )  0)
      texCoord2f (GL.TexCoord2 (0.50) (0.50))      ; vertex3f (GL.Vertex3 ( 0  ) ( 0   )  0)
      texCoord2f (GL.TexCoord2 (0.25) (0.00))      ; vertex3f (GL.Vertex3 (-a/2) (-a   )  0)
      texCoord2f (GL.TexCoord2 (0.00-dx) (0.50))   ; vertex3f (GL.Vertex3 (-a  ) (0    )  0)


  GL.flush
--  S.delay 16

  S.glSwapWindow (stWindow st)
  b <- checkForQuit
  when b $ do
    t <- getCurrentTime
    printf "Frames: %d\n" (stFrames st)
    printf "Av. Frames/s: %.2f\n" (du2d $ fromIntegral (stFrames st) / diffUTCTime t (stStartTime st))
    exitWith ExitSuccess
  modifyIORef stRef $ \st -> st { stTime = stTime st + 1/30, stFrames = stFrames st + 1 }
