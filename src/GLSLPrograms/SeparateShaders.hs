module GLSLPrograms.SeparateShaders (
  initShaders
) where

import Graphics.Rendering.OpenGL.Raw

-- friends
import Util
import GLM
import GraphicsGL.Util
import Coordinate


----------------------------------------------------------------------------------------------------
worldGLSLSource :: GLSLSource
worldGLSLSource =
  GLSLSource {
      glslVertexShader =
        concat $ intersperse "\n" [
            "#ifdef GL_ES"
          , "precision mediump float;"
          , "#endif"
          , "attribute vec3 position;"
          , "attribute vec2 texCoord;"
          , "uniform mat4 modelView;"
          , "varying vec2 vTexCoord;"
          , ""
          , "void main()"
          , "{"
          , "  gl_Position = modelView * vec4(position,1);"
          , "  vTexCoord = texCoord;"
          , "}"
          ]

    , glslFragmentShader =
        concat $ intersperse "\n" [
            "#ifdef GL_ES"
          , "precision mediump float;"
          , "#endif"
          , "uniform sampler2D texture;"
          , "uniform bool drawTexture;"
          , "uniform vec4 color;"
          , " "
          , "varying vec2 vTexCoord;"
          , " "
          , "void main()"
          , "{"
          , "  if (drawTexture) {"
          , "    // sample the texture at the interpolated texture coordinate"
          , "    // and write it to gl_FragColor "
          , "    gl_FragColor = texture2D(texture, vTexCoord);"
          , "  } else {"
          , "    gl_FragColor = color;"
          , "  }"
          , "}"
          ]
  }

----------------------------------------------------------------------------------------------------
blurGLSLSource :: GLSLSource
blurGLSLSource =
  GLSLSource {
      glslVertexShader =
        concat $ intersperse "\n" [
            "#ifdef GL_ES"
          , "precision mediump float;"
          , "#endif"
          , "attribute vec3 position;"
          , "attribute vec2 texCoord;"
          , "varying vec2   vTexCoord;"
          , ""
          , "void main()"
          , "{"
          , "  gl_Position = vec4(position,1);"
          , "  vTexCoord = texCoord;"
          , "}"
          ]

    , glslFragmentShader =
        concat $ intersperse "\n" [
            "#ifdef GL_ES"
          , "precision mediump float;"
          , "#endif"
          , "uniform sampler2D texture;"
          , "varying vec2 vTexCoord;"
          , ""
          , "uniform float blurFactor0;"
          , "uniform float blurFactor1;"
          , "uniform float blurFactor2;"
          , "uniform float blurFactor3;"
          , "uniform float blurFactor4;"
          , "uniform float radius;"
          , ""
          , "uniform bool axis;"
          , ""
          , "float scale = 1.0/radius;"
          , "vec4 blurComponent(float dist, float blurFactor) {"
          , "  if (axis) { // y-axis"
          , "   return texture2D(texture, vec2(vTexCoord.x, vTexCoord.y + dist*scale))*blurFactor;"
          , "  } else {"
          , "   return texture2D(texture, vec2(vTexCoord.x + dist*scale, vTexCoord.y))*blurFactor;"
          , "  }"
          , "}"
          , ""
          , "void main() {"
          , "  vec4 sum = vec4(0.0);"
          , "  sum += blurComponent( 0.0, blurFactor0);"
          , "  sum += blurComponent( 1.0, blurFactor1);"
          , "  sum += blurComponent(-1.0, blurFactor1);"
          , "  sum += blurComponent( 2.0, blurFactor2);"
          , "  sum += blurComponent(-2.0, blurFactor2);"
          , "  sum += blurComponent( 3.0, blurFactor3);"
          , "  sum += blurComponent(-3.0, blurFactor3);"
          , "  sum += blurComponent( 4.0, blurFactor4);"
          , "  sum += blurComponent(-4.0, blurFactor4);"
          , ""
          , "  gl_FragColor = vec4(sum.xyz, 1.0);"
          --,  "if ((axis ? 1.0 : 1.0) + (blurFactor0 + blurFactor1 + blurFactor2 +"
          --,  "  blurFactor3 + blurFactor4) > 0.0) {gl_FragColor =  texture2D(texture, vTexCoord); } else { gl_FragColor =  texture2D(texture, vTexCoord); }"
          , "}"
        ]
  }

----------------------------------------------------------------------------------------------------
initShaders :: (Int, Int) -> IO (GLSLProgram WorldGLSL, GLSLProgram BlurGLSL)
initShaders bds = do
  worldGLSL <- initWorldGLSL bds
  blurGLSL  <- initBlurGLSL bds
  return (worldGLSL, blurGLSL)

----------------------------------------------------------------------------------------------------
initWorldGLSL :: (Int, Int) -> IO (GLSLProgram WorldGLSL)
initWorldGLSL (w,h) = do
  programId <- compileGLSLSource worldGLSLSource
  --
  -- The co-ordinates are set to be the world co-ordinate system. This saves us
  -- converting for OpenGL calls
  --
  let bds = orthoBounds (w,h)
  ortho2D programId bds
  [positionLoc, texCoordLoc] <- mapM (getAttributeLocation programId) ["position", "texCoord"]
  [drawTextureLoc, colorLoc] <- mapM (getUniformLocation  programId) ["drawTexture", "color"]
  let worldData =  WorldGLSL { worldGLSLPosition    = positionLoc
                             , worldGLSLTexcoord    = texCoordLoc
                             , worldGLSLDrawTexture = drawTextureLoc
                             , worldGLSLColor       = colorLoc
                             , worldGLSLOrthoBounds = bds
                             }
  return $ GLSLProgram { glslProgramId = programId
                       , glslData = worldData
                       , glslInit = return ()
                       }

----------------------------------------------------------------------------------------------------
initBlurGLSL :: (Int, Int) -> IO (GLSLProgram BlurGLSL)
initBlurGLSL (w, h) = do
  programId <- compileGLSLSource blurGLSLSource
  glUseProgram programId
  let blurFactorNames = map (printf "blurFactor%d") [0..4 :: Int]
  [positionLoc, texCoordLoc] <- mapM (getAttributeLocation programId) ["position", "texCoord"]
  [axis,radius, bf0, bf1, bf2, bf3, bf4] <- mapM (getUniformLocation programId)
                                                 ("axis":"radius":blurFactorNames)
  fbo <- genFBO (w,h)
  glUniform1f radius (fromIntegral $ min w h)
  let blurData = BlurGLSL { blurGLSLPosition  = positionLoc
                          , blurGLSLTexcoord  = texCoordLoc
                          , blurGLSLFactor0   = bf0
                          , blurGLSLFactor1   = bf1
                          , blurGLSLFactor2   = bf2
                          , blurGLSLFactor3   = bf3
                          , blurGLSLFactor4   = bf4
                          , blurGLSLAxis      = axis
                          , blurGLSLPhase1FBO = fbo
                          }
  return $ GLSLProgram { glslProgramId = programId
                       , glslData = blurData
                       , glslInit = return ()
                       }