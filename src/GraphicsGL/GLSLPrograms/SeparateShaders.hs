module GraphicsGL.GLSLPrograms.SeparateShaders (
  initShaders
) where

import Graphics.Rendering.OpenGL.Raw

-- friends
import Util
import GraphicsGL.GLM
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
          , "precision lowp int;"
          , "#endif"
          ---
          , "attribute vec3 position;"
          , "attribute vec2 texCoord;"
          ----
          , "uniform float radius;"
          , "uniform bool axis;"
          ----
          , "varying vec2 blurTexCoords[9];"
          ----
          , "float scale = 1.0/radius;"
          ----
          , "void main()"
          , "{"
          , "  gl_Position = vec4(position,1);"
          , "  float axisX = axis ? 0.0 : 1.0;"
          , "  float axisY = axis ? 1.0 : 0.0;"
          , "  blurTexCoords[0] = texCoord;"
          , "  blurTexCoords[1] = vec2(texCoord.x +     axisX*scale, texCoord.y +     axisY*scale);"
          , "  blurTexCoords[2] = vec2(texCoord.x -     axisX*scale, texCoord.y -     axisY*scale);"
          , "  blurTexCoords[3] = vec2(texCoord.x + 2.0*axisX*scale, texCoord.y + 2.0*axisY*scale);"
          , "  blurTexCoords[4] = vec2(texCoord.x - 2.0*axisX*scale, texCoord.y - 2.0*axisY*scale);"
          , "  blurTexCoords[5] = vec2(texCoord.x + 3.0*axisX*scale, texCoord.y + 3.0*axisY*scale);"
          , "  blurTexCoords[6] = vec2(texCoord.x - 3.0*axisX*scale, texCoord.y - 3.0*axisY*scale);"
          , "  blurTexCoords[7] = vec2(texCoord.x + 4.0*axisX*scale, texCoord.y + 4.0*axisY*scale);"
          , "  blurTexCoords[8] = vec2(texCoord.x - 4.0*axisX*scale, texCoord.y - 4.0*axisY*scale);"
          , "}"
          ]

    , glslFragmentShader =
        concat $ intersperse "\n" [
            "#ifdef GL_ES"
          , "precision mediump float;"
          , "precision lowp int;"
          , "#endif"
          , "uniform sampler2D texture;"
          ----
          , "varying vec2 blurTexCoords[9];"
          ----
          , "uniform float blurFactor0;"
          , "uniform float blurFactor1;"
          , "uniform float blurFactor2;"
          , "uniform float blurFactor3;"
          , "uniform float blurFactor4;"
          ----
          --, "vec4 blurComponent(int i, float blurFactor) {"
          --, "   return texture2D(texture, blurTexCoords[i])*blurFactor;"
          --, "}"
          ----
          , "void main() {"
          , "  lowp vec4 sum = vec4(0.0);"
          , "  sum += texture2D(texture, blurTexCoords[0])*blurFactor0;"
          , "  sum += texture2D(texture, blurTexCoords[1])*blurFactor1;"
          , "  sum += texture2D(texture, blurTexCoords[2])*blurFactor1;"
          , "  sum += texture2D(texture, blurTexCoords[3])*blurFactor2;"
          , "  sum += texture2D(texture, blurTexCoords[4])*blurFactor2;"
          , "  sum += texture2D(texture, blurTexCoords[5])*blurFactor3;"
          , "  sum += texture2D(texture, blurTexCoords[6])*blurFactor3;"
          , "  sum += texture2D(texture, blurTexCoords[7])*blurFactor4;"
          , "  sum += texture2D(texture, blurTexCoords[8])*blurFactor4;"
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