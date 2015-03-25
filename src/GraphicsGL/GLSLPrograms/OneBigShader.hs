module GraphicsGL.GLSLPrograms.OneBigShader (
  initShaders
) where

import Graphics.Rendering.OpenGL.Raw

-- friends
import Util
import GraphicsGL
import Coordinate

initShaders :: ShadersGenerator
initShaders (w,h) = do
  programId <- compileGLSLSource glslSource
  glUseProgram programId
  --
  -- The co-ordinates are set to be the world co-ordinate system. This saves us
  -- converting for OpenGL calls
  --
  let bds = orthoBounds (w,h)
  ortho2D programId bds
  [positionLoc, texCoordLoc] <- mapM (getAttributeLocation programId) ["position", "texCoord"]
  [drawTextureLoc, colorLoc, functionLoc] <-
     mapM (getUniformLocation  programId) ["drawTexture", "color", "function"]
  let worldData =  WorldGLSL { worldGLSLPosition    = positionLoc
                             , worldGLSLTexcoord    = texCoordLoc
                             , worldGLSLDrawTexture = drawTextureLoc
                             , worldGLSLColor       = colorLoc
                             , worldGLSLOrthoBounds = bds
                             }
      worldInit = glUniform1i functionLoc 0
      worldGLSL = GLSLProgram { glslProgramId = programId
                              , glslData      = worldData
                              , glslInit      = worldInit
                              }
  let blurFactorNames = map (printf "blurFactor%d") [0..4 :: Int]
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
      blurInit = glUniform1i functionLoc 1
      blurGLSL = GLSLProgram { glslProgramId = programId
                             , glslData = blurData
                             , glslInit = blurInit
                             }
  return (worldGLSL, blurGLSL)



----------------------------------------------------------------------------------------------------
glslSource :: GLSLSource
glslSource =
  GLSLSource {
      glslVertexShader =
        concat $ intersperse "\n" [
            "#ifdef GL_ES"
          , "precision mediump float;"
          , "#endif"
          , "#define WORLD 0"
          , "#define BLUR  1"
          , "attribute vec3 position;"
          , "attribute vec2 texCoord;"
          , "uniform mat4 modelView;"
          , "uniform int function;"
          , "varying vec2 vTexCoord;"
          , ""
          , "void main()"
          , "{"
          , "  if (function == WORLD) {"
          , "    gl_Position = modelView * vec4(position,1);"
          , "  } else if (function == BLUR) {"
          , "    gl_Position = vec4(position, 1);"
          , "  }"
          , "  vTexCoord = texCoord;"
          , "}"
          ]

    , glslFragmentShader =
        concat $ intersperse "\n" [
            "#ifdef GL_ES"
          , "precision mediump float;"
          , "#endif"
          , "#define WORLD 0"
          , "#define BLUR  1"
          , "uniform int function;"
          , "uniform sampler2D texture;"
          , "uniform bool drawTexture;"
          , "uniform vec4 color;"
          , "uniform float blurFactor0;"
          , "uniform float blurFactor1;"
          , "uniform float blurFactor2;"
          , "uniform float blurFactor3;"
          , "uniform float blurFactor4;"
          , "uniform float radius;"
          , "uniform bool axis;"
          , "varying vec2 vTexCoord;"
          -----------
          , "float scale = 1.0/radius;"
          -----------
          , "vec4 blurComponent(float dist, float blurFactor) {"
          , "  if (axis) { // y-axis"
          , "   return texture2D(texture, vec2(vTexCoord.x, vTexCoord.y + dist*scale))*blurFactor;"
          , "  } else {"
          , "   return texture2D(texture, vec2(vTexCoord.x + dist*scale, vTexCoord.y))*blurFactor;"
          , "  }"
          , "}"
          -----------
--          , "bool condition = (axis || true) && (blurFactor0 + blurFactor1 + blurFactor2 + blurFactor3 + blurFactor4) < 1000.0;"
          -----------
          , "void main()"
          , "{"
          , "  if (function == WORLD) {"
          , "    if (drawTexture) {"
          , "      // sample the texture at the interpolated texture coordinate"
          , "      // and write it to gl_FragColor "
          , "      gl_FragColor = texture2D(texture, vTexCoord);"
          , "    } else {"
          , "      gl_FragColor = color;"
          , "    }"
          , "  } else if (function == BLUR) {"
          , "    vec4 sum = vec4(0.0);"
          , "    sum += blurComponent( 0.0, blurFactor0);"
          , "    sum += blurComponent( 1.0, blurFactor1);"
          , "    sum += blurComponent(-1.0, blurFactor1);"
          , "    sum += blurComponent( 2.0, blurFactor2);"
          , "    sum += blurComponent(-2.0, blurFactor2);"
          , "    sum += blurComponent( 3.0, blurFactor3);"
          , "    sum += blurComponent(-3.0, blurFactor3);"
          , "    sum += blurComponent( 4.0, blurFactor4);"
          , "    sum += blurComponent(-4.0, blurFactor4);"
          , "    gl_FragColor = vec4(sum.xyz, 1.0);"
--          , "    gl_FragColor = texture2D(texture, vTexCoord);"
          , "  }"
          , "}"
          ]
  }
