module GLSLPrograms where

import           Data.List

-- friends
import GLM

----------------------------------------------------------------------------------------------------
glslVertexShaderDefault :: String
glslVertexShaderDefault =
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

----------------------------------------------------------------------------------------------------
worldGLSLProgram :: GLSLProgram
worldGLSLProgram =
  GLSLProgram {
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
blurGLSLProgram :: GLSLProgram
blurGLSLProgram =
  GLSLProgram {
      glslVertexShader = glslVertexShaderDefault
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
--
-- A GLSL program that just passes a texture straight through.
--
_idGLSLProgram :: GLSLProgram
_idGLSLProgram =
  GLSLProgram {
      glslVertexShader   =
        concat $ intersperse "\n" [
            "#ifdef GL_ES"
          , "precision mediump float;"
          , "#endif"
          , "attribute vec3 position;"
          , "attribute vec2 texCoord;"
          , "varying vec2 vTexCoord;"
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
          , "void main()"
          , "{"
          , "  gl_FragColor = texture2D(texture, vTexCoord);"
          , "}"
          ]
  }
