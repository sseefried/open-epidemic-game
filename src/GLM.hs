--
--
-- Here's the beauty of the GLM monad.
--
--  You can only run GLM monads inside the GameM monad (not IO)
--
-- The downside at present is that it's very easy to just use [liftGLM] or [glm]
-- to inject arbitrary IO actions into the GameM monad.
--
-- The right, but time consuming, thing to do would be to wrap every single GL call
-- in the GLM monad and only expose those.
--
-- For now, try to enforce the discipline of only creating GLM things from the building blocks
-- in module [GraphicsGL]
--
module GLM (
  GLM, -- opaque
  glm, -- smart constructor
  liftGLM,
  getGfxState,
  runGLMIO
) where

import Control.Applicative

-- friends
import Types.Basic

data GLM a = GLM { unGLM :: GfxState -> IO a }

instance Functor GLM where
  -- (a -> b) -> (GLM a -> GLM b)
  fmap f (GLM g) = GLM $ fmap f . g

instance Monad GLM where
  return = GLM . const . return
  (GLM f) >>= k = GLM $ \as -> f as >>= \a -> unGLM (k a) as

instance Applicative GLM where
  pure = return
  (GLM f) <*> (GLM f') = GLM $ liftA2 (<*>) f f'

--
-- Lifts an [IO] in to the [GLM] monad.
--
liftGLM :: IO a -> GLM a
liftGLM io = GLM $ const io

-- Same as the GLM constructor. The difference is that [GLM] is opaque and can't be
-- pattern matched against
--
glm :: (GfxState -> IO a) -> GLM a
glm = GLM

getGfxState :: GLM GfxState
getGfxState = GLM return

runGLMIO :: GfxState -> GLM a -> IO a
runGLMIO glsls (GLM f) = f glsls