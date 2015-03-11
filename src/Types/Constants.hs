module Types.Constants where

import qualified Data.Map as M
import           Data.Map (Map)

-- friends
import Types.Basic
import Platform

-- -------------------
-- CO-ORDINATE SYSTEMS
-- -------------------
--
-- For the game we're going to a 4:3 aspect ratio (regardless of the screen).
-- Preferably we get the full height of the screen but for actual screens with aspect ratio
-- smaller than 4:3 we letter box.
--
--
-- There are 4 co-ordinate systems. Because we are using OpenGL and Cairo we
-- can use floating point values in all 3.
--
-- "left-handed" means x-axis goes left to right and y-axis top to bottom.
-- "right-handed" means x-axis goes left to right and y-axis bottom to top.
--
--
-- 1. Screen.
--    Type:   integral
--    System: left-handed
--    Origin: top-left
--    Width:  screen width
--    Height: screen height
--
--    SDL Mouse input comes in this co-ordinate system.
--
-- 2. Normalized
--    Type: fractional
--    System: left-handed
--    Origin: top-left
--    Width: 1.0
--    Height: 1.0
--
--    SDL Touch input comes in this system. This one is interesting because the
--    aspect ratio is 1, not the aspect ratio of the screen. If the screen is wider than high
--    then the the x-axis is "stretched". The distance from 0 to 1 on the x-axis is longer than
--    the same distance on the y-axis, with respect to the screen.
--
-- 3. Canvas
--    Type: fractional
--    System: right-handed
--    Origin: centre.
--    Width:  (see below)
--    Height: (see below)
--
--    The canvas co-ordinate system is for Cairo graphics. When rendering Cairo graphics to
--    image files the co-ordinate system is normally a left-handed one. However, because
--    we are rendering to OpenGL textures which use the World co-ordinate system it becomes
--    a right-handed system.
--
--    The width and height of the canvas are somewhat meaningless as we are rendering
--    to textures of arbitrary size. However, be careful. If you are not using mipmapping
--    the texture will not look good unless the *screen* width/height of the texture.
--    Since the OpenGL context is set to World co-ordinates you will need to scale from
--    World co-ordinates to Screen co-ordinates.
--
-- 4. World (fractional)
--
--    The *world* is split into a *side bar* and a *game field*. The terms have a precise
--    definition. The *world* is the *side bar* plus the *game field*.
--    The game field has an aspect ratio of [fieldAspectRatio]. The remainder is for
--    the side bar. The side bar is to the left of the game field.
--
--    If it is possible to map the full height of the world to the full height of the screen this
--    is done. This is possible when [worldAspectRatio] < screen aspect ratio. Otherwise it is
--    letter boxed.
--
--    The OpenGL context is set up to match the world co-ordinate system. The origin is
--    the centre of the *game field* not the centre of the *game area*.
--
--    # Game Field
--
--    The game field will always have bounds
--      (fieldLeft,       fieldRight,    fieldBottom,      fieldTop)
--    = (-fieldWidth/2, fieldWidth/2, -fieldHeight/2, fieldHeight/2)
--
--    However, because of letter boxing the bounds for the OpenGL othographic bounds may be
--    different.
--
--    # Side bar
--
--    The side bar will have bounds (sideBarLeft, fieldLeft, fieldBottom, fieldTop)
--    where
--      sideBarWidth = worldWidth - fieldWidth
--      sideBarLeft = sideBarLeft - sideBarWidth
--


----------------------------------------------------------------------------------------------------
-- Constants (feel free to change)
--
levelCompleteGrad, gameOverGrad, scoreGrad :: Gradient
levelCompleteGrad = (Color 0.09 0.37 0.16 1, Color 0.09 0.80 0.16 1)
gameOverGrad      = (Color 0.53 0.18 0.18 1, Color 0.73 0.18 0.18 1)
scoreGrad         = (Color 0.0 0.0 0.5 1,    Color 0.0  0.0  1.0  1)
continueGrad      = (Color 0 0 0 1,          Color 0.8  0.8  0.8  1)

--
-- Game constants
--
fieldWidth, fieldHeight :: Double
fieldWidth  = 100
fieldHeight = 100

-- Aspect ratio of entire game area.
worldAspectRatio :: Double
worldAspectRatio = 4/3 -- must be greater than [fieldAspectRatio]. Rest is for side bar

initialGermSize :: Double
initialGermSize = worldHeight / 30

initialGermSizeVariance :: Double
initialGermSizeVariance = worldHeight / 150

--
-- The doubling period starts at this value.
--
initialDoublingPeriod :: Double
initialDoublingPeriod = 3

-- A factor by which the doublingPeriod is decreased each time a germ is birthed.
doublingPeriodDecreaseFactor :: Double
doublingPeriodDecreaseFactor = 0.95

doublingPeriodVarianceFraction :: Double
doublingPeriodVarianceFraction = 0.2 -- as a fraction of the doubling period.

resistanceIncrease :: Double
resistanceIncrease = 1.1

-- Maximum number of germs before you are "infected"
maxGerms :: Int
maxGerms = 50

startingAntibioticEffectiveness :: Double
startingAntibioticEffectiveness = 0.92 -- percentage

-- Amount we multiply effectiveness by each time antibiotic is used
effectivenessDilutionFactor :: Double
effectivenessDilutionFactor = 0.90

-- the maximum amount that a component of a Color (R,G,B) can
-- mutate by (either up or down)
gradientColorMutationMax :: Double
gradientColorMutationMax = 0.15

--
-- When a level is finished or an antibiotics is unlocked we need to "mute" the events
-- for a certain amount of time so that a stray click or finger tap doesn't immediately
-- send the player to the next level before they've even notice they finished the level.
--
eventMuteTime :: Double
eventMuteTime = 1.5 -- seconds

unlockAntibioticsMap :: Map Int Antibiotic
unlockAntibioticsMap = M.fromList $ case debugGame of
  True  -> [(5, Penicillin), (10, Ciprofloxacin)]
  False -> [(25, Penicillin), (100, Ciprofloxacin)]

-- [antibioticColor] generates a color for the liquid in the antibiotic based on
-- the antibiotic and its effectiveness
antibioticRGB :: Antibiotic -> (Double,Double,Double)
antibioticRGB ab = case ab of
  Penicillin    -> (0,1,0)
  Ciprofloxacin -> (1,0,1)

desiredFramerate :: Double
desiredFramerate = 60

----------------------------------------------------------------------------------------------------
-- Derived constants. (Do not change)

worldWidth, worldHeight :: Double
worldHeight = fieldHeight
worldWidth  = worldAspectRatio * fieldWidth

sideBarWidth :: Double
sideBarWidth = worldWidth - fieldWidth

fieldAspectRatio :: Double
fieldAspectRatio = worldWidth/worldHeight

fieldLeft, fieldRight, fieldBottom, fieldTop :: Double
fieldLeft   = -fieldWidth/2
fieldRight  =  fieldWidth/2
fieldBottom = -fieldHeight/2
fieldTop    =  fieldHeight/2

sideBarLeft, sideBarRight, sideBarBottom, sideBarTop :: Double
sideBarLeft   = fieldLeft - sideBarWidth
sideBarRight  = fieldLeft
sideBarBottom = fieldBottom
sideBarTop    = fieldTop

worldLeft, worldRight, worldBottom, worldTop :: Double
worldLeft   = sideBarLeft
worldRight  = fieldRight
worldBottom = fieldBottom
worldTop    = fieldTop

antibioticWidth = sideBarWidth * 0.6

startLevelGerms :: Int
startLevelGerms = 4

-----------------------------------------------

-- FIXME: This probably needs to be moved somewhere.
-- [antibioticColor] generates a color for the liquid in the antibiotic based on
-- the antibiotic and its effectiveness
antibioticColor :: Antibiotic -> Double -> Color
antibioticColor ab effectiveness =
  let (r,g,b) = antibioticRGB ab
  in Color r g b (effectiveness/startingAntibioticEffectiveness)