module GameEvent where

-- friends
import Types

----------------------------------------------------------------------------------------------------
--
-- Events
-- ~~~~~~
--
-- This game may be implemented on a variety of backends, some which have mice/touchpads,
-- some which have touch screens, some which have keyboards and some that don't. One approach
-- would be for the game logic to understand the basic concepts of 'keypress', 'mouse click',
-- 'touch event', etc, but a more abstract way to do things is simply to have a notion of
-- "game events" such as "germ squash", "quit", "continue to next level", etc.
-- It is then the backend's job to correctly translate its events to these game events.
--
-- This actually saves me a lot of work. For instance, were we to have a notion of 'keypress'
-- in the game logic then you would have to account for every different kind of key that could
-- be pressed, which is not only a large number of keys but would likely be different between
-- the different backends. (Some would leave out certain keys not common on all keyboards, some
-- would have them. I'd have to choose a subset that worked for all of them, etc.)
--
data Event = Tap R2       -- location at which tap occurred.
           | Select R2
           | Unselect R2
           | Drag R2 R2
           | Physics Time -- how much time the last frame took
           | Reset
           | Pause    -- home button pushed
           | Resume   -- back in the game
           deriving (Show, Eq, Ord)

