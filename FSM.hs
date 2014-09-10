module FSM where


import           Data.Map (Map)
import qualified Data.Map as M

----------------------------------------------------------------------------------------------------
--
-- The [FSM] data structure full specifies a Finite State Machine along with its
-- transitions. [FSM] is parametrised on event, FSM state and game state types,
-- [ev], [ms] and [s] respectively.
--
--
type Unconditional s    = s -> s
type Conditional  ms s  = (s -> (s,Bool), ms)

type FSM ev ms s = Map ms (Map ev ([Unconditional s], [Conditional ms s]))


-- [maybe] with a slightly different ordering of arguments. Useful
-- for continuations style programming.
maybe' :: b -> Maybe a -> (a -> b) -> b
maybe' deflt mb f = maybe deflt f mb

unaryFold :: [a -> a] -> a -> a
unaryFold []     a = a
unaryFold (f:fs) a = unaryFold fs (f a)

appConds :: [Conditional ms s] -> ms -> s -> (ms, s)
appConds [] ms s             = (ms, s)
appConds ((f,ms'):rest) ms s = case condition of
  True  -> (ms', s')           -- return now
  False -> appConds rest ms s' -- continue applying conditionals but on new state.
  where
    (s', condition) = f s


runFSM :: (Ord ms, Ord ev) => FSM ev ms s -> ev -> ms -> s -> (ms, s)
runFSM fsm ev ms s =
  maybe' (ms, s) (M.lookup ms fsm) $ \m ->
  maybe' (ms, s) (M.lookup ev m) $ \(unconditionals, conditionals) ->
  let s' = unaryFold unconditionals s in
  appConds conditionals ms s'
