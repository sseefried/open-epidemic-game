module FSM where


import           Data.Map (Map)
import qualified Data.Map as M
import           Control.Monad.State

----------------------------------------------------------------------------------------------------
--
-- The [FSM] data structure full specifies a Finite State Machine along with its
-- transitions. [FSM] is parametrised on event, FSM state.
-- [ev], [ms].
--
--
data FSMTransitions m ms =
  FSMTransitions { fsmUnconditionals :: [m ()]
                 , fsmConditionals   :: [FSMCondTrans m ms] }

----------------------------------------------------------------------------------------------------
data FSMCondTrans m ms=
  FSMCondTrans { fsmCondTrans :: m Bool
               , fsmNextState :: ms }

----------------------------------------------------------------------------------------------------
type FSM m ev ms= Map ms (Map ev (FSMTransitions m ms))


----------------------------------------------------------------------------------------------------
-- [maybe] with a slightly different ordering of arguments. Useful
-- for continuations style programming.
maybe' :: b -> Maybe a -> (a -> b) -> b
maybe' deflt mb f = maybe deflt f mb

----------------------------------------------------------------------------------------------------
appConds :: Monad m => [FSMCondTrans m ms] -> ms -> m ms
appConds cts ms = go cts
  where
    go []       = return ms
    go (ct:cts) = do
      doTrans <- fsmCondTrans ct
      case doTrans of
        True  -> return (fsmNextState ct)
        False -> go cts

runFSM :: (Monad m, Ord ms, Ord ev) => FSM m ev ms-> ev -> ms -> m ms
runFSM fsm ev ms = do
  maybe' (return ms) (M.lookup ms fsm) $ \transMap ->
    maybe' (return ms) (M.lookup ev transMap) $ \transitions -> do
      sequence_ (fsmUnconditionals transitions)
      appConds (fsmConditionals transitions) ms