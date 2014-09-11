{-# LANGUAGE ScopedTypeVariables #-}
module FSM where


import           Data.Map (Map)
import qualified Data.Map as M
import           Control.Monad.State
import           Control.Applicative

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
data FSMCondTrans m ms =
  FSMCondTrans { fsmCondTrans :: m Bool
               , fsmNextState :: ms }

----------------------------------------------------------------------------------------------------
data FSM m ev ms = FSM { fsmAnyStateTransitions  :: Map ev (FSMTransitions m ms)
                       , fsmDependentTransitions :: Map ms (Map ev (FSMTransitions m ms)) }

----------------------------------------------------------------------------------------------------
-- [maybe] with a slightly different ordering of arguments. Useful
-- for continuations style programming.
maybe' :: b -> Maybe a -> (a -> b) -> b
maybe' deflt mb f = maybe deflt f mb

----------------------------------------------------------------------------------------------------
appConds :: Monad m => [FSMCondTrans m ms] -> m (Maybe ms)
appConds cts = go cts
  where
    go []       = return Nothing
    go (ct:cts) = do
      doTrans <- fsmCondTrans ct
      case doTrans of
        True  -> return . Just . fsmNextState $ ct
        False -> go cts

runFSM :: forall m ev ms. (Monad m, Functor m, Ord ms, Ord ev) => FSM m ev ms -> ev -> ms -> m ms
runFSM fsm ev ms = do
  mbFSMState <- foo (fsmAnyStateTransitions fsm) ms
  case mbFSMState of
    Just ms' -> return ms'
    Nothing  -> maybe' (return ms) (M.lookup ms (fsmDependentTransitions fsm)) $ \transMap ->
                  maybe ms id <$> foo transMap ms
  where
    foo :: Map ev (FSMTransitions m ms) -> ms -> m (Maybe ms)
    foo evMap ms =
      maybe' (return $ Just ms) (M.lookup ev evMap) $ \transitions -> do
        sequence_ (fsmUnconditionals transitions)
        appConds  (fsmConditionals transitions)
