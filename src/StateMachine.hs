{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

module StateMachine
  ( StateMachine(..)
  , Transition(..)
  , Guard(..)
  , transitionTo
  ) where

data StateMachine object state where
  StateMachine :: Eq state => {
    object :: object,
    currentState :: state,
    transitions :: [Transition state],
    guards :: [Guard object state]
  } -> StateMachine object state

data Transition state =
  Transition state -- ^ from
             state -- ^ to
  deriving (Eq, Show)

data Guard object state
  = GuardFrom state
              (object -> Bool)
  | GuardTo state
            (object -> Bool)
  | GuardFromTo state state
                (object -> Bool)

transitionTo :: StateMachine o s -> s -> Maybe (StateMachine o s)
transitionTo machine@(StateMachine object _ transitions guards) state =
  if validTransition machine state
    then Just $ StateMachine object state transitions guards
    else Nothing

validTransition :: StateMachine o s -> s -> Bool
validTransition (StateMachine _ _ [] _) _ = False
validTransition (StateMachine obj current transitions guards) next =
    transitionExists && guardsHappy
    where transitionExists = Transition current next `elem` transitions
          guardsHappy = and $ map (\g -> runGuard g obj) relevantGuards
          relevantGuards = guardsFor current next guards

runGuard :: Guard o s -> o -> Bool
runGuard (GuardFrom _ f) = f
runGuard (GuardTo _ f) = f
runGuard (GuardFromTo _ _ f) = f

guardsFor :: Eq s => s -> s -> [Guard o s] -> [Guard o s]
guardsFor from to guards = filter match guards
  where match (GuardFrom f _) = f == from
        match (GuardTo t _) = t == to
        match (GuardFromTo f t _) = f == from && t == to
