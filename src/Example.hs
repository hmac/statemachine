module Example where

import Data.Maybe (isJust)
import StateMachine

-- Object
data Payment = Payment { amount :: Int, payoutId :: Maybe Int }

-- States
data PaymentState = Pending | Submitted | Paid | PaidOut | Failed deriving (Eq)

-- Transitions
paymentTransitions :: [Transition PaymentState] 
paymentTransitions = [ Transition Pending Submitted
                     , Transition Submitted Paid
                     , Transition Paid Failed
                     ]

-- Guards
paymentGuards :: [Guard Payment PaymentState]
paymentGuards = [ GuardTo PaidOut (isJust . payoutId) ]

-- Machine
machine :: Payment -> PaymentState -> StateMachine Payment PaymentState
machine payment initialState = StateMachine { object = payment
                                            , currentState = initialState
                                            , transitions = paymentTransitions
                                            , guards = paymentGuards
                                            }
