import Test.Hspec
import StateMachine
import Data.Maybe

main :: IO ()
main = hspec $ do
  testTransitions
  testGuardTo
  testGuardFrom
  testGuardFromTo

testTransitions = 
  let states = ["pending", "submitted", "paid"]
      transitions = [Transition "pending" "submitted", Transition "submitted" "paid"]
      guards = []
      object = "someObj"
      machine = StateMachine { object = object, currentState = "pending", transitions = transitions, guards = guards }
  in do
    it "is able to transition pending -> submitted" $
      (currentState . fromJust) (transitionTo machine "submitted") `shouldBe` "submitted"
    it "isn't able to transition pending -> paid" $
      isNothing (transitionTo machine "paid") `shouldBe` True
    it "isn't able to transition pending -> pending" $
      isNothing (transitionTo machine "pending") `shouldBe` True
    it "isn't able to transition to a nonexistent state" $
      isNothing (transitionTo machine "woof") `shouldBe` True

testGuardTo =
  let states = ["pending", "submitted"]
      transitions = [Transition "pending" "submitted"]
      guards = [GuardTo "submitted" (\o -> o == (2 :: Int))]
      machine obj = StateMachine { object = obj, currentState = "pending", transitions = transitions, guards = guards }
  in
    it "enforces GuardTo" $ do
      isNothing (transitionTo (machine 1) "submitted") `shouldBe` True
      (currentState . fromJust) (transitionTo (machine 2) "submitted") `shouldBe` "submitted"

testGuardFrom =
  let states = ["pending", "submitted", "paid"]
      transitions = [Transition "pending" "submitted", Transition "submitted" "paid"]
      guards = [GuardFrom "pending" (\o -> o == (2 :: Int))]
      machine obj curState = StateMachine { object = obj, currentState = curState, transitions = transitions, guards = guards }
  in
    it "enforces GuardFrom" $ do
      isNothing (transitionTo (machine 1 "pending") "submitted") `shouldBe` True
      (currentState . fromJust) (transitionTo (machine 2 "pending") "submitted") `shouldBe` "submitted"
      (currentState . fromJust) (transitionTo (machine 1 "submitted") "paid") `shouldBe` "paid"

testGuardFromTo =
  let states = ["pending", "submitted", "paid"]
      transitions = [Transition "pending" "submitted", Transition "submitted" "paid"]
      guards = [GuardFrom "pending" (\o -> o == (2 :: Int))]
      machine obj curState = StateMachine { object = obj, currentState = curState, transitions = transitions, guards = guards }
  in
    it "enforces GuardFromTo" $ do
      isNothing (transitionTo (machine 1 "pending") "submitted") `shouldBe` True
      (currentState . fromJust) (transitionTo (machine 2 "pending") "submitted") `shouldBe` "submitted"
      (currentState . fromJust) (transitionTo (machine 1 "submitted") "paid") `shouldBe` "paid"
