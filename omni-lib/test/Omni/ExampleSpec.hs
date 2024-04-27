module Omni.ExampleSpec (spec) where

import Control.Monad
import Omni.Example (hello)
import Test.Syd

spec :: Spec
spec =
  describe "hello" $ do
    pending "test"
    forM_ [0 .. 10] $
      \i -> it "says hello" $ context (show i) $ hello `shouldBe` "Hello"