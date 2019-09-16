module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import LLL
import Test.Hspec
import Test.QuickCheck.Gen
import Test.QuickCheck.Monadic

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel do

  describe "concurrency primitives" $ parallel do

    let
      async' action = do
        delay <- pick $ (* 100) <$> choose (1, 5)
        run $ async $ threadDelay delay *> action
      oneOfMany' = run . oneOfMany @Int

    describe "trivial" do
      it "-> done" $ monadicIO do
        x <- oneOfMany' []
        pure case x of
          Right Nothing -> True
          Right{} -> False
          Left{} -> False

    describe "singleton" do

      it "done -> done" $ monadicIO do
        a <- async' $ pure Nothing
        x <- oneOfMany' [a]
        pure case x of
          Right Nothing -> True
          Right{} -> False
          Left{} -> False

      it "return(a) -> return(a)" $ monadicIO do
        a <- async' $ pure $ Just 1
        x <- oneOfMany' [a]
        pure case x of
          Right (Just 1) -> True
          Right{} -> False
          Left{} -> False

      it "fail(a) -> fail(a)" $ monadicIO do
        a <- async' $ throwIO $ AbortException "a"
        x <- oneOfMany' [a]
        pure case x of
          Left (fromException -> Just (AbortException "a")) -> True
          Right{} -> False
          Left{} -> False

    describe "parallel" do

      it "done | done -> done" $ monadicIO do
        a <- async' $ pure Nothing
        b <- async' $ pure Nothing
        x <- oneOfMany' [a, b]
        pure case x of
          Right Nothing -> True
          Right{} -> False
          Left{} -> False

      it "done | return(b) -> return(b)" $ monadicIO do
        a <- async' $ pure Nothing
        b <- async' $ pure $ Just 2
        x <- oneOfMany' [a, b]
        pure case x of
          Right (Just 2) -> True
          Right{} -> False
          Left{} -> False

      it "done | fail(b) -> fail(b)" $ monadicIO do
        a <- async' $ pure Nothing
        b <- async' $ throwIO $ AbortException "b"
        x <- oneOfMany' [a, b]
        pure case x of
          Left (fromException -> Just (AbortException "b")) -> True
          Right{} -> False
          Left{} -> False

      it "return(a) | done -> return(a)" $ monadicIO do
        a <- async' $ pure $ Just 1
        b <- async' $ pure Nothing
        x <- oneOfMany' [a, b]
        pure case x of
          Right (Just 1) -> True
          Right{} -> False
          Left{} -> False

      it "return(a) | return(b) -> error" $ monadicIO do
        a <- async' $ pure $ Just 1
        b <- async' $ pure $ Just 2
        x <- run $ try $ oneOfMany @Int [a, b]
        pure case x of
          Left (fromException -> Just InternalError{}) -> True
          Left{} -> False
          Right Left{} -> False
          Right Right{} -> False

      it "return(a) | fail(b) -> fail(b)" $ monadicIO do
        a <- async' $ pure $ Just 1
        b <- async' $ throwIO $ AbortException "b"
        x <- oneOfMany' [a, b]
        pure case x of
          Left (fromException -> Just (AbortException "b")) -> True
          Right{} -> False
          Left{} -> False

      it "fail(a) | done -> fail(a)" $ monadicIO do
        a <- async' $ throwIO $ AbortException "a"
        b <- async' $ pure Nothing
        x <- oneOfMany' [a, b]
        pure case x of
          Left (fromException -> Just (AbortException "a")) -> True
          Right{} -> False
          Left{} -> False

      it "fail(a) | return(b) -> fail(a)" $ monadicIO do
        a <- async' $ throwIO $ AbortException "a"
        b <- async' $ pure $ Just 2
        x <- oneOfMany' [a, b]
        pure case x of
          Left (fromException -> Just (AbortException "a")) -> True
          Right{} -> False
          Left{} -> False

      it "fail(a) | fail(b) -> fail(?)" $ monadicIO do
        a <- async' $ throwIO $ AbortException "a"
        b <- async' $ throwIO $ AbortException "b"
        x <- oneOfMany' [a, b]
        pure case x of
          Left (fromException -> Just (AbortException message))
            | message `elem` ["a", "b"] -> True
          Right{} -> False
          Left{} -> False

  describe "evaluation" $ parallel do

    it "lexical scope" do

      -- let x = 2 in
      -- let y = 3 in
      -- (fun y -> x * y) x
      result <- LLL.evaluate
        ( Let (Bind (Name "x") Nothing) (NumberIntro 2)
        $ Let (Bind (Name "y") Nothing) (NumberIntro 3)
        $ ImpliesElim
          (ImpliesIntro (Bind (Name "y") Nothing)
            $ BinaryElim Multiplication (Var (Name "x")) (Var (Name "y")))
          (Var (Name "x")))

      -- 4
      result `shouldBe` Just (NumberValue 4)

    it "'and' introduction and elimination" do

      -- let a, b = 2, 3 in
      -- a * b
      result <- LLL.evaluate
        ( AndElim (Bind (Name "a") Nothing) (Bind (Name "b") Nothing)
          (AndIntro (NumberIntro 2) (NumberIntro 3))
        $ BinaryElim Multiplication (Var (Name "a")) (Var (Name "b")))

      -- 6
      result `shouldBe` Just (NumberValue 6)

    it "'or' left-introduction and elimination" do

      -- let a = left 2 in
      -- match a with
      --   | left x -> x
      --   | right y -> 0
      result <- LLL.evaluate
        ( Let (Bind (Name "a") Nothing) (OrIntro False (NumberIntro 2))
        $ OrElim (Var (Name "a"))
          (Bind (Name "x") Nothing) (Var (Name "x"))
          (Bind (Name "y") Nothing) (NumberIntro 0))

      -- 2
      result `shouldBe` Just (NumberValue 2)

    it "'or' right-introduction and elimination" do

      -- let a = right 3 in
      -- match a with
      --   | left x -> 0
      --   | right y -> y
      result <- LLL.evaluate
        ( Let (Bind (Name "a") Nothing) (OrIntro True (NumberIntro 3))
        $ OrElim (Var (Name "a"))
          (Bind (Name "x") Nothing) (NumberIntro 0)
          (Bind (Name "y") Nothing) (Var (Name "y")))

      -- 3
      result `shouldBe` Just (NumberValue 3)

    it "'with' introduction and left-elimination" do

      -- let a = true & 3 in
      -- fst a
      result <- LLL.evaluate
        ( Let (Bind (Name "a") Nothing) (WithIntro (BoolIntro True) (NumberIntro 3))
        $ WithElim False (Var (Name "a")))

      -- true
      result `shouldBe` Just (BoolValue True)

    it "'with' introduction and right-elimination" do

      -- let a = true & 3 in
      -- snd a
      result <- LLL.evaluate
        ( Let (Bind (Name "a") Nothing) (WithIntro (BoolIntro True) (NumberIntro 3))
        $ WithElim True (Var (Name "a")))

      -- 3
      result `shouldBe` Just (NumberValue 3)

  describe "effects" do

    it "emits traces" do

      trace <- newChan

      -- trace 42
      result <- evaluateChan trace
        (Trace (NumberIntro 42))

      -- ()
      result `shouldBe` Just UnitValue

      -- 42
      output <- collectOutput trace
      output `shouldBe` [NumberValue 42]

    it "sequences effects" do

      trace <- newChan

      -- trace 2; trace 3; 5
      result <- evaluateChan trace
        ( Sequence (Trace (NumberIntro 2))
        $ Sequence (Trace (NumberIntro 3))
        $ NumberIntro 5)

      -- 5
      result `shouldBe` Just (NumberValue 5)

      -- 2, 3
      output <- collectOutput trace
      output `shouldBe` [NumberValue 2, NumberValue 3]

    it "evaluates 'fst' lazily" do

      trace <- newChan

      -- fst ((trace 2; 3) & (trace 5; false))
      result <- evaluateChan trace
        (WithElim False
        $ WithIntro
          (Sequence (Trace (NumberIntro 2)) (NumberIntro 3))
          (Sequence (Trace (NumberIntro 5)) (BoolIntro False)))

      -- 3
      result `shouldBe` Just (NumberValue 3)

      -- 2
      output <- collectOutput trace
      output `shouldBe` [NumberValue 2]

    it "evaluates 'snd' lazily" do

      trace <- newChan

      -- snd ((trace 2; 3) & (trace 5; false))
      result <- evaluateChan trace
        (WithElim True
        $ WithIntro
          (Sequence (Trace (NumberIntro 2)) (NumberIntro 3))
          (Sequence (Trace (NumberIntro 5)) (BoolIntro False)))

      -- 3
      result `shouldBe` Just (BoolValue False)

      -- 5
      output <- collectOutput trace
      output `shouldBe` [NumberValue 5]

type TestResult = Either SomeException (Maybe Int)

type ExceptionTestResult = Either SomeException TestResult
