{-# LANGUAGE LambdaCase #-}

module Main where

import Arbitrary (prop_roundtrip)
import Control.Monad.Except
import Data.List (intercalate)
import Data.Maybe (isJust)
import Environment
import Environment qualified as Env
import Equal qualified
import Log qualified
import Modules
import PrettyPrint as PP
import Syntax
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck qualified as QC
import Text.ParserCombinators.Parsec.Error
import TypeCheck

--------------------------------------------------------------------------------
-- Definition of tests to run
--------------------------------------------------------------------------------

examples :: TestTree
examples =
  testGroup
    "Examples"
    (positiveTests "pi" ["Equality", "Lennart", "List", "Vec"])

baseTests :: TestTree
baseTests = testGroup "Base tests" (negativeTests "test/base" ["Fail", "ConstructorEvidence"])

bugs :: TestTree
bugs =
  testGroup
    "Bugs"
    ( positiveTests
        "test/reproducers"
        [ -- https://github.com/sweirich/autoenv/pull/2
          "Bug1",
          "Bug2"
        ]
    )

main :: IO ()
main = do
  defaultMain $
    testGroup
      "All"
      [ QC.testProperty "PP-Parsing round trip" prop_roundtrip,
        examples,
        baseTests,
        bugs
      ]

--------------------------------------------------------------------------------
-- Helpers for tests definition
--------------------------------------------------------------------------------

standardLibrary :: [String]
standardLibrary = ["pi"]

positiveTests :: String -> [String] -> [TestTree]
positiveTests path tests = tcFile [path] True <$> tests

negativeTests :: String -> [String] -> [TestTree]
negativeTests path tests = tcFile [path] False <$> tests

--------------------------------------------------------------------------------
-- Testing functions
--------------------------------------------------------------------------------

data Result
  = ParsingFailure ParseError
  | TypingFailure Err
  | TestSuccess [Module] [Log.Log]

tester :: String -> [String] -> String -> (Result -> Assertion) -> TestTree
tester testName path fileName k = testCase testName $ do
  v <- runExceptT (getModules (path ++ standardLibrary) fileName)
  case v of
    Left b -> k $ ParsingFailure b
    Right val -> case runTcMonad (tcModules val) of
      (Left err, _) -> k $ TypingFailure err
      (Right res, logs) -> k $ TestSuccess res (filter (not . Log.isInfo) logs)

-- | Type check the given file
tcFile :: [String] -> Bool -> String -> TestTree
tcFile path positive name = tester (name ++ if positive then " [✔]" else " [✘]") path name $ \case
  ParsingFailure err -> assertFailure $ "Parsing error:" ++ show err
  TypingFailure err -> assertFailure $ "Type error:" ++ show (displayErr err PP.initDI)
  TestSuccess _ logs@(_ : _) -> assertFailure $ "Warnings were produced:" ++ intercalate "\n" (fmap show logs)
  TestSuccess r [] -> return ()
