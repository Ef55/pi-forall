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
    (tcFiles "pi" ["Equality", "Lennart", "List", "Vec"])

baseTests :: TestTree
baseTests = testGroup "Base tests" (tcFiles "test/base" ["Fail"])

main :: IO ()
main = do
  defaultMain $
    testGroup
      "All"
      [ QC.testProperty "PP-Parsing round trip" prop_roundtrip,
        examples,
        baseTests
      ]

--------------------------------------------------------------------------------
-- Helpers for tests definition
--------------------------------------------------------------------------------

standardLibrary :: [String]
standardLibrary = ["pi"]

tcFiles :: String -> [String] -> [TestTree]
tcFiles path tests = tcFile [path] <$> tests

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
tcFile :: [String] -> String -> TestTree
tcFile path name = tester (name ++ " [✔]") path name $ \case
  ParsingFailure err -> assertFailure $ "Parsing error:" ++ show err
  TypingFailure err -> assertFailure $ "Type error:" ++ show (displayErr err PP.initDI)
  TestSuccess _ logs@(_ : _) -> assertFailure $ "Warnings were produced:" ++ intercalate "\n" (fmap show logs)
  TestSuccess r [] -> return ()
