{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad.Except
import Data.List (intercalate)
import Data.Maybe (isJust)
import ParseScopeRT as AutoRT
import ParseResolveRT as UnRT
import PiForall.AutoEnv.Environment
import PiForall.AutoEnv.Environment qualified as Env
import PiForall.AutoEnv.Equal qualified as Equal
import PiForall.AutoEnv.Modules
import PiForall.AutoEnv.Syntax
import PiForall.AutoEnv.TypeCheck
import PiForall.Log qualified as Log
import PiForall.PrettyPrint as PP
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck qualified as QC
import Text.ParserCombinators.Parsec.Error

--------------------------------------------------------------------------------
-- Definition of tests to run
--------------------------------------------------------------------------------

std :: TestTree
std =
  testGroup
    "Standard Library"
    ( positiveTests
        "pi/std"
        [ "Equality",
          "List",
          "Option",
          "Vec",
          "HList",
          "Logic"
        ]
    )

examples :: TestTree
examples =
  testGroup
    "Examples"
    ( positiveTests
        "pi/examples"
        [ "Lennart",
          "Hurkens",
          "Lambda",
          "Lambda0",
          "Lambda1",
          "Lambda2",
          "Compiler",
          "Regex",
          "AVL"
        ]
    )

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
      [ QC.testProperty "PP-Parsing-Scope round trip" AutoRT.prop_roundtrip,
        QC.testProperty "PP-Parsing-Resolve round trip" UnRT.prop_roundtrip,
        std,
        examples,
        baseTests,
        bugs
      ]

--------------------------------------------------------------------------------
-- Helpers for tests definition
--------------------------------------------------------------------------------

standardLibrary :: [String]
standardLibrary = ["pi/std"]

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
  TypingFailure err -> assertFailure $ "Type error:\n" ++ show (displayErr err PP.initDI)
  TestSuccess _ logs@(_ : _) -> assertFailure $ "Warnings were produced:" ++ intercalate "\n" (fmap show logs)
  TestSuccess r [] -> return ()
