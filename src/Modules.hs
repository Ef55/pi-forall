{- pi-forall language -}

-- | Tools for working with multiple source files
module Modules (getModules, ModuleInfo (..)) where

import ConcreteSyntax qualified as C
import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Lazy
import Data.Graph qualified as Gr
import Data.List (nub, (\\))
import Environment
import Parser
import PrettyPrint
import ScopeCheck
import Syntax
import System.Directory
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath
import Text.ParserCombinators.Parsec.Error (ParseError, errorPos)
import TypeCheck

-- import System.FilePath (splitFileName)

-- | getModules starts with a top-level module, and gathers all of the module's
-- transitive dependency. It returns the list of parsed modules, with all
-- modules appearing after its dependencies.
getModules ::
  (Functor m, MonadError ParseError m, MonadIO m) =>
  [FilePath] ->
  String ->
  m [Module]
getModules prefixes top = do
  toParse <- gatherModules prefixes [ModuleImport top]
  flip evalStateT initialConstructorNames $ mapM reparse toParse

data ModuleInfo = ModuleInfo
  { modInfoName :: ModuleName,
    modInfoFilename :: String,
    modInfoImports :: [ModuleImport]
  }

-- | Build the module dependency graph.
--   This only parses the imports part of each file; later we go back and parse all of it.
gatherModules ::
  (Functor m, MonadError ParseError m, MonadIO m) =>
  [FilePath] ->
  [ModuleImport] ->
  m [ModuleInfo]
gatherModules prefixes ms = gatherModules' ms []
  where
    gatherModules' [] accum = return $ topSort accum
    gatherModules' ((ModuleImport m) : ms') accum = do
      modFileName <- getModuleFileName prefixes m
      cmod <- parseModuleImports modFileName
      let imports = C.moduleImports cmod
      let accum' = ModuleInfo m modFileName imports : accum
      let oldMods = map (ModuleImport . modInfoName) accum'
      gatherModules' (nub (ms' ++ imports) \\ oldMods) accum'

-- | Generate a sorted list of modules, with the postcondition that a module
-- will appear _after_ any of its dependencies.
topSort :: [ModuleInfo] -> [ModuleInfo]
topSort ms = reverse sorted
  where
    (gr, lu) =
      Gr.graphFromEdges'
        [ (m, modInfoName m, [i | ModuleImport i <- modInfoImports m])
          | m <- ms
        ]
    lu' v = let (m, _, _) = lu v in m
    sorted = [lu' v | v <- Gr.topSort gr]

-- | Find the file associated with a module.
getModuleFileName ::
  (MonadIO m) =>
  [FilePath] ->
  ModuleName ->
  m FilePath
getModuleFileName prefixes modul = do
  let makeFileName prefix = prefix </> mDotPi
      -- get M.pi from M or M.pi
      mDotPi =
        if takeExtension s == ".pi"
          then s
          else s <.> "pi"
      s = modul
      possibleFiles = map makeFileName prefixes
  files <- liftIO $ filterM doesFileExist possibleFiles
  if null files
    then
      error $
        "Can't locate module: "
          ++ show modul
          ++ "\nTried: "
          ++ show possibleFiles
    else return $ head files

-- | Fully parse a module (not just the imports).
reparse ::
  (MonadError ParseError m, MonadIO m, MonadState ConstructorNames m) =>
  ModuleInfo ->
  m Module
reparse (ModuleInfo _ fileName _) = do
  cnames <- get
  cmodu <- parseModuleFile cnames fileName
  put (C.moduleConstructors cmodu)
  case scopeCheckModule cmodu of
    Just m -> return m
    Nothing -> error $ "scope checking failed"

exitWith :: Either a b -> (a -> IO ()) -> IO b
exitWith res f =
  case res of
    Left x -> f x >> exitFailure
    Right y -> return y

-- | Type check the given string in the empty environment
go :: String -> IO ()
go str = do
  case parseExpr str of
    Left parseError -> putParseError parseError
    Right term -> do
      case scopeCheck term of
        Nothing -> putStrLn "scope check failed"
        Just term' -> do
          putStrLn $ "parsed and scope checked as"
          putStrLn $ pp term'
          let (res, logs) = runTcMonad (inferType term' emptyContext)
          mapM_ (putStr . show) logs
          case res of
            Left typeError -> putTypeError (displayErr typeError initDI)
            Right ty -> do
              putStrLn "typed with type"
              putStrLn $ pp ty

-- | Display a parse error to the user
putParseError :: ParseError -> IO ()
putParseError parseError = do
  putStrLn $ pp $ errorPos parseError
  print parseError

-- | Display a type error to the user
putTypeError :: Doc () -> IO ()
putTypeError doc = do
  putStrLn "Type Error:"
  print doc

-- | Type check the given file
goFilename :: String -> IO ()
goFilename pathToMainFile = do
  let prefixes = [currentDir, mainFilePrefix]
      (mainFilePrefix, name) = splitFileName pathToMainFile
      currentDir = ""
  putStrLn $ "processing " ++ name ++ "..."
  v <- runExceptT (getModules prefixes name)
  val <- v `exitWith` putParseError
  putStrLn "type checking..."
  let (d, logs) = runTcMonad (tcModules val)
  mapM_ (putStr . show) logs
  defs <- d `exitWith` (putTypeError . flip displayErr initDI)
  putStrLn $ pp (last defs)

-- | 'pi <filename>' invokes the type checker on the given
-- file and either prints the types of all definitions in the module
-- or prints an error message.
main :: IO ()
main = do
  [pathToMainFile] <- getArgs
  goFilename pathToMainFile
  exitSuccess