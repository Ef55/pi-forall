{- pi-forall language -}

-- | Tools for working with multiple source files
module PiForall.Unbound.Modules(getModules, ModuleInfo(..)) where

import Control.Monad
import PiForall.Unbound.Syntax

import Text.ParserCombinators.Parsec.Error ( ParseError )
import PiForall.Parser(parseModuleFile, parseModuleImports)
import Control.Monad.Except
import Control.Monad.State.Lazy
import System.FilePath
import System.Directory
import qualified Data.Graph as Gr
import Data.List(nub,(\\))
import PiForall.ConcreteSyntax qualified as C
import PiForall.Unbound.NameResolution qualified as NameResolution


-- | getModules starts with a top-level module, and gathers all of the module's
-- transitive dependency. It returns the list of parsed modules, with all
-- modules appearing after its dependencies.
getModules
  :: (Functor m, MonadError ParseError m, MonadIO m) =>
     [FilePath] -> String -> m [Module]
getModules prefixes top = do
  toParse <- gatherModules prefixes [ModuleImport top]
  flip evalStateT initialConstructorNames $ mapM reparse toParse


data ModuleInfo = ModuleInfo {
                    modInfoName     :: ModuleName,
                    modInfoFilename :: String,
                    modInfoImports  :: [ModuleImport]
                  }

-- | Build the module dependency graph.
--   This only parses the imports part of each file; later we go back and parse all of it.
gatherModules
  :: (Functor m, MonadError ParseError m, MonadIO m) =>
     [FilePath] -> [ModuleImport] -> m [ModuleInfo]
gatherModules prefixes ms = gatherModules' ms [] where
  gatherModules' [] accum = return $ topSort accum
  gatherModules' ((ModuleImport m):ms') accum = do
    modFileName <- getModuleFileName prefixes m
    imports <- C.moduleImports <$> parseModuleImports modFileName
    let accum' = ModuleInfo m modFileName imports :accum
    let oldMods = map (ModuleImport . modInfoName) accum'
    gatherModules' (nub (ms' ++ imports) \\ oldMods) accum'

-- | Generate a sorted list of modules, with the postcondition that a module
-- will appear _after_ any of its dependencies.
topSort :: [ModuleInfo] -> [ModuleInfo]
topSort ms = reverse sorted
  where (gr,lu) = Gr.graphFromEdges' [(m, modInfoName m, [i | ModuleImport i <- modInfoImports m])
                                      | m <- ms]
        lu' v = let (m,_,_) = lu v in m
        sorted = [lu' v | v <- Gr.topSort gr]

-- | Find the file associated with a module.
getModuleFileName :: (MonadIO m)
                  => [FilePath] -> ModuleName -> m FilePath
getModuleFileName prefixes modul = do
  let makeFileName prefix = prefix </> mDotPi
      -- get M.pi from M or M.pi
      mDotPi = if takeExtension s == ".pi"
                    then s
                    else s <.> "pi"
      s = modul
      possibleFiles = map makeFileName prefixes
  files <- liftIO $ filterM doesFileExist possibleFiles
  if null files
     then error $ "Can't locate module: " ++ show modul ++
                "\nTried: " ++ show possibleFiles
     else return $ head files

-- | Fully parse a module (not just the imports).
reparse :: (MonadError ParseError m, MonadIO m, MonadState ConstructorNames m) =>
            ModuleInfo -> m Module
reparse (ModuleInfo _ fileName _) = do
  cnames <- get
  modu <- parseModuleFile cnames fileName
  put (C.moduleConstructors modu)
  case NameResolution.resolve modu of
    Just m -> return m
    Nothing -> error "scope checking failed"

