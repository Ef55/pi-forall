module PiForall.AutoEnv.Environment where

import AutoEnv
  ( Ctx,
    Fin,
    LocalName,
    N0,
    Nat (S, Z),
    Refinement,
    SNat,
    Shiftable (..),
    Subst (applyE),
    applyEnv,
    emptyC,
    emptyR,
    weakenE',
    withSNat,
    (+++),
    type (+), SubstVar,
  )
import AutoEnv qualified
import AutoEnv.Bind.Local qualified as Local
import AutoEnv.Bind.Pat qualified as Pat
import AutoEnv.DependentScope qualified as Scope
import AutoEnv.Env (Shiftable, fromRefinement)
import AutoEnv.MonadScoped qualified as SimpleScope
import Control.Monad.Except
  ( ExceptT,
    MonadError (..),
    runExceptT,
  )
import Control.Monad.IO.Class
import Control.Monad.Reader
  ( MonadReader (local),
    ReaderT (..),
    ask,
    asks,
  )
import Control.Monad.Writer (MonadWriter (..), Writer, runWriter)
import Data.Fin qualified as Fin
import Data.SNat qualified as Nat
import Data.Foldable (Foldable (..), toList)
import Data.List
import Data.List qualified as List
import Data.Maybe (listToMaybe)
import Data.Vec qualified as Vec
import PiForall.Log
import PiForall.AutoEnv.ScopeCheck qualified as ScopeCheck
import PiForall.AutoEnv.Syntax
import PiForall.PrettyPrint
import Prettyprinter (Doc, nest, pretty, sep, vcat, (<+>))
import Prettyprinter qualified as PP
import Text.ParserCombinators.Parsec.Pos (SourcePos)

-------------------------------------------------------

-- * environment and type checking monad

-------------------------------------------------------

-- | Environment manipulation and accessing functions
data TcEnv n = TcEnv
  { -- | Datatype definitions, top-level declarations and definitions
    globals :: [ModuleEntry],
    -- | Type declarations: it's not safe to
    -- put these in the context until a corresponding term
    -- has been checked.
    hints :: [(GlobalName, Typ Z)],
    -- | what part of the file we are in (for errors/warnings)
    sourceLocation :: [SourceLocation],
    refinement :: Refinement Term n
  }

instance Shiftable TcEnv where
  shift k (TcEnv globals hints sourceLocation refinement) = TcEnv globals hints sourceLocation (shift k refinement)

type Scope t n = Scope.Scope LocalName t n Z

type MonadScoped t = Scope.MonadScoped LocalName t TcEnv

-- | The type checking Monad includes error (for error reporting) and IO
-- (for warning messages).
-- The Environment contains global declarations and definitions.
newtype TcMonad t n a = TcMonad (Scope.ScopedReaderT LocalName t TcEnv (ExceptT Err (Writer [Log])) n a)
  deriving (Functor, Applicative, Monad, MonadError Err, MonadWriter [Log])

-- Find how to make it derivable
instance (SubstVar t) => Scope.MonadScoped LocalName t TcEnv (TcMonad t) where
  scope' = TcMonad Scope.scope'
  pushEnv ext (TcMonad m) = TcMonad $ Scope.pushEnv ext m
  blob = TcMonad Scope.blob
  local f (TcMonad m) = TcMonad $ Scope.local f m

mapScope :: (forall n. s n -> s' n) -> TcMonad s' n a -> TcMonad s n a
mapScope f (TcMonad m) = TcMonad $ Scope.mapScope id f m

-- | Entry point for the type checking monad, given an
-- initial environment, returns either an error message
-- or some result.
runTcMonad :: TcMonad t Z a -> (Either Err a, [Log])
runTcMonad (TcMonad m) = runWriter (runExceptT (Scope.runScopedReaderT m (Nat.SZ, Scope.empty, emptyEnv)))

-- \| the current scope of local variables
-- env_scope :: Scope LocalName n,
-- \| current refinement for variables in scope
-- env_refinement :: Refinement Term n

-- instance MonadScoped LocalName TcMonad where
--   scope = asks env_scope

--   pushVec :: (SNatI p) => Vec p LocalName -> TcMonad (p + n) a -> TcMonad n a
--   pushVec pat (TcMonad m) =
--     TcMonad
--       ( ReaderT $ \env ->
--           runReaderT
--             m
--             TcEnv
--               { globals = globals env,
--                 hints = hints env,
--                 sourceLocation = sourceLocation env,
--                 env_scope = extendScope pat (env_scope env),
--                 env_refinement = shiftRefinement (size pat) (env_refinement env)
--               }
--       )

-- | Initial environment
emptyEnv :: TcEnv n
emptyEnv =
  TcEnv
    { globals = prelude,
      hints = [],
      sourceLocation = [],
      -- env_scope = emptyScope,
      refinement = emptyR
    }

--------------------------------------------------------------------
-- Globals

-- | Find a name's user supplied type signature
lookupHint :: (MonadScoped t m) => GlobalName -> m n (Maybe (Typ n))
lookupHint v = do
  hints <- hints <$> Scope.blob
  return $ listToMaybe [weakenClosed ty | (x, ty) <- hints, v == x]

lookupGlobalTy :: (SubstVar t) => GlobalName -> TcMonad t n (Typ n)
lookupGlobalTy v = do
  env <- Scope.blob
  case [a | ModuleDecl v' a <- globals env, v == v'] of
    [a] -> return (weakenClosed a)
    _ -> do
      mty <- lookupHint v
      case mty of
        Just ty -> return ty
        Nothing -> err [DS $ "The variable " ++ show v ++ " was not found"]

lookupGlobalDef :: (SubstVar t) => GlobalName -> TcMonad t n (Term n)
lookupGlobalDef v = do
  env <- Scope.blob
  case [a | ModuleDef v' a <- globals env, v == v'] of
    [a] -> return (weakenClosed a)
    _ ->
      err
        [ DS ("The variable " ++ show v ++ " was not found"),
          DS "(out of scope)"
        ]

-- | Find the datatype declaration of a type constructor in the context
lookupTCon :: (SubstVar t) => TyConName -> TcMonad t n DataDef
lookupTCon v = do
  g <- globals <$> Scope.blob
  scanGamma g
  where
    scanGamma [] = do
      currentEnv <- globals <$> Scope.blob
      err $
        [ DS "The type constructor",
          DC v,
          DS "was not found.",
          DS "The current environment is"
        ]
          <> (DZ <$> currentEnv)
    scanGamma ((ModuleData n d) : g) =
      if n == v
        then return d
        else scanGamma g
    scanGamma (_ : g) = scanGamma g

-- | Find a data constructor in the context, returns a list of
-- all potential matches
lookupDConAll ::
  (SubstVar t) =>
  DataConName ->
  TcMonad t n [(TyConName, ScopedConstructorDef)]
lookupDConAll v = do
  g <- globals <$> Scope.blob
  scanGamma g
  where
    scanGamma [] = return []
    scanGamma ((ModuleData tn (DataDef delta _ cs)) : g) =
      case find (\(ConstructorDef v'' _) -> v'' == v) cs of
        Nothing -> scanGamma g
        Just c -> do
          more <- scanGamma g
          return $
            ( tn,
              ScopedConstructorDef delta c
            )
              : more
    scanGamma (_ : g) = scanGamma g

-- | Given the name of a data constructor and the type that it should
-- construct, find the telescopes for its parameters and arguments.
-- Throws an error if the data constructor cannot be found for that type.
lookupDCon ::
  (SubstVar t) =>
  DataConName ->
  TyConName ->
  TcMonad t n ScopedConstructorDef
lookupDCon c tname = do
  matches <- lookupDConAll c
  case lookup tname matches of
    Just scd -> return scd
    Nothing ->
      err
        ( [ DS "Cannot find data constructor",
            DS c,
            DS "for type",
            DC tname,
            DS "Potential matches were:"
          ]
            ++ map (DC . fst) matches
        )

--------------------------------------------------------------------

-- | A local context is an environment that binds n variables (and may also
-- include local definitions).
type Context a = Ctx Term a

weakenDef :: SNat n -> (Fin p, Term p) -> (Fin (n + p), Term (n + p))
weakenDef m (x, y) = (Fin.weakenFin m x, applyE @Term (weakenE' m) y)

emptyContext :: Context N0
emptyContext = emptyC

{-
getLocalCtx :: forall n. SNatI n => TcMonad (Ctx Term n)
getLocalCtx = do
  c <- asks ctx
  case c of
    Context _ (t :: Ctx Term p) _ ->
         case testEquality @_ @n snat (snat :: SNat p) of
           Just Refl -> return t
           Nothing -> err [DS "invalid scope"]

getLocalDefs :: forall n. SNatI n => TcMonad [(Fin n, Term n)]
getLocalDefs = do
  c <- asks ctx
  case c of
    Context _ _ (t :: [(Fin p, Term p)]) ->
         case testEquality @_ @n snat (snat :: SNat p) of
           Just Refl -> return t
           Nothing -> err [DS "invalid scope"]
-}

{-
lookupDef :: Fin m -> Context m n -> TcMonad (Maybe (Term n))
lookupDef x (Context _ _ gamma) = go gamma
    where
      go [] = return Nothing
      go ((y,u):t) = if x == y then return (Just u) else go t
-}

{-
-- | Extend with new definitions
extendDecls :: [(Fin n, Term n)] -> Context m n -> Context m n
extendDecls d c@(Context gamma defs) = Context n gamma (d ++ defs)
-}

-- | Find the type of a local variable in the context
-- This cannot fail
lookupTy :: (MonadScoped Term m) => Fin n -> m n (Term n)
lookupTy x = snd <$> Scope.fromScope x

-- | Extend the context with a new declaration
extendTy :: Typ n -> Context n -> Context (S n)
extendTy d gamma = gamma +++ d

extendDef :: Fin n -> Term n -> Context n -> Context n
extendDef d = error "TODO: local definitions unsupported"

extendLocal :: Local p n -> (Context (p + n) -> m a) -> (Context n -> m a)
extendLocal (LocalDecl x t) k ctx = k (extendTy t ctx)
extendLocal (LocalDef x u) k ctx = error "TODO: local definitions unsupported"

getRefinement :: (SubstVar t) => TcMonad t n (Refinement Term n)
getRefinement = refinement <$> Scope.blob

refine :: (SubstVar t) => Term n -> TcMonad t n (Term n)
refine t = do
  r <- getRefinement
  Scope.withScopeSize $ return $ AutoEnv.refine r t

--------------------------------------------------------------------
-- Source locations

-- | Marked locations in the source code
data SourceLocation where
  SourceLocation :: forall n u s. (ScopeCheck.Scoping n u s, Display u) => SourcePos -> s -> (SNat n, Scope Term n) -> SourceLocation

-- | Push a new source position on the location stack.
extendSourceLocation :: (ScopeCheck.Scoping n u s, Display u) => SourcePos -> s -> TcMonad Term n a -> TcMonad Term n a
extendSourceLocation p t m = do
  s <- Scope.scope'
  Scope.local
    ( \e@TcEnv {sourceLocation = locs} ->
        e {sourceLocation = SourceLocation p t s : locs}
    )
    m

-- | access current source location
getSourceLocation :: (MonadScoped t m) => m n [SourceLocation]
getSourceLocation = sourceLocation <$> Scope.blob

--------------------------------------------------------------------
-- Errors

-- | An error that should be reported to the user
data Err = Err [SourceLocation] (Doc ())

instance Semigroup Err where
  (<>) :: Err -> Err -> Err
  (Err src1 d1) <> (Err src2 d2) = Err (src1 ++ src2) (d1 `mappend` d2)

instance Monoid Err where
  mempty :: Err
  mempty = Err [] mempty

-- | Scoped error message quoting
data D n
  = DS String
  | DD (Doc ())
  | -- An unscoped value
    forall a. (Display a) => DC a
  | -- A value in the current scope
    forall u s. (ScopeCheck.Scoping n u s, Display u) => DU s
  | -- A value in the empty scope
    forall u s. (ScopeCheck.Scoping Z u s, Display u) => DZ s
  | DR (Refinement Term n)

scopedDisplay :: (ScopeCheck.Scoping n u s, Display u) => s -> (SNat n, Scope o n) -> Doc ()
scopedDisplay t (n, Scope.Scope sc _) =
  withSNat n $
    let -- sc = Scope.projectScope s
        t' = withSNat (Vec.vlength sc) ScopeCheck.unscopeUnder sc t
     in disp t'

sdisplay :: (SubstVar t) => D n -> DispInfo -> TcMonad t n (Doc ())
sdisplay (DS s) di = return $ PP.pretty s
sdisplay (DD d) di = return d
sdisplay (DC c) di = return $ disp c
sdisplay (DU s) di = scopedDisplay s <$> Scope.scope'
sdisplay (DZ s) di = return $ scopedDisplay s (Nat.SZ, Scope.empty)
sdisplay (DR r) di = do
  ss <- Scope.scopeSize
  s <- Scope.uscope <$> Scope.scope
  let r' = withSNat ss $ fromRefinement r
  let t' = toList $ withSNat ss $ ScopeCheck.unscopeUnder s (ss, r')
  let c :: [Doc ()] = (\(i, t) -> disp i <+> PP.pretty "|-->" <+> disp t) <$> zip (toList s) t'
  return $ PP.vcat c

displayContext :: TcMonad Term n (D n)
displayContext = do
  ss <- Scope.scopeSize
  s <- Scope.scope
  let t' = toList $ ScopeCheck.unscopeUnder Vec.empty (ss, s)
  let c :: [Doc ()] = (\(i, t) -> disp i <+> PP.pretty ":" <+> disp t) <$> t'
  return $ DD $ PP.vcat c

displayRefinement :: TcMonad Term n (D n)
displayRefinement = DR <$> getRefinement

-- | display an error
-- TODO: preserve passed in di for printing the term???
displayErr :: Err -> DispInfo -> Doc ()
displayErr (Err [] msg) di = msg
displayErr (Err ((SourceLocation p term s) : _) msg) di =
  display p di
    <+> nest 2 msg
    <+> nest
      2
      ( pretty "\nin the expression"
          <+> nest 2 (scopedDisplay term s)
      )

-- | Print a warning
warn :: (SubstVar t) => [D n] -> TcMonad t n ()
warn d = do
  loc <- getSourceLocation
  msg <- mapM (`sdisplay` initDI) d
  tell $ List.singleton $ Warn $ show $ vcat msg

-- | Print an error, making sure that the scope lines up
err :: (SubstVar t) => [D n] -> TcMonad t n b
err d = do
  loc <- getSourceLocation
  msg <- mapM (`sdisplay` initDI) d
  throwError $ Err loc (sep msg)

-- | Augment an error message with addition information (if thrown)
extendErr :: (SubstVar t) => TcMonad t n a -> [D n] -> TcMonad t n a
extendErr ma d =
  ma `catchError` \(Err ps msg) -> do
    msg' <- mapM (`sdisplay` initDI) d
    throwError $ Err ps (vcat [msg, sep msg'])

whenNothing :: (SubstVar t) => Maybe a -> [D n] -> TcMonad t n a
whenNothing x msg =
  case x of
    Just r -> return r
    Nothing -> err msg

ensureError :: TcMonad t n a -> TcMonad t n (Either Err a)
ensureError c = (Right <$> c) `catchError` \err -> return $ Left err

--------------------------------------------------------------
-- Modules

-- | Add a type hint
extendHints :: (MonadScoped t m) => (GlobalName, Typ Z) -> m n a -> m n a
extendHints h = Scope.local (\m@TcEnv {hints = hs} -> m {hints = h : hs})

-- | Extend the global environment with a new entry
extendCtx :: (MonadScoped t m) => ModuleEntry -> m n a -> m n a
extendCtx d =
  Scope.local (\m@TcEnv {globals = cs} -> m {globals = d : cs})

-- | Extend the context with a list of global bindings
extendCtxs :: (MonadScoped t m) => [ModuleEntry] -> m n a -> m n a
extendCtxs ds =
  Scope.local (\m@TcEnv {globals = cs} -> m {globals = ds ++ cs})

-- | Extend the context with a module
-- Note we must reverse the order
extendCtxMod :: (MonadScoped t m) => Module -> m n a -> m n a
extendCtxMod m = extendCtxs (reverse $ moduleEntries m)

-- | Extend the context with a list of modules
extendCtxMods :: (MonadScoped t m) => [Module] -> m n a -> m n a
extendCtxMods mods k = foldr extendCtxMod k mods