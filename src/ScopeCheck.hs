{-# LANGUAGE FunctionalDependencies #-}

-- |
-- Module      : ScopeCheck
-- Description : Scope checking the Untyped lambda calculus
-- Stability   : experimental
--
-- This module demonstrates a translation from unscoped to well-scoped terms
module ScopeCheck (Scoping (..), scope, unscope) where

import AutoEnv.Bind.Local qualified as L
import AutoEnv.Bind.Pat (PatList (..))
import AutoEnv.Bind.Pat qualified as Pat
import AutoEnv.Bind.Scoped ((<:>))
import AutoEnv.Bind.Scoped qualified as Scoped
import AutoEnv.Bind.Single qualified as B
import AutoEnv.Lib
import AutoEnv.MonadScoped (ScopedReader)
import AutoEnv.MonadScoped qualified as Scoped
import ConcreteSyntax qualified as C
import Control.Monad (foldM)
import Control.Monad.Reader (MonadReader (ask), Reader, asks, runReader)
import Data.Maybe (fromJust)
import Data.Vec qualified as Vec
import Syntax qualified as S

--------------------------------------------------------------------------------
--- Types which are parametrized by something other than their scope do not fall
--- in the API, so we have to handle them separately
--------------------------------------------------------------------------------

data ScopedPattern n
  = forall p.
    (SNatI p) =>
    ScopedPattern (S.Pattern p) [(LocalName, Fin (p + n))]

data ScopedPatList n
  = forall p.
    (SNatI p) =>
    ScopedPatList (Pat.PatList S.Pattern p) [(LocalName, Fin (p + n))]

data ScopedTele n
  = forall p. (SNatI p) => ScopedTele [(LocalName, Fin (p + n))] (S.Telescope p n)

scopeCheckTele :: forall n. (SNatI n) => [(LocalName, Fin n)] -> C.Telescope -> Maybe (ScopedTele n)
scopeCheckTele scope [] = Just $ ScopedTele scope Scoped.TNil
scopeCheckTele scope (C.EntryDecl n ty : entries) = do
  ty' <- scope' scope ty
  let scope' :: [(LocalName, Fin (S n))]
      scope' = push n scope
  ScopedTele
    (ss :: [(LocalName, Fin (p + 'S n))])
    (tele' :: S.Telescope p (S n)) <-
    scopeCheckTele scope' entries
  let fact :: p + S n :~: (p + N1) + n
      fact = axiomAssoc @p @N1 @n
  withSNat (sPlus (snat @p) s1) $ case fact of
    Refl -> do
      let ret = S.LocalDecl n ty' <:> tele'
      return $ ScopedTele ss ret
scopeCheckTele scope (C.EntryDef n tm : entries) = do
  tm' <- scope' scope tm
  ScopedTele ss (tele' :: S.Telescope p n) <- scopeCheckTele scope entries
  case axiomPlusZ @p of
    Refl -> do
      ln <- lookup n scope
      let ret = S.LocalDef ln tm' <:> tele'
      return $ ScopedTele ss ret

toP ::
  (SNatI n) =>
  [(LocalName, Fin n)] ->
  C.Pattern ->
  Maybe (ScopedPattern n)
toP vs (C.PatVar x) =
  return (ScopedPattern (S.PatVar x) ((x, FZ) : map (fmap FS) vs))
toP vs (C.PatCon n pats) = do
  ScopedPatList pats' vs' <- toPL vs pats
  return (ScopedPattern (S.PatCon n pats') vs')

toPL ::
  forall n.
  (SNatI n) =>
  [(LocalName, Fin n)] ->
  [C.Pattern] ->
  Maybe (ScopedPatList n)
toPL vs [] = return $ ScopedPatList Pat.PNil vs
toPL vs (p : ps) = do
  ScopedPattern (p' :: S.Pattern p) vs' <- toP vs p
  withSNat (sPlus (snat :: SNat p) (snat :: SNat n)) $ do
    ScopedPatList (ps' :: Pat.PatList S.Pattern p1) vs'' <-
      toPL vs' ps
    Refl <- Just (axiomAssoc @p1 @p @n)
    withSNat (sPlus (snat :: SNat p1) (snat :: SNat p)) (return $ ScopedPatList (Pat.PCons p' ps') vs'')

unscopeLocal :: S.Local p n -> Unscope n C.Entry
unscopeLocal (S.LocalDecl n t) = C.EntryDecl n <$> unscope' t
unscopeLocal (S.LocalDef n t) = C.EntryDef <$> unscope' (Local n) <*> unscope' t

unscopeTelescope :: S.Telescope p n -> Unscope n [C.Entry]
unscopeTelescope Scoped.TNil = return []
unscopeTelescope (Scoped.TCons h t) =
  (:) <$> unscopeLocal h <*> Scoped.push h (unscopeTelescope t)

unscopePatList :: Pat.PatList S.Pattern p -> [C.Pattern]
unscopePatList Pat.PNil = []
unscopePatList (Pat.PCons pat t) = unscopePattern pat : unscopePatList t

unscopePattern :: S.Pattern p -> C.Pattern
unscopePattern (S.PatCon name pats) = C.PatCon name $ unscopePatList pats
unscopePattern (S.PatVar n) = C.PatVar n

--------------------------------------------------------------------------------
--- Scoping interface
--------------------------------------------------------------------------------

type Unscope n a = ScopedReader LocalName n a

class Scoping n u s | n u -> s, s -> u, s -> n where
  scope' :: (SNatI n) => [(LocalName, Fin n)] -> u -> Maybe s
  unscope' :: s -> Unscope n u

scope :: (Scoping Z u s) => u -> Maybe s
scope = scope' []

unscope :: (SNatI n, Scoping n u s) => Vec n LocalName -> s -> u
unscope v t = Scoped.runScopedReader v (unscope' t)

--------------------------------------------------------------------------------
--- Scoping instances
--------------------------------------------------------------------------------

data ScopedName n = Local (Fin n) | Global String
toTerm :: ScopedName n -> S.Term n
toTerm (Local n) = S.Var n
toTerm (Global n) = S.Global n

push :: a -> [(a, Fin n)] -> [(a, Fin (S n))]
push x vs = (x, FZ) : map (fmap FS) vs

instance Scoping n LocalName (ScopedName n) where
  scope' c v = case lookup v c of
    Just x -> Just $ Local x
    Nothing -> Just $ Global (name v)

  unscope' (Local n) = do
    bnds <- Scoped.scope
    return $ Scoped.scope_names bnds Vec.! n
  unscope' (Global n) = return $ LocalName n

instance Scoping n C.Match (S.Match n) where
  scope' vs (C.Branch pat tm) = do
    ScopedPattern (pat' :: S.Pattern p) vs' <- toP vs pat
    tm' <- withSNat (sPlus (snat :: SNat p) (snat :: SNat n)) $ scope' vs' tm
    return (S.Branch (Pat.bind pat' tm'))

  unscope' :: S.Match n -> Unscope n C.Match
  unscope' (S.Branch bnd) = do
    (pat, t) <- Scoped.withSize $ return $ Pat.unbindl bnd
    C.Branch (unscopePattern pat) <$> Scoped.push pat (unscope' t)

instance Scoping n C.Term (S.Term n) where
  scope' :: (SNatI n) => [(LocalName, Fin n)] -> C.Term -> Maybe (S.Term n)
  scope' vs C.TyType = return S.TyType
  scope' vs (C.Var v) = toTerm <$> scope' vs v
  scope' vs (C.Global x) = return (S.Global x)
  scope' vs (C.Pi a x b) = do
    a' <- scope' vs a
    b' <- scope' ((x, FZ) : map (fmap FS) vs) b
    return (S.Pi a' (L.bind x b'))
  scope' vs (C.Pos s a) = do
    a' <- scope' vs a
    return (S.Pos s a')
  scope' vs (C.Let x a b) = do
    a' <- scope' vs a
    b' <- scope' ((x, FZ) : map (fmap FS) vs) b
    return (S.Let a' (L.bind x b'))
  scope' vs (C.Lam v b) = do
    b' <- scope' ((v, FZ) : map (fmap FS) vs) b
    return $ S.Lam (L.bind v b')
  scope' vs (C.App f a) = do
    f' <- scope' vs f
    a' <- scope' vs a
    return $ S.App f' a'
  scope' vs (C.TyCon n tys) = do
    tys' <- mapM (scope' vs) tys
    return $ S.TyCon n tys'
  scope' vs (C.DataCon n args) = do
    args' <- mapM (scope' vs) args
    return $ S.DataCon n args'
  scope' vs (C.Case a brs) = do
    a' <- scope' vs a
    brs' <- mapM (scope' vs) brs
    return $ S.Case a' brs'
  scope' vs (C.Ann a b) = do
    a' <- scope' vs a
    b' <- scope' vs b
    return $ S.Ann a' b'
  scope' vs (C.TyEq a b) = do
    a' <- scope' vs a
    b' <- scope' vs b
    return $ S.TyEq a' b'
  scope' vs C.TmRefl = return S.TmRefl
  scope' vs (C.Subst a b) = do
    a' <- scope' vs a
    b' <- scope' vs b
    return $ S.Subst a' b'
  scope' vs (C.Contra a) = do
    a' <- scope' vs a
    return $ S.Contra a'
  scope' vs C.TrustMe = return S.TrustMe
  scope' vs C.PrintMe = return S.PrintMe

  unscope' :: S.Term n -> ScopedReader LocalName n C.Term
  unscope' S.TyType = pure C.TyType
  unscope' (S.Lam bnd) = do
    let (x, t) = L.unbindl bnd
    C.Lam x <$> Scoped.push x (unscope' t)
  unscope' (S.Var x) = C.Var <$> unscope' (Local x)
  unscope' (S.Global n) = return $ C.Global n
  unscope' (S.Pi ty bnd) = do
    ty' <- unscope' ty
    let (x, t) = L.unbindl bnd
    t' <- Scoped.push x $ unscope' t
    return $ C.Pi ty' x t'
  unscope' (S.Pos pos t) = C.Pos pos <$> unscope' t
  unscope' (S.Let t1 bnd) = do
    t1' <- unscope' t1
    let (x, t2) = L.unbindl bnd
    t2' <- Scoped.push x $ unscope' t2
    return $ C.Let x t1' t2'
  unscope' (S.TyCon name args) = C.TyCon name <$> mapM unscope' args
  unscope' (S.DataCon name args) = C.DataCon name <$> mapM unscope' args
  unscope' (S.Case s matches) = C.Case <$> unscope' s <*> mapM unscope' matches
  unscope' (S.App t1 t2) = C.App <$> unscope' t1 <*> unscope' t2
  unscope' (S.Ann t1 t2) = C.Ann <$> unscope' t1 <*> unscope' t2
  unscope' (S.TyEq t1 t2) = C.TyEq <$> unscope' t1 <*> unscope' t2
  unscope' S.TmRefl = return C.TmRefl
  unscope' (S.Subst t1 t2) = C.Subst <$> unscope' t1 <*> unscope' t2
  unscope' (S.Contra t) = C.Contra <$> unscope' t
  unscope' S.TrustMe = return C.TrustMe
  unscope' S.PrintMe = return C.PrintMe

instance Scoping n C.ConstructorDef (S.ConstructorDef n) where
  scope' :: (SNatI n) => [(LocalName, Fin n)] -> C.ConstructorDef -> Maybe (S.ConstructorDef n)
  scope' scope (C.ConstructorDef p dc theta) = do
    ScopedTele _ theta' <- scopeCheckTele scope theta
    pure $ S.ConstructorDef dc theta'

  unscope' :: S.ConstructorDef n -> Unscope n C.ConstructorDef
  unscope' (S.ConstructorDef name theta) = C.ConstructorDef Nothing name <$> unscopeTelescope theta

instance Scoping Z C.DataDef S.DataDef where
  scope' c (C.DataDef delta s cs) = do
    ScopedTele scope (delta' :: S.Telescope n Z) <- scopeCheckTele c delta
    case axiomPlusZ @n of
      Refl -> S.DataDef delta' <$> scope' c s <*> mapM (scope' scope) cs

  unscope' (S.DataDef @n delta sort cstrs) = do
    delta' <- unscopeTelescope delta
    sort' <- unscope' sort
    cstrs' <- case axiomPlusZ @n of Refl -> Scoped.push delta $ mapM unscope' cstrs
    return $ C.DataDef delta' sort' cstrs'

instance Scoping Z C.ModuleEntry S.ModuleEntry where
  scope' c (C.ModuleDecl gn ty) = S.ModuleDecl gn <$> scope' c ty
  scope' c (C.ModuleDef gn tm) = S.ModuleDef gn <$> scope' c tm
  scope' c (C.ModuleData dn datadef) = S.ModuleData dn <$> scope' c datadef
  scope' c (C.ModuleFail failing) = S.ModuleFail <$> scope' c failing

  unscope' :: S.ModuleEntry -> Unscope Z C.ModuleEntry
  unscope' (S.ModuleDecl gn ty) = C.ModuleDecl gn <$> unscope' ty
  unscope' (S.ModuleDef gn tm) = C.ModuleDef gn <$> unscope' tm
  unscope' (S.ModuleData dn dat) = C.ModuleData dn <$> unscope' dat
  unscope' (S.ModuleFail f) = C.ModuleFail <$> unscope' f

instance Scoping Z C.Module S.Module where
  scope' :: [(LocalName, Fin Z)] -> C.Module -> Maybe S.Module
  scope' c m = do
    entries <- mapM (scope' c) (C.moduleEntries m)
    return $
      S.Module
        { S.moduleName = C.moduleName m,
          S.moduleImports = C.moduleImports m,
          S.moduleEntries = entries,
          S.moduleConstructors = C.moduleConstructors m
        }

  unscope' :: S.Module -> Unscope Z C.Module
  unscope' m = do
    entries <- mapM unscope' $ S.moduleEntries m
    return
      C.Module
        { C.moduleName = S.moduleName m,
          C.moduleImports = S.moduleImports m,
          C.moduleEntries = entries,
          C.moduleConstructors = S.moduleConstructors m
        }
