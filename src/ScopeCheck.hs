{-# LANGUAGE FunctionalDependencies #-}
-- |
-- Module      : ScopeCheck
-- Description : Scope checking the Untyped lambda calculus
-- Stability   : experimental
--
-- This module demonstrates a translation from unscoped to well-scoped terms
module ScopeCheck (scopeCheck, scopeCheckModule, unscope) where

import AutoEnv.Bind.Local qualified as L
import AutoEnv.Bind.Pat (PatList (..))
import AutoEnv.Bind.Pat qualified as Pat
import AutoEnv.Bind.Scoped ((<:>))
import AutoEnv.Bind.Scoped qualified as Scoped
import AutoEnv.Bind.Single qualified as B
import AutoEnv.Lib
import ConcreteSyntax qualified as C
import Control.Monad (foldM)
import Control.Monad.Reader (MonadReader (ask), Reader, asks, runReader)
import Data.Maybe (fromJust)
import Data.Vec qualified as Vec
import Syntax qualified as S
import AutoEnv.MonadScoped (ScopedReader)
import AutoEnv.MonadScoped qualified as Scoped

--------------------------------------------------------------------------------
--- Transform concrete syntax into abstract syntax (aka check the scoping)
--------------------------------------------------------------------------------

push :: a -> [(a, Fin n)] -> [(a, Fin (S n))]
push x vs = (x, FZ) : map (fmap FS) vs

data ScopedPattern n
  = forall p.
    (SNatI p) =>
    ScopedPattern (S.Pattern p) [(LocalName, Fin (p + n))]

data ScopedPatList n
  = forall p.
    (SNatI p) =>
    ScopedPatList (Pat.PatList S.Pattern p) [(LocalName, Fin (p + n))]

scopeCheckModule :: C.Module -> Maybe S.Module
scopeCheckModule m = do
  entries <- mapM scopeCheckEntry (C.moduleEntries m)
  return $
    S.Module
      { S.moduleName = C.moduleName m,
        S.moduleImports = C.moduleImports m,
        S.moduleEntries = entries,
        S.moduleConstructors = C.moduleConstructors m
      }

scopeCheckEntry :: C.ModuleEntry -> Maybe S.ModuleEntry
scopeCheckEntry (C.ModuleDecl gn ty) = S.ModuleDecl gn <$> scopeCheck ty
scopeCheckEntry (C.ModuleDef gn tm) = S.ModuleDef gn <$> scopeCheck tm
scopeCheckEntry (C.ModuleData dn datadef) = S.ModuleData dn <$> scopeCheckData datadef
scopeCheckEntry (C.ModuleFail failing) = S.ModuleFail <$> scopeCheckEntry failing

data ScopedTele n
  = forall p. (SNatI p) => ScopedTele [(LocalName, Fin (p + n))] (S.Telescope p n)

scopeCheckData :: C.DataDef -> Maybe S.DataDef
scopeCheckData (C.DataDef delta s cs) = do
  ScopedTele scope (delta' :: S.Telescope n Z) <- scopeCheckTele [] delta
  case axiomPlusZ @n of
    Refl -> S.DataDef delta' <$> scopeCheck s <*> mapM (scopeCheckConstructor scope) cs

scopeCheckTele :: forall n. (SNatI n) => [(LocalName, Fin n)] -> C.Telescope -> Maybe (ScopedTele n)
scopeCheckTele scope [] = Just $ ScopedTele scope Scoped.TNil
scopeCheckTele scope (C.EntryDecl n ty : entries) = do
  ty' <- to scope ty
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
  tm' <- to scope tm
  ScopedTele ss (tele' :: S.Telescope p n) <- scopeCheckTele scope entries
  case axiomPlusZ @p of
    Refl -> do
      ln <- lookup n scope
      let ret = S.LocalDef ln tm' <:> tele'
      return $ ScopedTele ss ret

scopeCheckConstructor ::
  (SNatI n) =>
  [(LocalName, Fin n)] ->
  C.ConstructorDef ->
  Maybe (S.ConstructorDef n)
scopeCheckConstructor scope (C.ConstructorDef p dc theta) = do
  ScopedTele _ theta' <- scopeCheckTele scope theta
  pure $ S.ConstructorDef dc theta'

-- | Convert a named expression to deBruijn indicies, checking to make
-- sure that the expression is well scoped
scopeCheck :: C.Term -> Maybe (S.Term Z)
scopeCheck = to []

toM ::
  forall n.
  (SNatI n) =>
  [(LocalName, Fin n)] ->
  C.Match ->
  Maybe (S.Match n)
toM vs (C.Branch pat tm) = do
  ScopedPattern (pat' :: S.Pattern p) vs' <- toP vs pat
  tm' <- withSNat (sPlus (snat :: SNat p) (snat :: SNat n)) $ to vs' tm
  return (S.Branch (Pat.bind pat' tm'))

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

to :: (SNatI n) => [(LocalName, Fin n)] -> C.Term -> Maybe (S.Term n)
to vs C.TyType = return S.TyType
to vs (C.Var v) = case lookup v vs of
  Just x -> Just $ S.Var x
  Nothing -> Just $ S.Global y where LocalName y = v
to vs (C.Global x) = return (S.Global x)
to vs (C.Pi a x b) = do
  a' <- to vs a
  b' <- to ((x, FZ) : map (fmap FS) vs) b
  return (S.Pi a' (L.bind x b'))
to vs (C.Pos s a) = do
  a' <- to vs a
  return (S.Pos s a')
to vs (C.Let x a b) = do
  a' <- to vs a
  b' <- to ((x, FZ) : map (fmap FS) vs) b
  return (S.Let a' (L.bind x b'))
to vs (C.Lam v b) = do
  b' <- to ((v, FZ) : map (fmap FS) vs) b
  return $ S.Lam (L.bind v b')
to vs (C.App f a) = do
  f' <- to vs f
  a' <- to vs a
  return $ S.App f' a'
to vs (C.TyCon n tys) = do
  tys' <- mapM (to vs) tys
  return $ S.TyCon n tys'
to vs (C.DataCon n args) = do
  args' <- mapM (to vs) args
  return $ S.DataCon n args'
to vs (C.Case a brs) = do
  a' <- to vs a
  brs' <- mapM (toM vs) brs
  return $ S.Case a' brs'
to vs (C.Ann a b) = do
  a' <- to vs a
  b' <- to vs b
  return $ S.Ann a' b'
to vs (C.TyEq a b) = do
  a' <- to vs a
  b' <- to vs b
  return $ S.TyEq a' b'
to vs C.TmRefl = return S.TmRefl
to vs (C.Subst a b) = do
  a' <- to vs a
  b' <- to vs b
  return $ S.Subst a' b'
to vs (C.Contra a) = do
  a' <- to vs a
  return $ S.Contra a'
to vs C.TrustMe = return S.TrustMe
to vs C.PrintMe = return S.PrintMe

--------------------------------------------------------------------------------
--- The converse transformation, from abstract to concrete syntax
--------------------------------------------------------------------------------

type Unscope n a = ScopedReader LocalName n a

getName :: Fin n -> Unscope n LocalName
getName n = do
  bnds <- Scoped.scope
  return $ Scoped.scope_names bnds Vec.! n

unscopeModule :: S.Module -> Unscope Z C.Module
unscopeModule m = do
  entries <- mapM unscopeModuleEntry $ S.moduleEntries m
  return C.Module {
    C.moduleName = S.moduleName m,
    C.moduleImports = S.moduleImports m,
    C.moduleEntries = entries,
    C.moduleConstructors = S.moduleConstructors m
  }

unscopeModuleEntry :: S.ModuleEntry -> Unscope Z C.ModuleEntry
unscopeModuleEntry (S.ModuleDecl gn ty) = C.ModuleDecl gn <$> unscope ty
unscopeModuleEntry (S.ModuleDef gn tm) = C.ModuleDef gn <$> unscope tm
unscopeModuleEntry (S.ModuleData dn dat) = C.ModuleData dn <$> unscopeDatatype dat
unscopeModuleEntry (S.ModuleFail f) = C.ModuleFail <$> unscopeModuleEntry f

unscopeDatatype :: S.DataDef -> Unscope Z C.DataDef
unscopeDatatype (S.DataDef delta sort cstrs) = do
  delta' <- unscopeTelescope delta
  sort' <- unscope sort
  cstrs' <- Scoped.push delta $ mapM unscopeConstructor cstrs
  return $ C.DataDef delta' sort' cstrs'

unscopeConstructor :: forall n. S.ConstructorDef n -> Unscope (n + Z) C.ConstructorDef
unscopeConstructor (S.ConstructorDef name theta) = case axiomPlusZ @n of
  Refl -> C.ConstructorDef Nothing name <$> unscopeTelescope theta

unscopeTelescope :: S.Telescope p n -> Unscope n [C.Entry]
unscopeTelescope Scoped.TNil = return []
unscopeTelescope (Scoped.TCons h t) =
  (:) <$> unscopeLocal h <*>  Scoped.push h (unscopeTelescope t)

unscopeLocal :: S.Local p n -> Unscope n C.Entry
unscopeLocal (S.LocalDecl n t) = C.EntryDecl n <$> unscope t
unscopeLocal (S.LocalDef n t) = C.EntryDef <$> getName n <*> unscope t

unscope :: S.Term n -> ScopedReader LocalName n C.Term
unscope S.TyType = pure C.TyType
unscope (S.Lam bnd) = do
  let (x, t) = L.unbindl bnd
  C.Lam x <$> Scoped.push x (unscope t)
unscope (S.Var x) = C.Var <$> getName x
unscope (S.Global n) = return $ C.Global n
unscope (S.Pi ty bnd) = do
  ty' <- unscope ty
  let (x, t) = L.unbindl bnd
  t' <- Scoped.push x $ unscope t
  return $ C.Pi ty' x t'
unscope (S.Pos pos t) = C.Pos pos <$> unscope t
unscope (S.Let t1 bnd) = do
  t1' <- unscope t1
  let (x, t2) = L.unbindl bnd
  t2' <- Scoped.push x $ unscope t2
  return $ C.Let x t1' t2'
unscope (S.TyCon name args) = C.TyCon name <$> mapM unscope args
unscope (S.DataCon name args) = C.DataCon name <$> mapM unscope args
unscope (S.Case s matches) = C.Case <$> unscope s <*> mapM unscopeMatch matches
unscope (S.App t1 t2) = C.App <$> unscope t1 <*> unscope t2
unscope (S.Ann t1 t2) = C.Ann <$> unscope t1 <*> unscope t2
unscope (S.TyEq t1 t2) = C.TyEq <$> unscope t1 <*> unscope t2
unscope S.TmRefl = return C.TmRefl
unscope (S.Subst t1 t2) = C.Subst <$> unscope t1 <*> unscope t2
unscope (S.Contra t) = C.Contra <$> unscope t
unscope S.TrustMe = return C.TrustMe
unscope S.PrintMe = return C.PrintMe

unscopeMatch :: S.Match n -> ScopedReader LocalName n C.Match
unscopeMatch (S.Branch bnd) = do
  (pat, t) <- Scoped.withSize $ return $ Pat.unbindl bnd
  C.Branch (unscopePattern pat) <$> Scoped.push pat (unscope t)

unscopePattern :: S.Pattern n -> C.Pattern
unscopePattern (S.PatCon name pats) = C.PatCon name $ unscopePatList pats
unscopePattern (S.PatVar n) = C.PatVar n

unscopePatList :: Pat.PatList S.Pattern p -> [C.Pattern]
unscopePatList Pat.PNil = []
unscopePatList (Pat.PCons pat t) = unscopePattern pat : unscopePatList t