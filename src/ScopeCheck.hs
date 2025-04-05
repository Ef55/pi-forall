{-# LANGUAGE FunctionalDependencies #-}

-- |
-- Module      : ScopeCheck
-- Description : Scope checking the Untyped lambda calculus
-- Stability   : experimental
--
-- This module demonstrates a translation from unscoped to well-scoped terms
module ScopeCheck (Some1 (..), Scoping (..), scopeUnder, unscopeUnder, scope, unscope, unscopePattern) where

import AutoEnv qualified
import AutoEnv.Bind.Local qualified as L
import AutoEnv.Bind.Pat (PatList (..))
import AutoEnv.Bind.Pat qualified as Pat
import AutoEnv.Bind.Scoped ((<:>))
import AutoEnv.Bind.Scoped qualified as Scoped
import AutoEnv.Bind.Single qualified as B
import AutoEnv.DependentScope qualified as DS
import AutoEnv.Lib
import AutoEnv.MonadScoped (ScopedReader)
import AutoEnv.MonadScoped qualified as Scoped
import ConcreteSyntax qualified as C
import Control.Monad (foldM)
import Control.Monad qualified as Monad
import Control.Monad.Reader (MonadReader (ask), Reader, asks, runReader)
import Data.Fin qualified as Fin
import Data.Fin qualified as Nat
import Data.Maybe (fromJust)
import Data.Maybe qualified as Maybe
import Data.Type.Nat qualified as Nat
import Data.Vec qualified as Vec
import Syntax qualified as S
import Prelude hiding (lookup)

--------------------------------------------------------------------------------
--- Types which are parametrized by something other than their scope do not fit
--- the API, so we have to handle them separately
--------------------------------------------------------------------------------

data Some1 (p :: Nat -> Type) where
  Some1 :: forall x p. (SNatI x) => (p x) -> Some1 p

data Some2 (p :: Nat -> Nat -> Type) n where
  Some2 :: forall x n p. (SNatI x) => (p x n) -> Some2 p n

scopeTelescope :: forall n. C.Telescope -> Scope n (Some2 S.Telescope n)
scopeTelescope [] = pure $ Some2 Scoped.TNil
scopeTelescope (C.EntryDecl n ty : entries) = do
  ty' <- scope' ty
  Scoped.push n $ do
    Some2 @p1 tele' <- scopeTelescope entries
    let ret = S.LocalDecl n ty' <:> tele'
    withSNat (sPlus (snat @p1) (snat @(S Z))) $
      return $
        Some2 ret
scopeTelescope (C.EntryDef n tm : entries) = do
  tm' <- scope' tm
  Some2 (tele' :: S.Telescope p n) <- scopeTelescope entries
  case axiomPlusZ @p of
    Refl -> do
      ln <- Maybe.fromJust <$> lookup n
      let ret = S.LocalDef ln tm' <:> tele'
      return $ Some2 ret

unscopeTelescope :: S.Telescope p n -> Unscope n [C.Entry]
unscopeTelescope Scoped.TNil = return []
unscopeTelescope (Scoped.TCons h t) =
  (:) <$> unscopeLocal h <*> Scoped.push h (unscopeTelescope t)
  where
    unscopeLocal :: S.Local p n -> Unscope n C.Entry
    unscopeLocal (S.LocalDecl n t) = C.EntryDecl n <$> unscope' t
    unscopeLocal (S.LocalDef n t) = C.EntryDef <$> unscope' (Local n) <*> unscope' t

scopePattern :: C.Pattern -> Scope n (Some1 S.Pattern)
scopePattern (C.PatVar x) = return $ Some1 $ S.PatVar x
scopePattern (C.PatCon name pats) = do
  Some1 pats' <- scopePatList pats
  return $ Some1 $ S.PatCon name pats'
  where
    scopePatList :: [C.Pattern] -> Scope n (Some1 (Pat.PatList S.Pattern))
    scopePatList [] = return $ Some1 Pat.PNil
    scopePatList (p : ps) = do
      Some1 (p' :: S.Pattern p) <- scopePattern p
      Some1 (ps' :: Pat.PatList S.Pattern p1) <- scopePatList ps
      withSNat
        (sPlus (snat :: SNat p1) (snat :: SNat p))
        (return $ Some1 (Pat.PCons p' ps'))

unscopePattern :: S.Pattern p -> C.Pattern
unscopePattern = unscopePattern'
  where
    unscopePattern' :: S.Pattern p -> C.Pattern
    unscopePattern' (S.PatVar n) = C.PatVar n
    unscopePattern' (S.PatCon name pats) = C.PatCon name $ unscopePatList pats

    unscopePatList :: Pat.PatList S.Pattern p -> [C.Pattern]
    unscopePatList Pat.PNil = []
    unscopePatList (Pat.PCons pat t) = unscopePattern' pat : unscopePatList t

--------------------------------------------------------------------------------
--- Scoping interface
--------------------------------------------------------------------------------

type Scope n = Scoped.ScopedReaderT LocalName Maybe n

type Unscope n a = ScopedReader LocalName n a

class Scoping n u s | n u -> s, s -> u, s -> n where
  scope' :: u -> Scope n s
  unscope' :: s -> Unscope n u

scopeUnder :: (SNatI n, Scoping n u s) => Vec n LocalName -> u -> Maybe s
scopeUnder s u = Scoped.runScopedReaderT (scope' u) (Scoped.Scope snat s)

scope :: (Scoping Z u s) => u -> Maybe s
scope = scopeUnder Vec.empty

unscopeUnder :: (SNatI n, Scoping n u s) => Vec n LocalName -> s -> u
unscopeUnder v t = Scoped.runScopedReader v (unscope' t)

unscope :: (Scoping Z u s) => s -> u
unscope = unscopeUnder Vec.empty

--------------------------------------------------------------------------------
--- Scoping instances
--------------------------------------------------------------------------------

data ScopedName n = Local (Fin n) | Global String

toTerm :: ScopedName n -> S.Term n
toTerm (Local n) = S.Var n
toTerm (Global n) = S.Global n

lookup :: LocalName -> Scope n (Maybe (Fin n))
lookup n = iter . Scoped.scope_names <$> Scoped.scope
  where
    iter :: Vec n LocalName -> Maybe (Fin n)
    iter Vec.VNil = Nothing
    iter (h Vec.::: t) = if h == n then Just FZ else FS <$> iter t

instance Scoping n LocalName (ScopedName n) where
  scope' n = do
    n' :: Maybe (Fin n) <- lookup n
    return $ case n' of
      Just x -> Local x
      Nothing -> Global (name n)

  unscope' (Local n) = do
    bnds <- Scoped.scope
    return $ Scoped.scope_names bnds Vec.! n
  unscope' (Global n) = return $ LocalName n

instance Scoping n C.Match (S.Match n) where
  scope' (C.Branch pat tm) = do
    Some1 (pat' :: S.Pattern p) <- scopePattern pat
    tm' <- Scoped.push pat' $ scope' tm
    return (S.Branch (Pat.bind pat' tm'))

  unscope' :: S.Match n -> Unscope n C.Match
  unscope' (S.Branch bnd) = do
    (pat, t) <- return $ Pat.unbindl bnd
    C.Branch (unscopePattern pat) <$> Scoped.push pat (unscope' t)

instance Scoping n C.Term (S.Term n) where
  scope' :: C.Term -> Scope n (S.Term n)
  scope' C.TyType = return S.TyType
  scope' (C.Var v) = toTerm <$> scope' v
  scope' (C.Global x) = return (S.Global x)
  scope' (C.Pi a x b) = do
    a' <- scope' a
    b' <- Scoped.push x $ scope' b
    return (S.Pi a' (L.bind x b'))
  scope' (C.Pos s a) = do
    a' <- scope' a
    return (S.Pos s a')
  scope' (C.Let x a b) = do
    a' <- scope' a
    b' <- Scoped.push x $ scope' b
    return (S.Let a' (L.bind x b'))
  scope' (C.Lam v b) = do
    b' <- Scoped.push v $ scope' b
    return $ S.Lam (L.bind v b')
  scope' (C.App f a) = do
    f' <- scope' f
    a' <- scope' a
    return $ S.App f' a'
  scope' (C.TyCon n tys) = do
    tys' <- mapM (scope') tys
    return $ S.TyCon n tys'
  scope' (C.DataCon n args) = do
    args' <- mapM (scope') args
    return $ S.DataCon n args'
  scope' (C.Case a brs) = do
    a' <- scope' a
    brs' <- mapM scope' brs
    return $ S.Case a' brs'
  scope' (C.Ann a b) = do
    a' <- scope' a
    b' <- scope' b
    return $ S.Ann a' b'
  scope' (C.TyEq a b) = do
    a' <- scope' a
    b' <- scope' b
    return $ S.TyEq a' b'
  scope' C.TmRefl = return S.TmRefl
  scope' (C.Subst a b) = do
    a' <- scope' a
    b' <- scope' b
    return $ S.Subst a' b'
  scope' (C.Contra a) = do
    a' <- scope' a
    return $ S.Contra a'
  scope' C.TrustMe = return S.TrustMe
  scope' C.PrintMe = return S.PrintMe

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
  scope' :: C.ConstructorDef -> Scope n (S.ConstructorDef n)
  scope' (C.ConstructorDef p dc theta) = do
    Some2 theta' <- scopeTelescope theta
    pure $ S.ConstructorDef dc theta'

  unscope' :: S.ConstructorDef n -> Unscope n C.ConstructorDef
  unscope' (S.ConstructorDef name theta) = C.ConstructorDef Nothing name <$> unscopeTelescope theta

instance Scoping Z C.DataDef S.DataDef where
  scope' (C.DataDef delta s cs) = do
    Some2 (delta' :: S.Telescope p Z) <- scopeTelescope delta
    s' <- scope' s
    cs' <- case axiomPlusZ @p of Refl -> Scoped.push delta' $ mapM scope' cs
    return $ S.DataDef delta' s' cs'

  unscope' (S.DataDef @p delta sort cstrs) = do
    delta' <- unscopeTelescope delta
    sort' <- unscope' sort
    cstrs' <- case axiomPlusZ @p of Refl -> Scoped.push delta $ mapM unscope' cstrs
    return $ C.DataDef delta' sort' cstrs'

instance Scoping Z C.ModuleEntry S.ModuleEntry where
  scope' :: C.ModuleEntry -> Scope Z S.ModuleEntry
  scope' (C.ModuleDecl gn ty) = S.ModuleDecl gn <$> scope' ty
  scope' (C.ModuleDef gn tm) = S.ModuleDef gn <$> scope' tm
  scope' (C.ModuleData dn datadef) = S.ModuleData dn <$> scope' datadef
  scope' (C.ModuleFail failing) = S.ModuleFail <$> scope' failing

  unscope' :: S.ModuleEntry -> Unscope Z C.ModuleEntry
  unscope' (S.ModuleDecl gn ty) = C.ModuleDecl gn <$> unscope' ty
  unscope' (S.ModuleDef gn tm) = C.ModuleDef gn <$> unscope' tm
  unscope' (S.ModuleData dn dat) = C.ModuleData dn <$> unscope' dat
  unscope' (S.ModuleFail f) = C.ModuleFail <$> unscope' f

instance Scoping Z C.Module S.Module where
  scope' :: C.Module -> Scope Z S.Module
  scope' m = do
    entries <- mapM scope' $ C.moduleEntries m
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

-- instance Scoping Z (Vec n C.Term) (SNat n, AutoEnv.Env S.Term n n) where
--   scope' = iter AutoEnv.zeroE Nat.SZ
--     where
--       iter :: forall k m. AutoEnv.Ctx S.Term k -> SNat k -> Vec m C.Term -> Scope k (SNat (k + m), AutoEnv.Ctx S.Term (k + m))
--       iter ctx k Vec.VNil = case axiomPlusZ @k of Refl -> return (k, ctx)
--       iter ctx k (x Vec.::: (xs :: Vec m' C.Term)) = do
--         x' <- scope' x
--         let ctx' = ctx AutoEnv.+++ x'
--         case AutoEnv.axiomSus @k @m' of
--           Refl -> Scoped.push (LocalName $ show k) $ iter ctx' (withSNat k Nat.SS) xs

--   unscope' (n, e) = do
--     let u = withSNat n $ Vec.universe @n
--         names = LocalName . show <$> u
--     case axiomPlusZ @n of
--       Refl -> Scoped.push names $ mapM (unscope' . AutoEnv.applyE e . S.Var) u

instance Scoping n (Vec n C.Term) (SNat n, AutoEnv.Env S.Term n n) where
  scope' v = Vec.withDict v $ do
    iv <- mapM scope' v
    let env = AutoEnv.fromTable $ Vec.toList $ Vec.imap (,) iv
    return (snat, env)

  unscope' (n, env) = do
    let u :: Vec n (Fin n) = withSNat n Vec.universe
        ts :: Vec n (S.Term n) = AutoEnv.applyE env . S.Var <$> u
    mapM unscope' ts

instance Scoping n (Vec p (LocalName, C.Term)) (SNat p, DS.Telescope LocalName S.Term p n) where
  -- scope' Vec.VNil = return (SZ, DS.empty)
  -- scope' ((hx, ht) Vec.::: t) = do
  --   ht' :: S.Term n <- scope' ht
  --   (p', t' :: DS.Telescope LocalName S.Term p' (S n)) <- DS.push1 (hx, ht') $ scope' t
  --   let p = withSNat p SS
  --   return $ case axiomAssoc of Refl -> (p, DS.TCons (hx, ht') t')
  scope' v = error "TODO: implement"

  -- iter :: Vec p (LocalName, C.Term) -> (DS.Telescope LocalName S.Term p n -> Scope (p + n) (DS.Telescope LocalName S.Term (S p) n)) -> DS.Telescope LocalName S.Term

  unscope' (p, t) = do
    let (p, v) = DS.fromTelescope @S.Term t
        ns = fst <$> v
        ts = snd <$> v
    ts' <- withSNat p $ Scoped.pushVec ns $ mapM unscope' ts
    return $ Vec.reverse $ Vec.zipWith (,) ns ts'
