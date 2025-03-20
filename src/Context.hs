-- {-# LANGUAGE FunctionalDependencies #-}

-- import AutoEnv
-- import AutoEnv.Bind.Local qualified as Local
-- import Control.Monad.Reader (Reader)
-- import Control.Monad.Reader.Class qualified as Monomorphic
-- import Control.Monad.Cont (ContT)
-- import Control.Monad.Except (ExceptT)
-- import Control.Monad.Identity (IdentityT)
-- import qualified Control.Monad.State.Lazy as Lazy
-- import qualified Control.Monad.State.Strict as Strict
-- import qualified Control.Monad.Writer.CPS as CPS
-- import qualified Control.Monad.Writer.Lazy as Lazy
-- import qualified Control.Monad.Writer.Strict as Strict

-- class (forall r. Monad (m r)) => MonadReader (m :: Type -> Type -> Type) where
--   ask :: m r r
--   local :: (r -> r') -> m r' a -> m r a
--   reader :: (r -> r') -> m r r'

-- instance (MonadReader m) => Monomorphic.MonadReader r (m r) where
--   ask = pask
--   local = plocal
--   reader = preader

-- -- instance PMonadReader (->) where
-- --   pask = id
-- --   preader = id
-- --   plocal = flip (.)

-- -- ---------------------------------------------------------------------------
-- -- Instances for other mtl transformers
-- --
-- -- All of these instances need UndecidableInstances,
-- -- because they do not satisfy the coverage condition.

-- instance MonadReader r' m => MonadReader r' (ContT r m) where
--     ask   = lift ask
--     local = Cont.liftLocal ask local
--     reader = lift . reader

-- instance MonadReader r m => MonadReader r (ExceptT e m) where
--     ask   = lift ask
--     local = mapExceptT . local
--     reader = lift . reader

-- instance MonadReader r m => MonadReader r (IdentityT m) where
--     ask   = lift ask
--     local = mapIdentityT . local
--     reader = lift . reader

-- instance MonadReader r m => MonadReader r (Maybe m) where
--     ask   = lift ask
--     local = mapMaybeT . local
--     reader = lift . reader

-- instance MonadReader r m => MonadReader r (Lazy.StateT s m) where
--     ask   = lift ask
--     local = Lazy.mapStateT . local
--     reader = lift . reader

-- instance MonadReader r m => MonadReader r (Strict.StateT s m) where
--     ask   = lift ask
--     local = Strict.mapStateT . local
--     reader = lift . reader

-- instance (Monoid w, MonadReader r m) => MonadReader r (CPS.WriterT w m) where
--     ask   = lift ask
--     local = CPS.mapWriterT . local
--     reader = lift . reader

-- instance (Monoid w, MonadReader r m) => MonadReader r (Lazy.WriterT w m) where
--     ask   = lift ask
--     local = Lazy.mapWriterT . local
--     reader = lift . reader

-- instance (Monoid w, MonadReader r m) => MonadReader r (Strict.WriterT w m) where
--     ask   = lift ask
--     local = Strict.mapWriterT . local
--     reader = lift . reader

-- --------------------------------------------------------------------------------

-- class LocalExtensible a t where
--   extend :: forall n. LocalName -> a n -> a (S n)

-- underUnbind ::
--   forall m n c t a b.
--   (MonadReader m, LocalExtensible c t, Subst t a) =>
--   Local.Bind t a n ->
--   (a (S n) -> m (c (S n)) b) ->
--   (LocalName, m (c n) b)
-- underUnbind bnd k = do
--   let (x, t) = Local.unbind bnd
--   (x,) <$> plocal (extend @c @t x) $ k t

-- under :: (MonadReader m, LocalExtensible c t, Subst t a) => Local.Bind t a n -> (a (S n) -> m (c (S n)) b) -> m (c n) b
-- under bnd k = snd $ underUnbind bnd k