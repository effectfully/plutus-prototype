{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Control.Monad.Trans.Inner
    ( InnerT (..)
    , forBind
    , llift
    ) where

import           Control.Monad
import           Control.Monad.Except

forBind :: (Monad m, Traversable m, Applicative f) => m a -> (a -> f (m b)) -> f (m b)
forBind a f = join <$> traverse f a

-- |
-- > T Identity   ~ IdentityT
-- > T Maybe      ~ MaybeT
-- > T (Either e) ~ ExceptT e
-- > T ((,) w)    ~ WriterT    -- the tuple is flipped compared to the usual 'WriterT'
newtype InnerT f m a = InnerT
    { unInnerT :: m (f a)
    }

llift :: Applicative m => f a -> InnerT f m a
llift = InnerT . pure

-- ReaderInnerT r m a ~ r -> m a
-- StateInnerT s m a ~ s -> m (s, a)

instance (Functor f, Functor m) => Functor (InnerT f m) where
    fmap f (InnerT a) = InnerT $ fmap (fmap f) a

instance (Applicative f, Applicative m) => Applicative (InnerT f m) where
    pure = llift . pure

    InnerT f <*> InnerT a = InnerT $ (<*>) <$> f <*> a

instance (Monad f, Traversable f, Monad m) => Monad (InnerT f m) where
    InnerT m >>= f = InnerT $ do
        a <- m
        forBind a (unInnerT . f)

instance Applicative f => MonadTrans (InnerT f) where
    lift = InnerT . fmap pure

instance (e ~ (), Monad m) => MonadError e (InnerT Maybe m) where
    throwError _ = InnerT $ pure Nothing
    catchError (InnerT m) f = InnerT $ m >>= \case
        Nothing -> unInnerT $ f ()
        Just x  -> pure $ Just x

instance Monad m => MonadError e (InnerT (Either e) m) where
    throwError = InnerT . pure . Left
    catchError (InnerT m) f = InnerT $ m >>= \case
        Left e  -> unInnerT $ f e
        Right x -> pure $ Right x

-- throwErrorDefault :: e -> t f m a
-- catchErrorDefault :: InnerT f m a -> (e -> InnerT f m a) -> InnerT f m a

-- class Monad m => MonadInnerT f m | m -> f where
--     throwCatch :: f (f () -> m a) -> m a
--     throwCatch :: f (m a, f Void -> m a) -> m a
--     default throwCatch :: (m ~ t n, MonadInnerT f n) => f (f () -> m a) -> m a
--     throwCatch = _

-- throwError :: e -> m a
-- catchError :: m a -> (e -> m a) -> m a

-- Either e (m a, e -> m a) -> m a
-- Either e (Either e () -> m a) -> m a

-- f ~ Either e
-- f (f () -> m a) -> m a






-- {-# LANGUAGE DefaultSignatures      #-}
-- {-# LANGUAGE FlexibleInstances      #-}
-- {-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE LambdaCase             #-}
-- {-# LANGUAGE MultiParamTypeClasses  #-}
-- {-# LANGUAGE TypeFamilies           #-}
-- {-# LANGUAGE UndecidableInstances   #-}

-- module Control.Monad.InnerT.T
--     ( InnerT (..)
--     , forBind
--     , llift
--     ) where

-- import           Control.Applicative
-- import           Control.Monad
-- import           Control.Monad.Except
-- import           Data.Functor.Compose
-- import           Data.Functor.Identity

-- forBind :: (Monad m, Traversable m, Applicative f) => m a -> (a -> f (m b)) -> f (m b)
-- forBind a f = join <$> traverse f a

-- -- |
-- -- > InnerT Identity Identity   ~ IdentityT
-- -- > InnerT Identity Maybe      ~ MaybeT
-- -- > InnerT Identity (Either e) ~ ExceptT e
-- -- > InnerT Identity ((,) w)    ~ WriterT w  -- the tuple is flipped compared to the usual 'WriterT'
-- -- > InnerT ((->) r) Identity   ~ ReaderT r
-- -- > InnerT ((->) s) ((,) s)    ~ StateT  s
-- newtype InnerT g f m a = InnerT
--     { unInnerT :: g (m (f a))
--     }

-- type InnerT = InnerT Identity
-- type OuterT g = InnerT g Identity

-- llift :: (Applicative g, Applicative m) => f a -> InnerT g f m a
-- llift = InnerT . pure . pure

-- -- ReaderInnerT r m a ~ r -> m a
-- -- StateInnerT s m a ~ s -> m (s, a)

-- instance (Functor g, Functor m, Functor f) => Functor (InnerT g f m) where
--     fmap f (InnerT a) = InnerT $ fmap (fmap (fmap f)) a

-- instance (Applicative g, Applicative m, Applicative f) => Applicative (InnerT g f m) where
--     pure = llift . pure

--     InnerT f <*> InnerT a = InnerT $ liftA2 (liftA2 (<*>)) f a

-- instance (Monad g, Traversable g, Monad m, Monad f, Traversable f) => Monad (InnerT g f m) where
--     InnerT m >>= f = InnerT $ do
--         a <- fmap Compose m
--         _ $ forBind a (fmap Compose . unInnerT . f)
-- --         a <- m
-- --         forBind a (unInnerT . f)

-- instance Applicative f => MonadInnerT (InnerT g f) where
--     lift = InnerT . fmap pure

-- instance (e ~ (), Monad m) => MonadError e (InnerT g Maybe m) where
--     throwError _ = InnerT $ pure Nothing
--     catchError (InnerT m) f = InnerT $ m >>= \case
--         Nothing -> unInnerT $ f ()
--         Just x  -> pure $ Just x

-- instance Monad m => MonadError e (InnerT g (Either e) m) where
--     throwError = InnerT . pure . Left
--     catchError (InnerT m) f = InnerT $ m >>= \case
--         Left e  -> unInnerT $ f e
--         Right x -> pure $ Right x

-- -- throwErrorDefault :: e -> t f m a
-- -- catchErrorDefault :: InnerT f m a -> (e -> InnerT f m a) -> InnerT f m a

-- -- class Monad m => MonadInnerT f m | m -> f where
-- --     throwCatch :: f (f () -> m a) -> m a
-- --     throwCatch :: f (m a, f Void -> m a) -> m a
-- --     default throwCatch :: (m ~ t n, MonadInnerT f n) => f (f () -> m a) -> m a
-- --     throwCatch = _

-- -- throwError :: e -> m a
-- -- catchError :: m a -> (e -> m a) -> m a

-- -- Either e (m a, e -> m a) -> m a
-- -- Either e (Either e () -> m a) -> m a

-- -- f ~ Either e
-- -- f (f () -> m a) -> m a
