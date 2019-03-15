{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}

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

instance Monad m => MonadError () (InnerT Maybe m) where
    throwError _ = InnerT $ pure Nothing
    catchError (InnerT m) f = InnerT $ m >>= \case
        Nothing -> unInnerT $ f ()
        Just x  -> pure $ Just x

instance Monad m => MonadError e (InnerT (Either e) m) where
    throwError = InnerT . pure . Left
    catchError (InnerT m) f = InnerT $ m >>= \case
        Left e  -> unInnerT $ f e
        Right x -> pure $ Right x
