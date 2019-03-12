-- | This module defines a common type various evaluation machine use to return their results.

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.PlutusCore.Evaluation.Result
    ( EvaluationResult (..)
    , EvaluationResultDef
    , evaluationResultToMaybe
    , maybeToEvaluationResult
    ) where

import           Language.PlutusCore.Name
import           Language.PlutusCore.Pretty
import           Language.PlutusCore.Type
import           PlutusPrelude

import           Control.Monad.Except

-- | The parameterized type of results various evaluation engines return.
-- On the PLC side this becomes (via @makeDynamicBuiltin@) either a call to 'error' or
-- a value of the PLC counterpart of type @a@.
data EvaluationResult a
    = EvaluationSuccess a
    | EvaluationFailure
    deriving (Show, Eq, Functor, Foldable, Traversable)

-- | The default type of results various evaluation engines return.
type EvaluationResultDef = EvaluationResult (Value TyName Name ())

instance Applicative EvaluationResult where
    pure = EvaluationSuccess

    EvaluationSuccess f <*> a = fmap f a
    EvaluationFailure   <*> _ = EvaluationFailure

instance Monad EvaluationResult where
    EvaluationSuccess x >>= f = f x
    EvaluationFailure   >>= _ = EvaluationFailure

instance Alternative EvaluationResult where
    empty = EvaluationFailure

    EvaluationSuccess x <|> _ = EvaluationSuccess x
    EvaluationFailure   <|> a = a

-- instance e ~ () => MonadError e EvaluationResult where
--     throwError () = llift EvaluationFailure
--     catchError (T m) f = T $ m >>= \case
--         EvaluationFailure   -> unT $ f ()
--         EvaluationSuccess x -> pure $ EvaluationSuccess x

-- instance (e ~ (), Monad m) => MonadError e (T EvaluationResult m) where
--     throwError () = llift EvaluationFailure
--     catchError (T m) f = T $ m >>= \case
--         EvaluationFailure   -> unT $ f ()
--         EvaluationSuccess x -> pure $ EvaluationSuccess x

instance PrettyBy config a => PrettyBy config (EvaluationResult a) where
    prettyBy config (EvaluationSuccess value) = prettyBy config value
    prettyBy _      EvaluationFailure         = "Failure"

instance PrettyClassic a => Pretty (EvaluationResult a) where
    pretty = prettyClassicDef

-- | Map 'EvaluationSuccess' to 'Just' and 'EvaluationFailure' to 'Nothing'.
evaluationResultToMaybe :: EvaluationResult a -> Maybe a
evaluationResultToMaybe (EvaluationSuccess res) = Just res
evaluationResultToMaybe EvaluationFailure       = Nothing

-- | Map 'Just' to 'EvaluationSuccess' and 'Nothing' to 'EvaluationFailure'.
maybeToEvaluationResult :: Maybe a -> EvaluationResult a
maybeToEvaluationResult (Just res) = EvaluationSuccess res
maybeToEvaluationResult Nothing    = EvaluationFailure
