{-# LANGUAGE DataKinds              #-}
-- {-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
-- | This module defines types and functions related to "type-eval checking".

{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE UndecidableInstances   #-}

module Language.PlutusCore.Generators.Internal.TypeEvalCheck where
{-    ( TypeEvalCheckError (..)
    , TypeEvalCheckResult (..)
    , TypeEvalCheckM
    , typeEvalCheckBy
    , unsafeTypeEvalCheck
    ) where -}

import           Language.PlutusCore
import           Language.PlutusCore.Constant
import           Language.PlutusCore.Evaluation.CkMachine
import           Language.PlutusCore.Evaluation.Result
import           Language.PlutusCore.Generators.Internal.TypedBuiltinGen
import           Language.PlutusCore.Generators.Internal.Utils
import           Language.PlutusCore.Pretty
import           PlutusPrelude

import           Control.Lens.TH
import           Control.Monad.Except
import           Data.Traversable

{-

{- Note [Type-eval checking]
We generate terms along with values they are supposed to evaluate to. Before evaluating a term,
we type check it. Then we evaluate the term and check whether the expected result matches with
the actual one. Thus "type-eval checking".
-}

-- | The type of errors that can occur during type-eval checking.
data TypeEvalCheckError
--     = TypeEvalCheckErrorOutOfBounds
    = TypeEvalCheckErrorIllFormed (Error ())
    | TypeEvalCheckErrorIllEvaled (Value TyName Name ()) (Value TyName Name ())
      -- ^ The former is an expected result of evaluation, the latter -- is an actual one.
makeClassyPrisms ''TypeEvalCheckError

instance AsError TypeEvalCheckError () where
    _Error = _TypeEvalCheckErrorIllFormed . _Error

instance AsTypeError TypeEvalCheckError () where
    _TypeError = _TypeEvalCheckErrorIllFormed . _TypeError

-- | Type-eval checking of a term results in a value of this type.
data TypeEvalCheckResult = TypeEvalCheckResult
    { _termCheckResultType  :: NormalizedType TyName ()
      -- ^ The type of the term.
    , _termCheckResultValue :: EvaluationResult
      -- ^ The result of evaluation of the term.
    }

instance (PrettyBy config (Error ()), PrettyBy config (Value TyName Name ())) =>
        PrettyBy config TypeEvalCheckError where
    prettyBy config (TypeEvalCheckErrorIllFormed err)             =
        "The term is ill-formed:" <+> prettyBy config err
    prettyBy config (TypeEvalCheckErrorIllEvaled expected actual) =
        "The expected value:" <+> prettyBy config expected <> hardline <>
        "doesn't match with the actual value:" <+> prettyBy config actual

-- | The monad type-eval checking runs in.
type TypeEvalCheckM = ExceptT TypeEvalCheckError Quote

-- See Note [Type-eval checking].
-- | Type check and evaluate a term and check that the expected result is equal to the actual one.
typeEvalCheckBy
    :: (Term TyName Name () -> EvaluationResult) -- ^ An evaluator.
    -> TermOf (TypedBuiltinValue Size a)
    -> TypeEvalCheckM (TermOf TypeEvalCheckResult)
typeEvalCheckBy eval (TermOf term tbv) = TermOf term <$> do
    let typecheck = typecheckTerm (TypeConfig True mempty mempty mempty defaultTypecheckerGas)
    termTy <- typecheck term
    resExpected <- liftQuote $ maybeToEvaluationResult <$> makeBuiltin tbv
    fmap (TypeEvalCheckResult termTy) $
        for ((,) <$> resExpected <*> eval term) $ \(valExpected, valActual) ->
            if valExpected == valActual
                then return valActual
                else throwError $ TypeEvalCheckErrorIllEvaled valExpected valActual

-- | Type check and evaluate a term and check that the expected result is equal to the actual one.
-- Throw an error in case something goes wrong.
unsafeTypeEvalCheck
    :: forall a. TermOf (TypedBuiltinValue Size a) -> Quote (Maybe (TermOf (Value TyName Name ())))
unsafeTypeEvalCheck termOfTbv = do
    errOrRes <- runExceptT $ typeEvalCheckBy evaluateCk termOfTbv
    pure $ case errOrRes of
        Left err         -> errorPlc err
        Right termOfTecr -> traverse (evaluationResultToMaybe . _termCheckResultValue) termOfTecr
-}



data Err1 = Err1
data Err2 = Err2
data Err3 = Err3_1 Err1 | Err3_2 Err2
newtype Err4 = Err4_3 Err3


-- class b <: c where
--   upcast :: b -> c

--   instance a <: b => a <: c where
--     upcast = upcast @b . upcast @a

-- instance Err1 <: Err3 where
--   upcast = Err3_1

-- instance Err2 <: Err3 where
--   upcast = Err3_2

-- instance Err3 <: Err4 where
--   upcast = Err4_3

-- upcast :: Err1 -> Err4
-- upcast :: Err2 -> Err4

--------------------

-- class SuperOfErr1 a where
--   upcastErr1 :: Err1 -> a

-- class SuperOfErr3 a where
--   upcastErr3 :: Err3 -> a
--
--   instance SuperOfErr1 a where
--     upcastErr1 = upcastErr3 . Err3_1

--------------------

type family SubStep a :: [*]

class a <: b where
    type SubStar a b :: [*]
    type SubStar a b = a ': SubStep a
    upcast :: a -> b

type instance SubStep Err3 = '[Err1, Err2]
type instance SubStep Err4 = '[Err3]

-- lookup (Err1, b) (SubStep b)

--     Err4
--       |      -- SubStep
--     Err3
--    /    \    -- SubStep
--  Err1  Err2

-- instance Err1 <: b where
--

-- instance Err3 <: Err4 where
--   type SubStar Err3 Err4 = Err3 ': [Err1, Err2]


--------------------

-- class KnownUpcast a b where
--     knownUpcast :: Upcast a b

-- class Upcasting a b where
--     upcast :: Upcast a b -> a -> b

-- instance (Upcasting Err1 a, KnownUpcast Err1 a) => SuperOfErr1 a where
--     upcastErr1 = upcast knownUpcast

-- data To a b = To (a -> b)

-- data KnownTo

-- class a <: b where
--     data family Upcast a b
--     upcast :: a -> b

-- instance

-- data family Upcast a b c

-- class SuperOfErr1 a where
--     upcastErr1 :: Err1 -> a

-- data instance Upcast Err1 Err3 c = UpcastErr1ViaErr3

-- type family Proxy a c

-- class SuperOfErr3 a where
--     upcastErr3 :: Err3 -> a
--     type ProxyErr3Err1 a
--     type ProxyErr3Err1 a = Err3

-- type instance Proxy Err1 c = ProxyErr3Err1 c
-- -- type instance Proxy Err1 c = c

-- --     upcastErr1ViaErr3 :: {- SuperOfErr3 a => -} Err1 -> a
-- --     upcastErr1ViaErr3 = upcastErr3 . Err3_1

--     -- upcastErr1ViaErr3 :: Upcast Err1 Err3 c
--     -- upcastErr1ViaErr3 = UpcastErr1ViaErr3 (upcastErr3 . Err3_1)

-- instance SuperOfErr3 Err4 where

--     upcastErr3 = Err4_3


-- data instance Upcast Err1 Err3 a

-- instance KnownUpcast a b =? SuperOfErr1 a where
--     upcastErr1 =

-- upcastErr1ViaErr3 :: SuperOfErr3 a => Err1 -> a


--------------------

-- instance Err4 <: b where
--   instance Err1 <: b where
--     upcast = upcast . Err4_1

-- class Upcasting a b where
--   data Upcast a b
--   upcast :: Upcast a b -> a -> b

-- instance Err1 <: b => Err3 <: b where
--   data Upcast Err1 b = UpcastErr1 (Err1 -> b)
--   upcast :: Err3 -> b
--   upcast = ...

-- instance Err1 <: b where
--

-- data family Subtype a b

-- class a <: b where
--     data Upcast a b
--     upcast :: Upcast a b -> a -> b

-- instance Int <: b where
