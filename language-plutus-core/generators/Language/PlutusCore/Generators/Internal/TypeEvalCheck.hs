{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE PolyKinds              #-}
-- | This module defines types and functions related to "type-eval checking".

{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE UndecidableInstances   #-}

{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE GADTs                  #-}

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

import           Data.Proxy

import           GHC.Exts                                                (Constraint)

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



data Err1 = Err1                    deriving (Show)
data Err2 = Err2                    deriving (Show)
data Err3 = Err31 Err1 | Err32 Err2 deriving (Show)
newtype Err4 = Err43 Err3           deriving (Show)
newtype Err5 = Err52 Err2           deriving (Show)


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


data SList as where
    SNil  :: SList '[]
    SCons :: Proxy a -> SList as -> SList (a ': as)

class IList as where
    slist :: SList as

instance IList '[] where
    slist = SNil

instance IList as => IList (a ': as) where
    slist = SCons Proxy slist

type family SubtypeOf a :: [*]

type family Head (as :: [k]) :: k where
    Head (a ': _) = a

type family Append as bs :: [k] where
    Append '[]       bs = bs
    Append (a ': as) bs = a ': Append as bs

type family ConcatMapPaths p a bs :: [[*]] where
    ConcatMapPaths p a '[]       = '[]
    ConcatMapPaths p a (b ': bs) = Append (Paths p a b) (ConcatMapPaths p a bs)

type family Paths p a b :: [[*]] where
    Paths p a a = p ': '[]
    Paths p a b = ConcatMapPaths (b ': p) a (SubtypeOf b)

type family Path a b where
    Path a b = Head (Paths '[] a b)

class a <! b where
    upcastDirect :: a -> b

type family Connect a (bs :: [*]) c :: Constraint where
    Connect a '[]       c = a ~ c
    Connect a (b ': bs) c = (a <! b, Connect b bs c)

upcastDirectTo :: a <! b => proxy b -> a -> b
upcastDirectTo _ = upcastDirect

upcastBy :: Connect a ps b => SList ps -> a -> b
upcastBy  SNil        x = x
upcastBy (SCons p ps) x = upcastBy ps $ upcastDirectTo p x

class a <: b where
    upcast :: a -> b

instance (Connect a (Path a b) b, IList (Path a b)) => a <: b where
    upcast = upcastBy (slist :: SList (Path a b))

-- expected: A <: c
-- have: A <: B, B <: c

-- expected: Connect A (Path A c) c, IList (Path A c)
-- have: Connect A (Path A B) B, IList (Path A B)
--       Connect B (Path B c) c, IList (Path B c)

-- prove: Path a c == Path a b ++ Path b c

-- expected: Connect a (ps ++ qs) c, IList (ps ++ qs)
-- have: Connect a ps b, IList ps
--       Connect b qs c, IList qs


withTrans :: forall a b c r. (a <: b, b <: c) => Proxy (a, b, c) -> (a <: c => r) -> r
withTrans _ = go (slist :: SList (Path b c)) where
    go :: SList ps
       -> ((Connect a ps c, IList ps) => r)
       -> r
    go  SNil        r = _
    go (SCons p ps) r = _

type instance SubtypeOf Err1 = '[]
type instance SubtypeOf Err2 = '[]
type instance SubtypeOf Err3 = '[Err1, Err2]
type instance SubtypeOf Err4 = '[Err3]
type instance SubtypeOf Err5 = '[Err2]

instance Err1 <! Err3 where
    upcastDirect = Err31

instance Err2 <! Err3 where
    upcastDirect = Err32

instance Err3 <! Err4 where
    upcastDirect = Err43

instance Err2 <! Err5 where
    upcastDirect = Err52

test1 :: Err1 -> Err3
test1 = upcast

test2 :: Err2 -> Err3
test2 = upcast

test3 :: Err1 -> Err4
test3 = upcast

test4 :: Err2 -> Err4
test4 = upcast

test5 :: Err2 -> Err5
test5 = upcast

{-
## Current situation

Right now we use `Control.Monad.Error.Lens` and there are a number of problems with it.

I've recently added the value restriction check and now the type signature of `parseTypecheck` looks like this:

```haskell
parseTypecheck
    :: (AsParseError e AlexPosn,
        AsValueRestrictionError e TyName AlexPosn,
        AsUniqueError e AlexPosn,
        AsNormalizationError e TyName Name AlexPosn,
        AsTypeError e AlexPosn,
        MonadError e m,
        MonadQuote m)
    => TypeCheckConfig -> BSL.ByteString -> m (NormalizedType TyName ())
```

This is quite unwieldy. Moreover, `printType`, `printNormalizeType` and `parseScoped` -- all of them needed to be updated. What we really want to write is

```haskell
parseTypecheck
    :: (AsError e AlexPosn,
        MonadError e m,
        MonadQuote m)
    => TypeCheckConfig -> BSL.ByteString -> m (NormalizedType TyName ())
```

Where `AsError` subsumes all the other errors as it's actually defined like that.

So we get an O(n) overhead in O(m) functions, that's a O(n*m) overhead in total, which is pretty bad.

In addition to this overhead, we also need to write boilerplate like

```haskell
instance ann ~ () => AsError TypeEvalCheckError ann where
    _Error = _TypeEvalCheckErrorIllFormed . _Error

instance (tyname ~ TyName, ann ~ ()) => AsValueRestrictionError TypeEvalCheckError tyname ann where
    _ValueRestrictionError = _TypeEvalCheckErrorIllFormed . _ValueRestrictionError
```

We know that `AsError err ann` implies `AsValueRestrictionError err tyname ann`, but the current approach does not allow to communicate this information to the compiler.

## Description of the problem

Arbitrary error types form the following DAG:

```
                 ...
                   \
Err6     ...        Err7
    \    /         /    \
     Err4      Err5      \
    /    \    /    \      \
 ...      Err1      Err2   Err3
                          /
                      Err8
```

There shouldn't be any "diamonds" (a "diamond" means that an error can be embedded into another error via distinct constructors). What we would like to have is a system where you can say that the children of `Err7` are `Err5` and `Err3` (denoted as `Err3 <: Err7` and `Err5 <: 7`) and automatically get `Err1 <: Err7`. `Err2 <: Err7`, `Err3 <: Err7`, so that you can directly throw an `Err1` in a context that allows to throw `Err7`.

## An example

We'll be using the following example to show how various approaches work:

```haskell
data Err1 = Err1
data Err2 = Err2
data Err3 = Err31 Err1 | Err32 Err2
newtype Err4 = Err43 Err3
newtype Err5 = Err52 Err2
```

which forms the following DAG:

```
      Err4
        |
      Err3      Err5
     /    \    /
 Err1      Err2
```

## A non-solution

Bottom-up type inference:

```haskell
type family SupertypeOf e
type instance SupertypeOf Err1 = Err3
type instance SupertypeOf Err2 = Err3
type instance SupertypeOf Err2 = Err5
type instance SupertypeOf Err3 = Err4
```

Here we define a single type family `SupertypeOf` and specify the super type of each of the errors.

"What do you mean "the" supertype? There can be many!"
"Wait a minute, `Err2` does have two supertypes, these type family instances are incorrect! There can be only one instance per type."

Yep, but if we define those type instances in different files, then we're fine. This means that we won't be able to use `Err3` and `Err5` in the same file, but maybe this is acceptable sometimes.

But this approach is super fragile and my mom told me to be a responsible person and never cheat on my compiler, so we won't implement that.

## An imaginary solution

It would be just great, if it was possible to write the following (I'm using functions as opposed to prisms for simplicity):

```haskell
class SuperOfErr1 a where
  upcastErr1 :: Err1 -> a

class SuperOfErr1 a => SuperOfErr3 a where
  upcastErr3 :: Err3 -> a

  instance SuperOfErr1 a where
    upcastErr1 = upcastErr3 . Err3_1
```

Here we declare two classes, `SuperOfErr1` and `SuperOfErr3`, and say that any `SuperOfErr3 a` instance also brings in scope the `SuperOfErr1 a` instance. And we don't need to manually satisfy the `SuperOfErr1 a` constraint of `SuperOfErr3 a` -- it's satisfied automatically!

Unfortunately, this feature does not seem to exist and I'm not able to emulate it in any way.

## A possible solution

But what about top-down inference? We can have the following type family:

```haskell
type family SubtypeOf a :: [*]
type instance SubtypeOf Err3 = '[Err1, Err2]
type instance SubtypeOf Err4 = '[Err3]
```

<end_of_the_manuscript>

I.e. describe
-}


--       Err4
--         |
--       Err3      Err5
--      /    \    /
--  Err1      Err2

-- type family SupertypeOf e
-- type instance SupertypeOf Err1 = Err3
-- type instance SupertypeOf Err2 = Err3
-- type instance SupertypeOf Err2 = Err5
-- type instance SupertypeOf Err3 = Err4

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
