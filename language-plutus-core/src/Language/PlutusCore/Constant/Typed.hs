-- | This module assigns types to built-ins.
-- See the @plutus/language-plutus-core/docs/Constant application.md@
-- article for how this emerged.

{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
-- {-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators             #-}

module Language.PlutusCore.Constant.Typed where
--     ( {- BuiltinSized (..)
--     , TypedBuiltinSized (..)
--     , SizeEntry (..)
--     , BuiltinType (..) -}
--     --   TypedBuiltin (..)
--     -- , TypedBuiltinValue (..)
--       TypeScheme (..)
--     , TypedBuiltinName (..)
--     , DynamicBuiltinNameMeaning (..)
--     , DynamicBuiltinNameDefinition (..)
--     , DynamicBuiltinNameMeanings (..)
--     , Evaluator
--     , Evaluate
--     , Convert
--     , KnownType (..)
--     , eraseTypedBuiltinSized
--     , runEvaluate
--     , withEvaluator
--     , readKnownM
--     ) where

import           Language.PlutusCore.Constant.Dynamic.Pretty
import           Language.PlutusCore.Evaluation.Result
import           Language.PlutusCore.Lexer.Type
import           Language.PlutusCore.Name
import           Language.PlutusCore.Name
import           Language.PlutusCore.Pretty
import           Language.PlutusCore.Quote
import           Language.PlutusCore.StdLib.Data.Unit
import           Language.PlutusCore.Type                    hiding (Value)
import           PlutusPrelude

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.ByteString.Lazy.Char8                  as BSL
import           Data.Map                                    (Map)
import           Data.Proxy
import           Data.Text                                   (Text)
import           GHC.TypeLits

newtype Sized a (s :: k) = Sized
    { unSized :: a
    }

data Dict c where
    Dict :: c => Dict c

data Holds c f where
    Holds :: c a => f a -> Holds c f

data BuiltinPipe (sized :: * -> Nat -> *) arg where
    BuiltinPipeRes
        :: (sized ~ Sized => Holds KnownType arg)
        -> BuiltinPipe sized arg
    BuiltinPipeAllType
        :: (Type TyName () -> BuiltinPipe sized arg)
        -> BuiltinPipe sized arg
    BuiltinPipeAllSize
        :: (forall s. KnownNat s => Proxy s -> BuiltinPipe sized arg)
        -> BuiltinPipe sized arg
    BuiltinPipeArg
        :: (sized ~ Sized => Dict (KnownType a))
        -> (arg a -> BuiltinPipe sized arg)
        -> BuiltinPipe sized arg

showAbstractSized :: proxy (sized Integer s) -> Type TyName ()
showAbstractSized = undefined

-- \(b :: arg Bool) -> ...
addIntegerPipe :: forall sized arg. Applicative arg => BuiltinPipe sized arg
addIntegerPipe =
    BuiltinPipeAllSize $ \(_ :: Proxy s) ->
    BuiltinPipeArg Dict $ \(aX :: arg (sized Integer s)) ->
    BuiltinPipeArg Dict $ \(aY :: arg (sized Integer s)) ->
    BuiltinPipeRes $ Holds $
        addInteger @s <$> aX <*> aY

data BoundS a (unique :: Nat)

-- Uniques as KnownNats mapped to their human-readable indices.
asType :: BuiltinPipe BoundS Proxy -> Quote (Type TyName ())
asType = go mempty where
    go :: UniqueMap TypeUnique Int -> BuiltinPipe BoundS Proxy -> Quote (Type TyName ())
    go levels = undefined -- (BuiltinPipeRes _) = undefined

a = 'a'

-- -- Seems to work.
-- data AKnownType a where
--     AKnownType :: KnownType a => AKnownType a

-- data AKnownArg arg where
--     AKnownArg :: KnownType a => arg a -> AKnownArg arg

-- data BuiltinPipe (var :: Nat -> *) arg where
--     BuiltinPipeVal
--         :: (var ~ Value => AKnownArg arg)
--         -> BuiltinPipe var arg
--     BuiltinPipeAllType
--         :: (Type TyName () -> BuiltinPipe var arg)
--         -> BuiltinPipe var arg
--     BuiltinPipeAllSize
--         :: (forall s. KnownNat s => Proxy s -> BuiltinPipe var arg)
--         -> BuiltinPipe var arg
--     BuiltinPipeArg
--         :: (var ~ Value => AKnownType a)
--         -> (arg a -> BuiltinPipe var arg)
--         -> BuiltinPipe var arg

-- data Value (a :: k) = Value
-- data Bound (a :: k) = Bound

-- -- class KnownVar a where

-- -- instance KnownNat n => KnownVar (Value n) where
-- -- instance KnownNat n => KnownVar (Bound n) where

-- -- \(b :: arg Bool) -> ...
-- addIntegerPipe :: forall var arg. Applicative arg => BuiltinPipe var arg
-- addIntegerPipe =
--     BuiltinPipeAllSize $ \(_ :: Proxy s) ->
--     BuiltinPipeArg AKnownType $ \(aX :: arg (Sized Integer (var s))) ->
--     BuiltinPipeArg AKnownType $ \(aY :: arg (Sized Integer (var s))) ->
--     BuiltinPipeVal $ AKnownArg $
--         addInteger @s <$> aX <*> aY




-- -- Wrong ("<whatever> is untouchable" junk).
-- data BuiltinPipeVal arg size where
--     BuiltinPipeValRes  :: KnownType a => a -> BuiltinPipeVal arg size
--     BuiltinPipeValAllSize
--         :: size s
--         -> (KnownNat s => BuiltinPipeVal arg size)
--         -> BuiltinPipeVal arg size
--     BuiltinPipeValArg
--         :: arg a
--         -> (a -> BuiltinPipeVal arg size)
--         -> BuiltinPipeVal arg size

-- data AsKnownType a where
--     AsKnownType :: KnownType a => AsKnownType a

-- data IsSized sized where
--     IsSized :: sized ~ Sized => IsSized sized

-- data BuiltinPipe (sized :: * -> Nat -> *) arg where
--     BuiltinPipeVal     :: (sized ~ Sized => AsKnownType a) -> (Applicative arg => IsSized sized -> arg a) -> BuiltinPipe sized arg
--     BuiltinPipeAllType :: (Type TyName () -> BuiltinPipe sized arg) -> BuiltinPipe sized arg
--     BuiltinPipeAllSize :: (forall s. KnownNat s => Proxy s -> BuiltinPipe sized arg) -> BuiltinPipe sized arg
--     BuiltinPipeArg     :: (sized ~ Sized => AsKnownType a) -> (arg a -> BuiltinPipe sized arg) -> BuiltinPipe sized arg



-- -- Wrong.
-- data (a :: k) :~: b where
--     Refl :: a :~: a

-- data ANat k where
--     ANat :: ANat Nat

-- data BuiltinPipe arg where
--     BuiltinPipeVal     :: KnownType a => arg a -> BuiltinPipe arg
--     BuiltinPipeAllType :: (Type TyName () -> BuiltinPipe arg) -> BuiltinPipe arg
--     BuiltinPipeAllSize
--         :: (forall k (s :: k). KnownLit s => Proxy s -> arg (ANat k) -> BuiltinPipe arg)
--         -> BuiltinPipe arg
--     BuiltinPipeArg     :: KnownType a => (arg a -> BuiltinPipe arg) -> BuiltinPipe arg

-- class KnownLit (s :: k) where
--     type Lit k
--     litVal :: proxy s -> Lit k

-- instance KnownNat n => KnownLit (n :: Nat) where
--     type Lit Nat = Integer
--     litVal = natVal @n

-- instance KnownSymbol s => KnownLit (s :: Symbol) where
--     type Lit Symbol = String
--     litVal = symbolVal @s

-- -- class KnownSize (s :: k) a | k -> a where
-- --     reifySize :: proxy s -> a

-- -- instance KnownNat n => KnownSize (n :: Nat) Integer where
-- --     reifySize = natVal @n

-- -- instance KnownSymbol s => KnownSize (s :: Symbol) String where
-- --     reifySize = symbolVal @s

-- -- \(b :: arg Bool) -> ...
-- addIntegerPipe :: forall arg. Monad arg => BuiltinPipe arg
-- addIntegerPipe =
--     BuiltinPipeAllSize $ \(_ :: Proxy s) qS ->
--     BuiltinPipeArg $ \(aX :: arg (Sized Integer s)) ->
--     BuiltinPipeArg $ \(aY :: arg (Sized Integer s)) ->
--     BuiltinPipeVal $
--         (qS >>= \ANat -> (addInteger @s <$> aX <*> aY) :: arg (EvaluationResult (Sized Integer s)))



-- -- Wrong
-- data BuiltinPipeVal arg size where
--     BuiltinPipeValRes  :: KnownType a => a -> BuiltinPipeVal arg size
--     BuiltinPipeValAllSize
--         :: size
--         -> (forall s. KnownNat s => Proxy s -> BuiltinPipeVal arg size)
--         -> BuiltinPipeVal arg size
--     BuiltinPipeValArg
--         :: arg a
--         -> (a -> BuiltinPipeVal arg size)
--         -> BuiltinPipeVal arg size

-- data BuiltinPipe arg size where
--     BuiltinPipeVal     :: BuiltinPipeVal arg size -> BuiltinPipe arg size
--     BuiltinPipeAllType :: (Type TyName () -> BuiltinPipe arg size) -> BuiltinPipe arg size
--     BuiltinPipeAllSize :: (size -> BuiltinPipe arg size) -> BuiltinPipe arg size
--     BuiltinPipeArg     :: KnownType a => (arg a -> BuiltinPipe arg size) -> BuiltinPipe arg size

-- addIntegerPipe :: BuiltinPipe arg size
-- addIntegerPipe =
--     BuiltinPipeAllSize $ \aS ->
--     BuiltinPipeArg $ \aX ->
--     BuiltinPipeArg $ \aY ->
--     BuiltinPipeVal $
--         BuiltinPipeValAllSize aS $ \(_ :: Proxy s) ->
--         BuiltinPipeValArg aX $ \x ->
--         BuiltinPipeValArg aY $ \y ->
--         BuiltinPipeValRes $
--             addInteger @s x y

-- - PLC types can be extracted from pipes
-- - thus the value-level part can be ignored
-- - and Haskell types determine PLC types
-- - but the value-level part has to be present
-- - and it must be well-typed



-- Correct, but PLC types can't be extracted.
-- data BuiltinPipe where
--     -- BuiltinPipeRes :: Either Text (Term TyName Name ()) -> BuiltinPipe
--     BuiltinPipeRes     :: KnownType a => a -> BuiltinPipe
--     BuiltinPipeAllType :: (Type TyName () -> BuiltinPipe) -> BuiltinPipe
--     BuiltinPipeAllSize :: (forall s. KnownNat s => Proxy s -> BuiltinPipe) -> BuiltinPipe
--     BuiltinPipeArg     :: KnownType a => (a -> BuiltinPipe) -> BuiltinPipe

-- addIntegerPipe :: BuiltinPipe
-- addIntegerPipe =
--     BuiltinPipeAllSize $ \(_ :: Proxy s) ->
--     BuiltinPipeArg $ \x ->
--     BuiltinPipeArg $ \y ->
--     BuiltinPipeRes $ addInteger @s x y

{-
data BuiltinPipe
    = BuiltinPipeRes (forall m. Monad m => EvaluateT ConvertT m (Term TyName Name ()))
    | BuiltinPipeAll (Type TyName ()      -> BuiltinPipe)
    | BuiltinPipeArg (Term TyName Name () -> BuiltinPipe)

withKnownNat :: Natural -> (forall n. KnownNat n => Proxy n -> c) -> c
withKnownNat nat k =
    case someNatVal nat of
        SomeNat proxy -> k proxy

withKnownSize
    :: Type TyName ann
    -> (forall s. KnownNat s => Proxy s -> EvaluateT ConvertT m c)
    -> EvaluateT ConvertT m c
withKnownSize ty k =
    -- TODO: evaluate `ty`.
    case ty of
        TyInt _ s -> withKnownNat s k

addIntegerPipe :: BuiltinPipe
addIntegerPipe =
    BuiltinPipeAll $ \tS ->
    BuiltinPipeArg $ \tX ->
    BuiltinPipeArg $ \tY ->
    BuiltinPipeRes $
        withKnownSize tS $ \(_ :: Proxy s) -> do
            x <- readKnownM tX
            y <- readKnownM tY
            makeKnown =<< addInteger @s x y
-}

-- -- Could have this, but still need to type check stuff like `f addInteger`, but then we can't
-- -- instantiate lambdas.
-- BuiltinPipeAll $ \t1 ->
-- BuiltinPipeAll $ \t2 ->
-- (typeSchema, operation)

-- BuiltinPipeAll $ \s ->
-- (Arg . Arg $ Res @(EvaluationResult (Sized Integer s)), addInteger @s)

-- addInteger : all s. integer s -> integer s -> integer s
-- cast : all a b. a -> maybe b

-- -- | The definition of a dynamic built-in consists of its name and meaning.
-- data DynamicBuiltinNameDefinition =
--     DynamicBuiltinNameDefinition DynamicBuiltinName DynamicBuiltinNameMeaning

-- -- | Mapping from 'DynamicBuiltinName's to their 'DynamicBuiltinNameMeaning's.
-- newtype DynamicBuiltinNameMeanings = DynamicBuiltinNameMeanings
--     { unDynamicBuiltinNameMeanings :: Map DynamicBuiltinName DynamicBuiltinNameMeaning
--     } deriving (Semigroup, Monoid)

data DynamicBuiltinNameMeanings

instance Semigroup DynamicBuiltinNameMeanings
instance Monoid DynamicBuiltinNameMeanings

type Evaluator f m = DynamicBuiltinNameMeanings -> f TyName Name () -> m EvaluationResultDef

newtype EvaluateT t m a = EvaluateT
    { unEvaluateT :: ReaderT (Evaluator Term m) (t m) a
    } deriving
        ( Functor, Applicative, Monad, Alternative, MonadPlus
        , MonadReader (Evaluator Term m)
        , MonadError e
        )

runEvaluate :: Evaluator Term m -> EvaluateT t m a -> t m a
runEvaluate eval (EvaluateT a) = runReaderT a eval

withEvaluator :: (Evaluator Term m -> t m a) -> EvaluateT t m a
withEvaluator = EvaluateT . ReaderT

{- Note [Semantics of dynamic built-in types]
We only allow dynamic built-in types that

1. can be represented using static types in PLC. For example Haskell's 'Char' can be represented as
@integer 4@ in PLC. This restriction makes the dynamic built-in types machinery somewhat similar to
type aliases in Haskell (defined via the @type@ keyword). The reason for this restriction is that
storing values of arbitrary types of a host language in the AST of a target language is commonly far
from being trivial, hence we do not support this right now, but we plan to figure out a way to allow
such extensions to the AST
2. are of kind @*@. Dynamic built-in types that are not of kind @*@ can be encoded via recursive
instances. For example:

    instance KnownType dyn => KnownType [dyn] where
        ...

This is due to the fact that we use Haskell classes to assign semantics to dynamic built-in types and
since it's anyway impossible to assign a meaning to an open PLC type, because you'd have to somehow
interpret free variables, we're only interested in closed PLC types and those can be handled by
recursive instances as shown above.

Since type classes are globally coherent by design, we also have global coherence for dynamic built-in
types for free. Any dynamic built-in type means the same thing regardless of the blockchain it's
added to. It may prove to be restrictive, but it's a good property to start with, because less things
can silently stab you in the back.

An @KnownType dyn@ instance provides

1. a way to encode @dyn@ as a PLC type ('getTypeEncoding')
2. a function that encodes values of type @dyn@ as PLC terms ('makeKnown')
3. a function that decodes PLC terms back to Haskell values ('readKnown')

The last two are ought to constitute an isomorphism (modulo 'Maybe').
-}

{- Note [Converting PLC values to Haskell values]
The first thought that comes to mind when you asked to convert a PLC value to the corresponding Haskell
value is "just match on the AST". This works nicely for simple things like 'Char's which we encode as
@integer@s, see the @KnownType Char@ instance below.

But how to convert something more complicated like lists? A PLC list gets passed as argument to
a built-in after it gets evaluated to WHNF. We can't just match on the AST here, because after
the initial lambda it can be anything there: function applications, other built-ins, recursive data,
anything. "Well, just normalize it" -- not so fast: for one, we did not have a term normalization
procedure at the moment this note was written, for two, it's not something that can be easily done,
because you have to carefully handle uniques (we generate new terms during evaluation) and perform type
substitutions, because types must be preserved.

Besides, matching on the AST becomes really complicated: you have to ensure that a term does have
an expected semantics by looking at the term's syntax. Huge pattern matches followed by multiple
checks that variables have equal names in right places and have distinct names otherwise. Making a
mistake is absolutely trivial here. Of course, one could just omit checks and hope it'll work alright,
but eventually it'll break and debugging won't be fun at all.

So instead of dealing with syntax of terms, we deal with their semantics. Namely, we evaluate terms
using some evaluator (normally, the CEK machine). For the temporary lack of ability to put values of
arbitrary Haskell types into the Plutus Core AST, we convert PLC values to Haskell values and "emit"
the latter via a combination of 'unsafePerformIO' and 'IORef'. For example, we fold a PLC list with
a dynamic built-in name (called `emit`) that calls 'unsafePerformIO' over a Haskell function that
appends an element to the list stored in an 'IORef':

    plcListToHaskellList list =
        evaluateCek anEnvironment (foldList {dyn} {unit} (\(r : unit) -> emit) unitval list)

After evaluation finishes, we read a Haskell list from the 'IORef'
(which requires another 'unsafePerformIO') and return it.
-}

{- Note [Evaluators]
A dynamic built-in name can be applied to something that contains uninstantiated variables. There are
several possible ways to handle that:

1. each evaluator is required to perform substitutions to instantiate all variables in arguments to
built-ins. The drawback is that this can be inefficient in cases when there are many applications of
built-ins and arguments are of non-primitive types. Besides, substitution is tricky and is trivial to
screw up
2. we can break encapsulation and pass environments to the built-ins application machinery, so that it
knows how to instantiate variables. This would work for the strict CEK machine, but the lazy
CEK machine also has a heap and there can be other evaluators that have their internal state that
can't just be thrown away and it's impossible for the built-ins application machinery to handle states
of all possible evaluators beforehand
3. or we can just require to pass the current evaluator with its encapsulated state to functions that
evaluate built-in applications. The type of evaluators is this then:

    type Evaluator f = DynamicBuiltinNameMeanings -> f TyName Name () -> EvaluationResult

so @Evaluator Term@ receives a map with meanings of dynamic built-in names which extends the map the
evaluator already has (this is needed, because we add new dynamic built-in names during conversion of
PLC values to Haskell values, see Note [Converting PLC values to Haskell values]), a 'Term' to evaluate
and returns an 'EvaluationResult' (we may want to later add handling of errors here). Thus, whenever
we want to resume evaluation during computation of a dynamic built-in application, we just call the
received evaluator

(3) seems best, so it's what is implemented.
-}


-- | The monad in which we convert PLC terms to Haskell values.
-- Conversion can fail with
--
-- 1. 'EvaluationFailure' if at some point constants stop fitting into specified sizes.
-- 2. A textual error if a PLC term can't be converted to a Haskell value of a specified type.
newtype ConvertT m a = ConvertT
    { unConvertT :: ExceptT Text (InnerT EvaluationResult m) a
    } deriving
        ( Functor, Applicative, Monad
        , MonadError Text
        )

-- GHC does not want to derive this for some reason.
instance MonadTrans ConvertT where
    lift = ConvertT . lift . lift

instance Monad m => Alternative (ConvertT m) where
    empty = ConvertT . lift $ llift empty
    ConvertT (ExceptT (InnerT m)) <|> ConvertT (ExceptT (InnerT n)) =
        ConvertT . ExceptT . InnerT $ (<|>) <$> m <*> n

convertM :: Monad m => m (EvaluationResult a) -> ConvertT m a
convertM = ConvertT . lift . InnerT

class KnownTypeAst a where
    -- | The type representing @a@ used on the PLC side.
    toTypeAst :: proxy a -> Type TyName ()

-- See Note [Semantics of dynamic built-in types].
-- See Note [Converting PLC values to Haskell values].
-- Types and terms are supposed to be closed, hence no 'Quote'.
-- | Haskell types known to exist on the PLC side.
class KnownTypeAst a => KnownType a where
    -- | Convert a Haskell value to the corresponding PLC value.
    -- 'Left' represents a conversion failure.
    makeKnown :: MonadError Text m => a -> m (Term TyName Name ())

    -- See Note [Evaluators].
    -- | Convert a PLC value to the corresponding Haskell value.
    readKnown :: Monad m => Evaluator Term m -> Term TyName Name () -> ConvertT m a

readKnownM
    :: (Monad m, KnownType a)
    => Term TyName Name () -> EvaluateT ConvertT m a
readKnownM term = withEvaluator $ \eval -> readKnown eval term

instance KnownTypeAst () where
    toTypeAst _ = unit

-- Encode '()' from Haskell as @all r. r -> r@ from PLC.
-- This is a very special instance, because it's used to define functions that are needed for
-- other instances, so we keep it here.
instance KnownType () where
    -- We need this matching, because otherwise Haskell expressions are thrown away rather than being
    -- evaluated and we use 'unsafePerformIO' in multiple places, so we want to compute the '()' just
    -- for side effects that the evaluation may cause.
    makeKnown () = pure unitval

    readKnown eval term = do
        let int1 = TyApp () (TyBuiltin () TyInteger) (TyInt () 4)
            asInt1 = Constant () . BuiltinInt () 1
        res <- convertM . eval mempty . Apply () (TyInst () term int1) $ asInt1 1
        case res of
            Constant () (BuiltinInt () 1 1) -> pure ()
            _                               -> throwError "Not a builtin ()"

addInteger
    :: KnownNat s => Sized Integer s -> Sized Integer s -> EvaluationResult (Sized Integer s)
addInteger = undefined

-- instance KnownLit s => KnownType (Sized Integer s) where
--     toTypeAst _ = undefined
--     makeKnown = undefined
--     readKnown = undefined

instance KnownNat s => KnownTypeAst (Sized Integer s) where
    toTypeAst = undefined

instance KnownNat s => KnownType (Sized Integer s) where
    makeKnown = undefined
    readKnown = undefined

instance KnownTypeAst a => KnownTypeAst (EvaluationResult a) where
    toTypeAst = undefined

instance KnownType a => KnownType (EvaluationResult a) where
    makeKnown = undefined
    readKnown = undefined


















-- -- | Built-in types indexed by @size@.
-- data BuiltinSized
--     = BuiltinSizedInt
--     | BuiltinSizedBS
--     | BuiltinSizedSize
--     deriving (Show, Eq)

-- -- | Built-in types indexed by @size@ along with their denotation.
-- data TypedBuiltinSized a where
--     TypedBuiltinSizedInt  :: TypedBuiltinSized Integer
--     TypedBuiltinSizedBS   :: TypedBuiltinSized BSL.ByteString
--     -- See Note [Semantics of sizes].
--     TypedBuiltinSizedSize :: TypedBuiltinSized ()

-- -- | Type-level sizes.
-- data SizeEntry size
--     = SizeValue Size  -- ^ A constant size.
--     | SizeBound size  -- ^ A bound size variable.
--     deriving (Eq, Ord, Functor)
-- -- We write @SizeEntry Size@ sometimes, so this data type is not perfect, but it works fine.

-- -- | Built-in types.
-- data BuiltinType size
--     = BuiltinSized (SizeEntry size) BuiltinSized

-- -- | Built-in types. A type is considired "built-in" if it can appear in the type signature
-- -- of a primitive operation. So @boolean@ is considered built-in even though it is defined in PLC
-- -- and is not primitive.
-- data TypedBuiltin size a where
--     TypedBuiltinSized :: SizeEntry size -> TypedBuiltinSized a -> TypedBuiltin size a
--     -- Any type that implements 'KnownType' can be lifted to a 'TypedBuiltin',
--     -- because any such type has a PLC representation and provides conversions back and forth
--     -- between Haskell and PLC and that's all we need.
--     TypedBuiltinDyn   :: KnownType dyn => TypedBuiltin size dyn










-- instance (size ~ Size, PrettyDynamic a) => Pretty (KnownTypeValue a) where
--     pretty = undefined
--     pretty (TypedBuiltinValue (TypedBuiltinSized se _) x) = pretty se <+> "!" <+> prettyDynamic x
--     pretty (TypedBuiltinValue TypedBuiltinDyn          x) = prettyDynamic x




-- -- | A 'BuiltinName' with an associated 'TypeScheme'.
-- data TypedBuiltinName a r = TypedBuiltinName BuiltinName (forall size. TypeScheme size a r)
-- I attempted to unify various typed things, but sometimes type variables must be universally
-- quantified, sometimes they must be existentially quatified. And those are distinct type variables.

-- -- | Convert a 'TypedBuiltinSized' to its untyped counterpart.
-- eraseTypedBuiltinSized :: TypedBuiltinSized a -> BuiltinSized
-- eraseTypedBuiltinSized TypedBuiltinSizedInt  = BuiltinSizedInt
-- eraseTypedBuiltinSized TypedBuiltinSizedBS   = BuiltinSizedBS
-- eraseTypedBuiltinSized TypedBuiltinSizedSize = BuiltinSizedSize

-- instance Pretty BuiltinSized where
--     pretty BuiltinSizedInt  = "integer"
--     pretty BuiltinSizedBS   = "bytestring"
--     pretty BuiltinSizedSize = "size"

-- instance Pretty (TypedBuiltinSized a) where
--     pretty = pretty . eraseTypedBuiltinSized

-- instance Pretty size => Pretty (SizeEntry size) where
--     pretty (SizeValue size) = pretty size
--     pretty (SizeBound size) = pretty size

-- instance Pretty size => Pretty (TypedBuiltin size a) where
--     pretty (TypedBuiltinSized se tbs) = parens $ pretty tbs <+> pretty se
--     TODO: do we want this entire thing to be 'PrettyBy' rather than 'Pretty'?
--     This is just used in errors, so we probably do not care much.
--     pretty dyn@TypedBuiltinDyn        = prettyPlcDef $ toTypeAst dyn
