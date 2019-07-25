{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
-- | This module assigns types to built-ins.Typed
-- See the @plutus/language-plutus-core/docs/Constant application.md@
-- article for how this emerged.

{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeApplications          #-}

module Language.PlutusCore.Constant.Typed
    ( OpaqueTerm (..)
    , TypeScheme (..)
    , TypedBuiltinName (..)
    , DynamicBuiltinNameMeaning (..)
    , DynamicBuiltinNameDefinition (..)
    , DynamicBuiltinNameMeanings (..)
    ) where

import           Language.PlutusCore.Evaluation.Result
import           Language.PlutusCore.Lexer.Type hiding (name)
import           Language.PlutusCore.Name
import           Language.PlutusCore.Pretty
import           Language.PlutusCore.Constant.Universe
import           Language.PlutusCore.StdLib.Data.Unit
import           Language.PlutusCore.Type
import           Language.PlutusCore.Constant.DefaultUni
import           PlutusPrelude

import           Control.Monad.Except
import           Control.Monad.Morph                         as Morph
import           Control.Monad.Reader
import           Control.Monad.Trans.Compose                 (ComposeT (..))
import           Control.Monad.Trans.Inner
import           Data.GADT.Compare
import           Data.Map                                    (Map)
import           Data.Proxy
import           Data.Text                                   (Text)
import           GHC.TypeLits

infixr 9 `TypeSchemeArrow`

{- Note [The reverse example]
Having a dynamic built-in with the following signature:

    reverse : all a. list a -> list a

that maps to Haskell's

    reverse :: forall a. [a] -> [a]

evaluation of

    PLC.reverse {bool} (cons true (cons false nil))

proceeds as follows:

      PLC.reverse {bool} (cons true (cons false nil))
    ~ makeKnown (Haskell.reverse (readKnown (cons true (cons false nil))))
    ~ makeKnown (Haskell.reverse [OpaqueTerm true, OpaqueTerm false])
    ~ makeKnown [OpaqueTerm false, OpaqueTerm true]
    ~ cons false (cons true nil)

Note how we use 'OpaqueTerm' in order to wrap a PLC term as a Haskell value using 'readKnown' and
then unwrap the term back using 'makeKnown' without ever inspecting the term.
-}

-- See Note [The reverse example] for an example.
-- | The denotation of a term whose type is a bound variable.
-- I.e. the denotation of such a term is the term itself.
-- This is because we have parametricity in Haskell, so we can't inspect a value whose
-- type is a bound variable, so we never need to convert such a term from Plutus Core to Haskell
-- and back and instead can keep it intact.
newtype OpaqueTerm uni (text :: Symbol) (unique :: Nat) = OpaqueTerm
    { unOpaqueTerm :: Term TyName Name uni ()
    }

-- | Type schemes of primitive operations.
-- @a@ is the Haskell denotation of a PLC type represented as a 'TypeScheme'.
-- @r@ is the resulting type in @a@, e.g. the resulting type in
-- @ByteString -> Size -> Integer@ is @Integer@.
data TypeScheme uni a r where
    TypeSchemeResult  :: uni `Includes` a => Proxy a -> TypeScheme uni a a
    TypeSchemeArrow   :: uni `Includes` a => Proxy a -> TypeScheme uni b r -> TypeScheme uni (a -> b) r
    TypeSchemeAll
        :: (KnownSymbol text, KnownNat uniq)
           -- Here we require the user to manually provide the unique of a type variable.
           -- That's nothing but silly, but I do not see what else we can do with the current design.
           -- Once the 'BuiltinPipe' thing gets implemented, we'll be able to bind 'uniq' inside
           -- the continuation and also put there the @KnownNat uniq@ constraint
           -- (i.e. use universal quantification for uniques) and that should work alright.
        => Proxy '(text, uniq)
           -- We use a funny trick here: instead of binding
           --
           -- > TypedBuiltin (OpaqueTerm text uniq)
           --
           -- directly we introduce an additional and "redundant" type variable. The reason why we
           -- do that is because this way we can also bind such a variable later when constructing
           -- the 'TypeScheme' of a polymorphic builtin, so that for the user this looks exactly
           -- like a single bound type variable instead of this weird @OpaqueTerm text uniq@ thing.
           --
           -- And note that in most cases we do not need to bind anything at the type level and can
           -- use the variable bound at the term level directly, because it's of the type that
           -- 'TypeSchemeResult' expects. Type-level binding is only needed when you want to apply
           -- a type constructor to the variable, like in
           --
           -- > reverse : all a. list a -> list a
        -> (forall ot. ot ~ OpaqueTerm uni text uniq => Proxy ot -> TypeScheme uni a r)
        -> TypeScheme uni a r

    -- The @r@ is rather ad hoc and needed only for tests.
    -- We could use type families to compute it instead of storing as an index.
    -- That's a TODO perhaps.

-- | A 'BuiltinName' with an associated 'TypeScheme'.
data TypedBuiltinName uni a r = TypedBuiltinName BuiltinName (TypeScheme uni a r)

{- Note [DynamicBuiltinNameMeaning]
We represent the meaning of a 'DynamicBuiltinName' as a 'TypeScheme' and a Haskell denotation.
We need both while evaluting a 'DynamicBuiltinName', because 'TypeScheme' is required for
well-typedness to avoid using 'unsafeCoerce' and similar junk, while the denotation is what
actually computes. We do not need denotations for type checking, nor strongly typed 'TypeScheme'
is required, however analogously to static built-ins, we compute the types of dynamic built-ins from
their 'TypeScheme's. This way we only define a 'TypeScheme', which we anyway need, and then compute
the corresponding 'Type' from it. And we can't go the other way around -- from untyped to typed --
of course. Therefore a typed thing has to go before the corresponding untyped thing and in the
final pipeline one has to supply a 'DynamicBuiltinNameMeaning' for each of the 'DynamicBuiltinName's.
-}

-- See Note [DynamicBuiltinNameMeaning].
-- | The meaning of a dynamic built-in name consists of its 'Type' represented as a 'TypeScheme'
-- and its Haskell denotation.
data DynamicBuiltinNameMeaning uni =
    forall a r. DynamicBuiltinNameMeaning (TypeScheme uni a r) a

-- | The definition of a dynamic built-in consists of its name and meaning.
data DynamicBuiltinNameDefinition uni =
    DynamicBuiltinNameDefinition DynamicBuiltinName (DynamicBuiltinNameMeaning uni)

-- | Mapping from 'DynamicBuiltinName's to their 'DynamicBuiltinNameMeaning's.
newtype DynamicBuiltinNameMeanings uni = DynamicBuiltinNameMeanings
    { unDynamicBuiltinNameMeanings :: Map DynamicBuiltinName (DynamicBuiltinNameMeaning uni)
    } deriving (Semigroup, Monoid)

instance (GShow uni, Closed uni, uni `Everywhere` Pretty) =>
            Pretty (OpaqueTerm uni text unique) where
    pretty = pretty . unOpaqueTerm
