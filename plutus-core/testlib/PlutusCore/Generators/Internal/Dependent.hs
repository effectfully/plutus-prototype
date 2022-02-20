-- | Orphan 'GEq' and 'GCompare' instances for known types.
-- The reason we keep the instances separate is that they are highly unsafe ('unsafeCoerce' is used)
-- and needed only for tests.

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module PlutusCore.Generators.Internal.Dependent
    ( AsKnownTypeAst (..)
    , proxyAsKnownTypeAst
    ) where

import PlutusPrelude

import PlutusCore.Builtin

import Data.GADT.Compare
import Universe
import Unsafe.Coerce

liftOrdering :: Ordering -> GOrdering a b
liftOrdering LT = GLT
liftOrdering EQ = error "'liftOrdering': 'Eq'"
liftOrdering GT = GGT

-- | Contains a proof that @a@ is a 'KnownTypeAst'.
data AsKnownTypeAst uni a where
    AsKnownTypeAst :: KnownTypeAst uni a => AsKnownTypeAst uni a

instance GShow uni => Pretty (AsKnownTypeAst uni a) where
    pretty a@AsKnownTypeAst = pretty $ toTypeAst @_ @uni a

instance GShow uni => GEq (AsKnownTypeAst uni) where
    a `geq` b = do
        -- TODO: there is a HUGE problem here. @EvaluationResult a@ and @a@ have the same string
        -- representation currently, so we need to either fix that or come up with a more sensible
        -- approach, because an attempt to generate a constant application that may fail results in
        -- UNDEFINED BEHAVIOR.
        -- We can probably require each 'KnownTypeAst' to be 'Typeable' and avoid checking for equality
        -- string representations here, but this complicates the library.
        guard $ display @String a == display b
        Just $ unsafeCoerce Refl

instance GShow uni => GCompare (AsKnownTypeAst uni) where
    a `gcompare` b
        | Just Refl <- a `geq` b = GEQ
        | otherwise              = liftOrdering $ display @String a `compare` display b

-- | Turn any @proxy a@ into an @AsKnownTypeAst a@ provided @a@ is a 'KnownTypeAst'.
proxyAsKnownTypeAst :: KnownTypeAst uni a => proxy a -> AsKnownTypeAst uni a
proxyAsKnownTypeAst _ = AsKnownTypeAst
