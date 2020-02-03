{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.PlutusCore.Core.Type
    ( Gas(..)
    , TyMeta(..)
    , Meta(..)
    , Kind(..)
    , TypeBuiltin(..)
    , Type(..)
    , BuiltinName(..)
    , DynamicBuiltinName(..)
    , StagedBuiltinName(..)
--     , Builtin(..)
    , Constant(..)
    , Term(..)
    , Value
    , Version(..)
    , Program(..)
    , Normalized(..)
    , HasUniques
    , defaultVersion
--     , allBuiltinNames
    -- * Helper functions
    , toTerm
    , termAnn
    , typeAnn
    )
where

import           PlutusPrelude

import           Language.PlutusCore.Name
import           Language.PlutusCore.Constant.Universe

import           Control.Lens
import qualified Data.ByteString.Lazy       as BSL
import           Data.Hashable
import           Data.Text                  (Text)
import           GHC.Exts                   (Constraint)
import           Instances.TH.Lift          ()
import           Language.Haskell.TH.Syntax (Lift)

import           Control.DeepSeq

{- Note [Annotations and equality]
Equality of two things does not depend on their annotations.
So don't use @deriving Eq@ for things with annotations.
-}

data TyMeta euni where
    TyMetaBuiltin :: uni a -> TyMeta (Extend fun uni)

data Meta euni where
    MetaConstant :: uni a -> a -> Meta (Extend fun uni)
    MetaFunction :: fun -> Meta (Extend fun uni)

instance Show (TyMeta euni) where
    show = undefined
instance NFData (TyMeta euni) where
    rnf = undefined
instance Lift (TyMeta euni) where
    lift = undefined

instance Show (Meta euni) where
    show = undefined
instance NFData (Meta euni) where
    rnf = undefined
instance Lift (Meta euni) where
    lift = undefined

newtype Gas = Gas
    { unGas :: Natural
    }

data Kind ann
    = Type ann
    | KindArrow ann (Kind ann) (Kind ann)
    deriving (Functor, Show, Generic, NFData, Lift, Hashable)

-- | A 'Type' assigned to expressions.
data Type tyname uni ann
    = TyVar ann (tyname ann)
    | TyFun ann (Type tyname uni ann) (Type tyname uni ann)
    | TyIFix ann (Type tyname uni ann) (Type tyname uni ann)
      -- ^ Fix-point type, for constructing self-recursive types
    | TyForall ann (tyname ann) (Kind ann) (Type tyname uni ann)
    | TyBuiltin ann (TyMeta uni)
    | TyLam ann (tyname ann) (Kind ann) (Type tyname uni ann)
    | TyApp ann (Type tyname uni ann) (Type tyname uni ann)
    deriving (Show, Functor, Generic, NFData, Lift)

data Term tyname name uni ann
    = Var ann (name ann) -- ^ a named variable
    | TyAbs ann (tyname ann) (Kind ann) (Term tyname name uni ann)
    | LamAbs ann (name ann) (Type tyname uni ann) (Term tyname name uni ann)
    | Apply ann (Term tyname name uni ann) (Term tyname name uni ann)
    | Constant ann (Meta uni)
    | TyInst ann (Term tyname name uni ann) (Type tyname uni ann)
    | Unwrap ann (Term tyname name uni ann)
    | IWrap ann (Type tyname uni ann) (Type tyname uni ann) (Term tyname name uni ann)
    | Error ann (Type tyname uni ann)
    deriving (Functor, Generic, Hashable)

type Value = Term

-- | Version of Plutus Core to be used for the program.
data Version ann
    = Version ann Natural Natural Natural
    deriving (Show, Functor, Generic, NFData, Lift, Hashable)

-- | A 'Program' is simply a 'Term' coupled with a 'Version' of the core language.
data Program tyname name uni ann = Program ann (Version ann) (Term tyname name uni ann)
    deriving (Functor, Generic, Hashable)

newtype Normalized a = Normalized
    { unNormalized :: a
    } deriving (Show, Eq, Functor, Foldable, Traversable, Lift, Generic)
      deriving newtype NFData
      deriving Applicative via Identity
deriving newtype instance PrettyBy config a => PrettyBy config (Normalized a)

type ParametersHave (constr :: * -> Constraint) tyname name uni ann =
    (uni `Everywhere` constr, constr (tyname ann), constr (name ann), constr ann)

deriving instance (GShow uni, ParametersHave Show tyname name uni ann) => Show (Term tyname name uni ann)
deriving instance ParametersHave NFData tyname name uni ann => NFData (Term tyname name uni ann)
deriving instance ParametersHave Lift tyname name uni ann => Lift (Term tyname name uni ann)

deriving instance (GShow uni, ParametersHave Show tyname name uni ann) => Show (Program tyname name uni ann)
deriving instance ParametersHave NFData tyname name uni ann => NFData (Program tyname name uni ann)
deriving instance ParametersHave Lift tyname name uni ann => Lift (Program tyname name uni ann)

-- | All kinds of uniques an entity contains.
type family HasUniques a :: Constraint
type instance HasUniques (Kind ann) = ()
type instance HasUniques (Type tyname ann) = HasUnique (tyname ann) TypeUnique
type instance HasUniques (Term tyname name ann)
    = (HasUnique (tyname ann) TypeUnique, HasUnique (name ann) TermUnique)
type instance HasUniques (Program tyname name ann) = HasUniques
    (Term tyname name ann)

-- | The default version of Plutus Core supported by this library.
defaultVersion :: ann -> Version ann
defaultVersion ann = Version ann 1 0 0

-- -- | The list of all 'BuiltinName's.
-- allBuiltinNames :: [BuiltinName]
-- allBuiltinNames = [minBound .. maxBound]
-- -- The way it's defined ensures that it's enough to add a new built-in to 'BuiltinName' and it'll be
-- -- automatically handled by tests and other stuff that deals with all built-in names at once.

toTerm :: Program tyname name ann -> Term tyname name ann
toTerm (Program _ _ term) = term

typeAnn :: Type tyname ann -> ann
typeAnn (TyVar ann _       ) = ann
typeAnn (TyFun ann _ _     ) = ann
typeAnn (TyIFix ann _ _    ) = ann
typeAnn (TyForall ann _ _ _) = ann
typeAnn (TyBuiltin ann _   ) = ann
typeAnn (TyLam ann _ _ _   ) = ann
typeAnn (TyApp ann _ _     ) = ann

termAnn :: Term tyname name ann -> ann
termAnn (Var ann _       ) = ann
termAnn (TyAbs ann _ _ _ ) = ann
termAnn (Apply ann _ _   ) = ann
termAnn (Constant ann _  ) = ann
termAnn (TyInst ann _ _  ) = ann
termAnn (Unwrap ann _    ) = ann
termAnn (IWrap ann _ _ _ ) = ann
termAnn (Error ann _     ) = ann
termAnn (LamAbs ann _ _ _) = ann
