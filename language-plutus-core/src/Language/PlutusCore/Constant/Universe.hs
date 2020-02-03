{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Language.PlutusCore.Constant.Universe
    ( Some (..)
    , SomeOf (..)
    , Extend (..)
    , Unextend
    , SomeOriginal (..)
--     , Builtin (..)

    , Includes (..)
    , Everywhere (..)
    , knownUniOf
    , bringApply
    , GShow (..)
    ) where

import           Control.DeepSeq
import           Data.GADT.Compare
import           Data.List
import           Data.Proxy
import           Data.Text.Prettyprint.Doc (Pretty (..))
import           GHC.Exts
import           Language.Haskell.TH.Syntax (Lift (..))
import Data.GADT.Show

data Some f = forall a. Some (f a)
data SomeOf f = forall a. SomeOf (f a) a

data Extend b uni a where
    Extension :: Extend b uni b
    Original  :: uni a -> Extend b uni a

data SomeOriginal euni where
    SomeOriginal :: euni ~ Extend b uni => uni a -> SomeOriginal euni -- (Extend b uni)

type family Unextend (euni :: * -> *) :: * -> * where
    Unextend (Extend b uni) = uni

instance GEq uni => GEq (Extend b uni) where
    geq Extension       Extension       = Just Refl
    geq (Original uni1) (Original uni2) = geq uni1 uni2
    geq _               _               = Nothing

-- We probably want to use that together with `fastsum`.
-- But also allow @Either@ and use type families for computing the index of a type,
-- because we want to extend @uni@ in order to unlift values.
class uni `Includes` a where
    knownUni :: uni a

knownUniOf :: uni `Includes` a => proxy a -> uni a
knownUniOf _ = knownUni

class uni `Everywhere` (constr :: * -> Constraint) where
    bring :: proxy constr -> uni a -> (constr a => r) -> r

bringApply
    :: uni `Everywhere` constr
    => Proxy constr -> (forall a. constr a => a -> r) -> SomeOf uni -> r
bringApply proxy f (SomeOf uni x) = bring proxy uni $ f x

parens :: String -> String
parens str = "(" ++ str ++ ")"

instance (euni ~ Extend b uni, GShow uni) => Show (SomeOriginal euni) where
    show (SomeOriginal uni) = "SomeOriginal " ++ parens (gshow uni)

instance GShow uni => Show (Some uni) where
   show (Some uni) = "Some " ++ parens (gshow uni)

instance (GShow uni, uni `Everywhere` Show) => Show (SomeOf uni) where
    show (SomeOf uni x) =
        intercalate " "
            [ "SomeOf"
            , parens $ gshow uni
            , parens $ bring (Proxy @Show) uni (show x)
            ]

instance (euni ~ Extend b uni, GShow uni) => Pretty (SomeOriginal euni) where
    pretty (SomeOriginal uni) = pretty $ gshow uni

instance GShow uni => Pretty (Some uni) where
    pretty (Some uni) = pretty $ gshow uni

instance uni `Everywhere` Pretty => Pretty (SomeOf uni) where
    pretty = bringApply (Proxy @Pretty) pretty

instance GEq uni => Eq (Some uni) where
    Some uni1 == Some uni2 = uni1 `defaultEq` uni2

instance (GEq uni, uni `Everywhere` Eq) => Eq (SomeOf uni) where
    SomeOf uni1 x1 == SomeOf uni2 x2 =
        case uni1 `geq` uni2 of
            Nothing   -> False
            Just Refl -> bring (Proxy @Eq) uni1 (x1 == x2)

-- We could use 'NFData1' here, but we don't really need it for our particular case.
instance NFData (SomeOriginal euni) where
    rnf (SomeOriginal a) = a `seq` ()

-- We could use 'NFData1' here, but we don't really need it for our particular case.
instance NFData (Some f) where
    rnf (Some a) = a `seq` ()

instance uni `Everywhere` NFData => NFData (SomeOf uni) where
    rnf = bringApply (Proxy @NFData) rnf

instance Lift (SomeOriginal uni) where
    lift = undefined -- TODO

instance Lift (Some uni) where
    lift = undefined -- TODO

instance uni `Everywhere` Lift => Lift (SomeOf uni) where
    lift = bringApply (Proxy @Lift) lift
