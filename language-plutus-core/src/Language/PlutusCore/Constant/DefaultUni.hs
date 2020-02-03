{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.PlutusCore.Constant.DefaultUni
    ( ByteString16 (..)
    , DefaultBuiltinName (..)
    , DefaultUni (..)
    ) where

import           PlutusPrelude

import           Language.PlutusCore.Constant.Universe

import qualified Data.ByteString.Lazy                  as BSL
import           Data.GADT.Compare.TH

-- TODO: use strict bytestrings.
newtype ByteString16 = ByteString16
    { unByteString16 :: BSL.ByteString
    }

data DefaultBuiltinName
    = AddInteger
    | SubtractInteger
    | MultiplyInteger
    | DivideInteger
    | QuotientInteger
    | RemainderInteger
    | ModInteger
    | LessThanInteger
    | LessThanEqInteger
    | GreaterThanInteger
    | GreaterThanEqInteger
    | EqInteger
    | Concatenate
    | TakeByteString
    | DropByteString
    | SHA2
    | SHA3
    | VerifySignature
    | EqByteString
    | LtByteString
    | GtByteString

data DefaultUni a where
    DefaultUniInteger     :: DefaultUni Integer
    DefaultUniByteString  :: DefaultUni ByteString16
    DefaultUniBool        :: DefaultUni Bool
    DefaultUniUnit        :: DefaultUni ()
    DefaultUniChar        :: DefaultUni Char
    DefaultUniString      :: DefaultUni String
--     DefaultUniBuiltinName :: DefaultUni (Builtin DefaultBuiltinName)

-- euni ~ Extend name uni

type EDefaultUni = Extend DefaultBuiltinName DefaultUni

deriveGEq ''DefaultUni

instance DefaultUni `Includes` Integer         where knownUni = DefaultUniInteger
instance DefaultUni `Includes` ByteString16    where knownUni = DefaultUniByteString
instance DefaultUni `Includes` Bool            where knownUni = DefaultUniBool
instance DefaultUni `Includes` ()              where knownUni = DefaultUniUnit
instance DefaultUni `Includes` Char            where knownUni = DefaultUniChar
instance a ~ Char => DefaultUni `Includes` [a] where knownUni = DefaultUniString

-- instance a ~ DefaultBuiltinName => DefaultUni `Includes` (Builtin a) where
--     knownUni = DefaultUniBuiltinName

instance GShow DefaultUni where
    gshowsPrec _ DefaultUniInteger    = undefined -- "DefaultUniInteger"
    gshowsPrec _ DefaultUniByteString = undefined -- "DefaultUniByteString"
    gshowsPrec _ DefaultUniString     = undefined -- "DefaultUniString"
    gshowsPrec _ _                    = undefined -- "blah-blah"

instance
        ( constr Integer
        , constr ByteString16
        , constr Bool
        , constr ()
        , constr Char
        , constr String
--         , constr (Builtin DefaultBuiltinName)
        ) => DefaultUni `Everywhere` constr where
    bring _ DefaultUniInteger    = id
    bring _ DefaultUniByteString = id
    bring _ DefaultUniString     = id
    bring _ DefaultUniBool       = id
    bring _ DefaultUniUnit       = id
    bring _ DefaultUniChar       = id
--     bring _ DefaultUniBuiltinName = id

instance Pretty DefaultBuiltinName where
    pretty AddInteger           = "addInteger"
    pretty SubtractInteger      = "subtractInteger"
    pretty MultiplyInteger      = "multiplyInteger"
    pretty DivideInteger        = "divideInteger"
    pretty QuotientInteger      = "quotientInteger"
    pretty ModInteger           = "modInteger"
    pretty RemainderInteger     = "remainderInteger"
    pretty LessThanInteger      = "lessThanInteger"
    pretty LessThanEqInteger    = "lessThanEqualsInteger"
    pretty GreaterThanInteger   = "greaterThanInteger"
    pretty GreaterThanEqInteger = "greaterThanEqualsInteger"
    pretty EqInteger            = "equalsInteger"
    pretty Concatenate          = "concatenate"
    pretty TakeByteString       = "takeByteString"
    pretty DropByteString       = "dropByteString"
    pretty EqByteString         = "equalsByteString"
    pretty LtByteString         = "lessThanByteString"
    pretty GtByteString         = "greaterThanByteString"
    pretty SHA2                 = "sha2_256"
    pretty SHA3                 = "sha3_256"
    pretty VerifySignature      = "verifySignature"

-- type HasDefaultUni uni = DefaultUni `Everywhere` Includes uni

-- type HasDefaultUni uni =
--     ( uni `Includes` Integer
--     , uni `Includes` ByteString16
--     , uni `Includes` Bool
--     , uni `Includes` ()
--     , uni `Includes` Char
--     , uni `Includes` String
--     )
