{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

module Language.PlutusCore.Constant.Name
    ( withTypedBuiltinName
    , typedAddInteger
    , typedSubtractInteger
    , typedMultiplyInteger
    , typedDivideInteger
    , typedQuotientInteger
    , typedModInteger
    , typedRemainderInteger
    , typedLessThanInteger
    , typedLessThanEqInteger
    , typedGreaterThanInteger
    , typedGreaterThanEqInteger
    , typedEqInteger
    , typedConcatenate
    , typedTakeByteString
    , typedDropByteString
    , typedSHA2
    , typedSHA3
    , typedVerifySignature
    , typedEqByteString
    , typedLtByteString
    , typedGtByteString
    ) where

import           Language.PlutusCore.Constant.Typed
import           Language.PlutusCore.Constant.Universe
import           Language.PlutusCore.Evaluation.Result
import           Language.PlutusCore.Lexer.Type

import qualified Data.ByteString.Lazy.Char8                     as BSL
import           Data.Proxy

-- | Apply a continuation to the typed version of a 'BuiltinName'.
withTypedBuiltinName
    :: uni `Includes` Integer => BuiltinName -> (forall a r. TypedBuiltinName uni a r -> c) -> c
withTypedBuiltinName AddInteger           k = k typedAddInteger
withTypedBuiltinName SubtractInteger      k = k typedSubtractInteger
withTypedBuiltinName MultiplyInteger      k = k typedMultiplyInteger
withTypedBuiltinName DivideInteger        k = k typedDivideInteger
withTypedBuiltinName QuotientInteger      k = k typedQuotientInteger
withTypedBuiltinName RemainderInteger     k = k typedRemainderInteger
withTypedBuiltinName ModInteger           k = k typedModInteger
withTypedBuiltinName LessThanInteger      k = k typedLessThanInteger
withTypedBuiltinName LessThanEqInteger    k = k typedLessThanEqInteger
withTypedBuiltinName GreaterThanInteger   k = k typedGreaterThanInteger
withTypedBuiltinName GreaterThanEqInteger k = k typedGreaterThanEqInteger
withTypedBuiltinName EqInteger            k = k typedEqInteger
withTypedBuiltinName Concatenate          k = k typedConcatenate
withTypedBuiltinName TakeByteString       k = k typedTakeByteString
withTypedBuiltinName DropByteString       k = k typedDropByteString
withTypedBuiltinName SHA2                 k = k typedSHA2
withTypedBuiltinName SHA3                 k = k typedSHA3
withTypedBuiltinName VerifySignature      k = k typedVerifySignature
withTypedBuiltinName EqByteString         k = k typedEqByteString
withTypedBuiltinName LtByteString         k = k typedLtByteString
withTypedBuiltinName GtByteString         k = k typedGtByteString

oneArg
    :: (uni `Includes` a, uni `Includes` b)
    => TypeScheme uni (a -> b) b
oneArg =
    Proxy `TypeSchemeArrow` TypeSchemeResult Proxy

twoArgs
    :: (uni `Includes` a, uni `Includes` b, uni `Includes` c)
    => TypeScheme uni (a -> b -> c) c
twoArgs =
    Proxy `TypeSchemeArrow` Proxy `TypeSchemeArrow` TypeSchemeResult Proxy

threeArgs
    :: (uni `Includes` a, uni `Includes` b, uni `Includes` c, uni `Includes` d)
    => TypeScheme uni (a -> b -> c -> d) d
threeArgs =
    Proxy `TypeSchemeArrow` Proxy `TypeSchemeArrow` Proxy `TypeSchemeArrow` TypeSchemeResult Proxy

-- | Typed 'AddInteger'.
typedAddInteger
    :: uni `Includes` Integer => TypedBuiltinName uni (Integer -> Integer -> Integer) Integer
typedAddInteger = TypedBuiltinName AddInteger twoArgs

-- | Typed 'SubtractInteger'.
typedSubtractInteger
    :: uni `Includes` Integer => TypedBuiltinName uni (Integer -> Integer -> Integer) Integer
typedSubtractInteger = TypedBuiltinName SubtractInteger twoArgs

-- | Typed 'MultiplyInteger'.
typedMultiplyInteger
    :: uni `Includes` Integer => TypedBuiltinName uni (Integer -> Integer -> Integer) Integer
typedMultiplyInteger = TypedBuiltinName MultiplyInteger twoArgs

-- | Typed 'DivideInteger'.
typedDivideInteger
    :: uni `Includes` Integer => TypedBuiltinName uni (Integer -> Integer -> EvaluationResult Integer) (EvaluationResult Integer)
typedDivideInteger = TypedBuiltinName DivideInteger twoArgs

-- | Typed 'QuotientInteger'
typedQuotientInteger
    :: uni `Includes` Integer => TypedBuiltinName uni (Integer -> Integer -> EvaluationResult Integer) (EvaluationResult Integer)
typedQuotientInteger = TypedBuiltinName QuotientInteger twoArgs

-- | Typed 'RemainderInteger'.
typedRemainderInteger
    :: uni `Includes` Integer => TypedBuiltinName uni (Integer -> Integer -> EvaluationResult Integer) (EvaluationResult Integer)
typedRemainderInteger = TypedBuiltinName RemainderInteger twoArgs

-- | Typed 'ModInteger'
typedModInteger
    :: uni `Includes` Integer => TypedBuiltinName uni (Integer -> Integer -> EvaluationResult Integer) (EvaluationResult Integer)
typedModInteger = TypedBuiltinName ModInteger twoArgs

-- | Typed 'LessThanInteger'.
typedLessThanInteger
    :: uni `Includes` Integer => TypedBuiltinName uni (Integer -> Integer -> Bool) Bool
typedLessThanInteger = TypedBuiltinName LessThanInteger twoArgs

-- | Typed 'LessThanEqInteger'.
typedLessThanEqInteger
    :: uni `Includes` Integer => TypedBuiltinName uni (Integer -> Integer -> Bool) Bool
typedLessThanEqInteger = TypedBuiltinName LessThanEqInteger twoArgs

-- | Typed 'GreaterThanInteger'.
typedGreaterThanInteger
    :: uni `Includes` Integer => TypedBuiltinName uni (Integer -> Integer -> Bool) Bool
typedGreaterThanInteger = TypedBuiltinName GreaterThanInteger twoArgs

-- | Typed 'GreaterThanEqInteger'.
typedGreaterThanEqInteger
    :: uni `Includes` Integer => TypedBuiltinName uni (Integer -> Integer -> Bool) Bool
typedGreaterThanEqInteger = TypedBuiltinName GreaterThanEqInteger twoArgs

-- | Typed 'EqInteger'.
typedEqInteger
    :: uni `Includes` Integer => TypedBuiltinName uni (Integer -> Integer -> Bool) Bool
typedEqInteger = TypedBuiltinName EqInteger twoArgs

-- | Typed 'Concatenate'.
typedConcatenate
    :: uni `Includes` Integer => TypedBuiltinName uni (BSL.ByteString -> BSL.ByteString -> BSL.ByteString) BSL.ByteString
typedConcatenate = TypedBuiltinName Concatenate twoArgs

-- | Typed 'TakeByteString'.
typedTakeByteString
    :: uni `Includes` Integer => TypedBuiltinName uni (Integer -> BSL.ByteString -> BSL.ByteString) BSL.ByteString
typedTakeByteString = TypedBuiltinName TakeByteString twoArgs

-- | Typed 'DropByteString'.
typedDropByteString
    :: uni `Includes` Integer => TypedBuiltinName uni (Integer -> BSL.ByteString -> BSL.ByteString) BSL.ByteString
typedDropByteString = TypedBuiltinName DropByteString twoArgs

-- | Typed 'SHA2'.
typedSHA2
    :: uni `Includes` Integer => TypedBuiltinName uni (BSL.ByteString -> BSL.ByteString) BSL.ByteString
typedSHA2 = TypedBuiltinName SHA2 oneArg

-- | Typed 'SHA3'.
typedSHA3
    :: uni `Includes` Integer => TypedBuiltinName uni (BSL.ByteString -> BSL.ByteString) BSL.ByteString
typedSHA3 = TypedBuiltinName SHA3 oneArg

-- | Typed 'VerifySignature'.
typedVerifySignature
    :: uni `Includes` Integer => TypedBuiltinName uni (BSL.ByteString -> BSL.ByteString -> BSL.ByteString -> EvaluationResult Bool) (EvaluationResult Bool)
typedVerifySignature = TypedBuiltinName VerifySignature threeArgs

-- | Typed 'EqByteString'.
typedEqByteString
    :: uni `Includes` Integer => TypedBuiltinName uni (BSL.ByteString -> BSL.ByteString -> Bool) Bool
typedEqByteString = TypedBuiltinName EqByteString twoArgs

-- | Typed 'LtByteString'.
typedLtByteString
    :: uni `Includes` Integer => TypedBuiltinName uni (BSL.ByteString -> BSL.ByteString -> Bool) Bool
typedLtByteString = TypedBuiltinName LtByteString twoArgs

-- | Typed 'GtByteString'.
typedGtByteString
    :: uni `Includes` Integer => TypedBuiltinName uni (BSL.ByteString -> BSL.ByteString -> Bool) Bool
typedGtByteString = TypedBuiltinName GtByteString twoArgs
