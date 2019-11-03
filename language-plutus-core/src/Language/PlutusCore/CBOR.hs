{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Serialise instances for Plutus Core types. Make sure to read the Note [Stable encoding of PLC]
-- before touching anything in this file.
module Language.PlutusCore.CBOR () where

import           Language.PlutusCore.DeBruijn
import           Language.PlutusCore.Error
import           Language.PlutusCore.Lexer      (AlexPosn)
import           Language.PlutusCore.Lexer.Type hiding (name)
import           Language.PlutusCore.MkPlc      (TyVarDecl (..), VarDecl (..))
import           Language.PlutusCore.Name
import           Language.PlutusCore.Type
import           PlutusPrelude

import           Codec.CBOR.Decoding
import           Codec.CBOR.Encoding
import           Codec.Serialise
import qualified Data.ByteString.Lazy           as BSL
import           Data.Functor.Foldable          hiding (fold)

{- Note [Stable encoding of PLC]
READ THIS BEFORE TOUCHING ANYTHING IN THIS FILE

We need the encoding of PLC on the blockchain to be *extremely* stable. It *must not* change
arbitrarily, otherwise we'll be unable to read back old transactions and validate them.

Consequently we don't use the derivable instances of `Serialise` for the PLC types that go
on the chain.

However, the instances in this file *are* constrained by instances for names, type names,
and annotations. What's to stop the instances for *those* changing, thus changing
the overall encoding on the chain?

The answer is that what goes on the chain is *always* a `Program TyName Name ()`. The instances
for `TyName` and `Name` are nailed down here, and the instance for `()` is standard.

However, having this flexibility allows us to encode e.g. PLC with substantial annotations
(like position information) in situation where the stability is *not* critical, such as
for testing.
-}


instance Serialise BuiltinName where
    encode bi =
        let i = case bi of
                AddInteger           -> 0
                SubtractInteger      -> 1
                MultiplyInteger      -> 2
                DivideInteger        -> 3
                RemainderInteger     -> 4
                LessThanInteger      -> 5
                LessThanEqInteger    -> 6
                GreaterThanInteger   -> 7
                GreaterThanEqInteger -> 8
                EqInteger            -> 9
                Concatenate          -> 10
                TakeByteString       -> 11
                DropByteString       -> 12
                SHA2                 -> 13
                SHA3                 -> 14
                VerifySignature      -> 15
                EqByteString         -> 16
                QuotientInteger      -> 17
                ModInteger           -> 18
                LtByteString         -> 19
                GtByteString         -> 20
        in encodeTag i

    decode = go =<< decodeTag
        where go 0  = pure AddInteger
              go 1  = pure SubtractInteger
              go 2  = pure MultiplyInteger
              go 3  = pure DivideInteger
              go 4  = pure RemainderInteger
              go 5  = pure LessThanInteger
              go 6  = pure LessThanEqInteger
              go 7  = pure GreaterThanInteger
              go 8  = pure GreaterThanEqInteger
              go 9  = pure EqInteger
              go 10 = pure Concatenate
              go 11 = pure TakeByteString
              go 12 = pure DropByteString
              go 13 = pure SHA2
              go 14 = pure SHA3
              go 15 = pure VerifySignature
              go 16 = pure EqByteString
              go 17 = pure QuotientInteger
              go 18 = pure ModInteger
              go 19 = pure LtByteString
              go 20 = pure GtByteString
              go _  = fail "Failed to decode BuiltinName"

instance Serialise Unique where
    encode (Unique i) = encodeInt i
    decode = Unique <$> decodeInt

instance Serialise ann => Serialise (Name ann) where
    -- TODO: should we encode the name or not?
    encode (Name ann txt u) = encode ann <> encode txt <> encode u
    decode = Name <$> decode <*> decode <*> decode

instance Serialise ann => Serialise (TyName ann) where
    encode (TyName n) = encode n
    decode = TyName <$> decode

instance Serialise ann => Serialise (Version ann) where
    encode (Version ann n n' n'') = fold [ encode ann, encode n, encode n', encode n'' ]
    decode = Version <$> decode <*> decode <*> decode <*> decode

instance Serialise ann => Serialise (Kind ann) where
    encode = cata a where
        a (TypeF ann)           = encodeTag 0 <> encode ann
        a (KindArrowF ann k k') = fold [ encodeTag 1, encode ann, k , k' ]

    decode = go =<< decodeTag
        where go 0 = Type <$> decode
              go 1 = KindArrow <$> decode <*> decode <*> decode
              go _ = fail "Failed to decode Kind ()"

instance (Serialise ann, Serialise (tyname ann)) => Serialise (Type tyname uni ann) where
    encode = cata a where
        a (TyVarF ann tn)        = encodeTag 0 <> encode ann <> encode tn
        a (TyFunF ann t t')      = encodeTag 1 <> encode ann <> t <> t'
        a (TyIFixF ann pat arg)  = encodeTag 2 <> encode ann <> pat <> arg
        a (TyForallF ann tn k t) = encodeTag 3 <> encode ann <> encode tn <> encode k <> t
        a (TyConstantF ann con)  = encodeTag 4 <> encode ann <> encode con
        a (TyLamF ann n k t)     = encodeTag 5 <> encode ann <> encode n <> encode k <> t
        a (TyAppF ann t t')      = encodeTag 6 <> encode ann <> t <> t'

    decode = go =<< decodeTag
        where go 0 = TyVar <$> decode <*> decode
              go 1 = TyFun <$> decode <*> decode <*> decode
              go 2 = TyIFix <$> decode <*> decode <*> decode
              go 3 = TyForall <$> decode <*> decode <*> decode <*> decode
              go 4 = TyConstant <$> decode <*> decode
              go 5 = TyLam <$> decode <*> decode <*> decode <*> decode
              go 6 = TyApp <$> decode <*> decode <*> decode
              go _ = fail "Failed to decode Type TyName ()"

instance Serialise DynamicBuiltinName where
    encode (DynamicBuiltinName name) = encode name
    decode = DynamicBuiltinName <$> decode

instance Serialise ann => Serialise (Builtin ann) where
    encode (BuiltinName ann bn)     = encodeTag 0 <> encode ann <> encode bn
    encode (DynBuiltinName ann dbn) = encodeTag 1 <> encode ann <> encode dbn

    decode = go =<< decodeTag
        where go 0 = BuiltinName <$> decode <*> decode
              go 1 = DynBuiltinName <$> decode <*> decode
              go _ = fail "Failed to decode Builtin ()"


-- instance Serialise ann => Serialise (Constant a) where
--     encode (BuiltinInt ann i) = fold [ encodeTag 0, encode x, encodeInteger i ]
--     encode (BuiltinBS ann bs) = fold [ encodeTag 1, encode x, encodeBytes (BSL.toStrict bs) ]
--     encode (BuiltinStr ann s) = encodeTag 2 <> encode ann <> encode s
--     decode = go =<< decodeTag
--         where go 0 = BuiltinInt <$> decode <*> decodeInteger
--               go 1 = BuiltinBS <$> decode <*> fmap BSL.fromStrict decodeBytes
--               go 2 = BuiltinStr <$> decode <*> decode
--               go _ = fail "Failed to decode Constant ()"

instance ( Serialise ann
         , Serialise (tyname ann)
         , Serialise (name ann)
         ) => Serialise (Term tyname name uni ann) where
    encode = cata a where
        a (VarF ann n)           = encodeTag 0 <> encode ann <> encode n
        a (TyAbsF ann tn k t)    = encodeTag 1 <> encode ann <> encode tn <> encode k <> t
        a (LamAbsF ann n ty t)   = encodeTag 2 <> encode ann <> encode n <> encode ty <> t
        a (ApplyF ann t t')      = encodeTag 3 <> encode ann <> t <> t'
        a (ConstantF ann c)      = encodeTag 4 <> encode ann <> encode c
        a (TyInstF ann t ty)     = encodeTag 5 <> encode ann <> t <> encode ty
        a (UnwrapF ann t)        = encodeTag 6 <> encode ann <> t
        a (IWrapF ann pat arg t) = encodeTag 7 <> encode ann <> encode pat <> encode arg <> t
        a (ErrorF ann ty)        = encodeTag 8 <> encode ann <> encode ty
        a (BuiltinF ann bi)      = encodeTag 9 <> encode ann <> encode bi

    decode = go =<< decodeTag
        where go 0 = Var <$> decode <*> decode
              go 1 = TyAbs <$> decode <*> decode <*> decode <*> decode
              go 2 = LamAbs <$> decode <*> decode <*> decode <*> decode
              go 3 = Apply <$> decode <*> decode <*> decode
              go 4 = Constant <$> decode <*> decode
              go 5 = TyInst <$> decode <*> decode <*> decode
              go 6 = Unwrap <$> decode <*> decode
              go 7 = IWrap <$> decode <*> decode <*> decode <*> decode
              go 8 = Error <$> decode <*> decode
              go 9 = Builtin <$> decode <*> decode
              go _ = fail "Failed to decode Term TyName Name ()"

instance ( Serialise ann
         , Serialise (tyname ann)
         , Serialise (name ann)
         ) => Serialise (VarDecl tyname name uni ann) where
    encode (VarDecl t name tyname ) = encode t <> encode name <> encode tyname
    decode = VarDecl <$> decode <*> decode <*> decode

instance (Serialise ann, Serialise (tyname ann))  => Serialise (TyVarDecl tyname ann) where
    encode (TyVarDecl t tyname kind) = encode t <> encode tyname <> encode kind
    decode = TyVarDecl <$> decode <*> decode <*> decode

instance ( Serialise ann
         , Serialise (tyname ann)
         , Serialise (name ann)
         ) => Serialise (Program tyname name uni ann) where
    encode (Program ann v t) = encode ann <> encode v <> encode t
    decode = Program <$> decode <*> decode <*> decode

deriving newtype instance (Serialise a) => Serialise (Normalized a)

instance Serialise a => Serialise (Token a)
instance Serialise AlexPosn
instance Serialise Keyword
instance Serialise Special

deriving instance Serialise Index

instance Serialise ann => Serialise (DeBruijn ann) where
    encode (DeBruijn ann txt i) = encode ann <> encode txt <> encode i
    decode = DeBruijn <$> decode <*> decode <*> decode

instance Serialise ann => Serialise (TyDeBruijn ann) where
    encode (TyDeBruijn n) = encode n
    decode = TyDeBruijn <$> decode

instance (Serialise a) => Serialise (ParseError a)
instance (Serialise (tyname a), Serialise a) => Serialise (ValueRestrictionError tyname a)
instance (Serialise (tyname a), Serialise (name a), Serialise a) => Serialise (NormalizationError tyname name a)
instance (Serialise a) => Serialise (UniqueError a)
instance Serialise UnknownDynamicBuiltinNameError
instance (Serialise a) => Serialise (InternalTypeError a)
instance (Serialise a) => Serialise (TypeError a)
instance (Serialise a) => Serialise (Error a)
