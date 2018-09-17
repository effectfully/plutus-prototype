{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DeriveAnyClass          #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE UndecidableInstances    #-}

module Language.PlutusCore.Error ( Error (..)
                                 , NormalizationError (..)
                                 , RenameError (..)
                                 , TypeError (..)
                                 , IsError (..)
                                 ) where

import           Language.PlutusCore.Lexer
import           Language.PlutusCore.Name
import           Language.PlutusCore.PrettyCfg
import           Language.PlutusCore.Type
import           PlutusPrelude

import qualified Data.Text                     as T

data NormalizationError tyname name a = BadType a (Type tyname a) T.Text
                                      | BadTerm a (Term tyname name a) T.Text
                                      deriving (Show, Eq, Functor, Generic, NFData)

instance (Pretty a, PrettyCfg (Type tyname a), PrettyCfg (Term tyname name a)) =>
        PrettyCfg (NormalizationError tyname name a) where
    prettyCfg cfg (BadType l ty expct) = "Malformed type at" <+> pretty l <> ". Type" <+> squotes (prettyCfg cfg ty) <+> "is not a" <+> pretty expct <> "."
    prettyCfg cfg (BadTerm l t expct) = "Malformed term at" <+> pretty l <> ". Term" <+> squotes (prettyCfg cfg t) <+> "is not a" <+> pretty expct <> "."


-- | A 'RenameError' is thrown when a free variable is encountered during
-- rewriting.
data RenameError a = UnboundVar (Name a)
                   | UnboundTyVar (TyName a)
                   deriving (Show, Eq, Functor, Generic, NFData)

instance Pretty a => PrettyCfg (RenameError a) where
    prettyCfg cfg (UnboundVar n@(Name loc _ _)) = "Error at" <+> pretty loc <> ". Variable" <+> prettyCfg cfg n <+> "is not in scope."
    prettyCfg cfg (UnboundTyVar n@(TyName (Name loc _ _))) = "Error at" <+> pretty loc <> ". Type variable" <+> prettyCfg cfg n <+> "is not in scope."

data TypeError a = InternalError -- ^ This is thrown if builtin lookup fails
                 | KindMismatch a (Type TyNameWithKind a) (Kind a) (Kind a)
                 | TypeMismatch a (Term TyNameWithKind NameWithType a) (Type TyNameWithKind a) (Type TyNameWithKind a)
                 | OutOfGas
                 deriving (Show, Eq, Functor, Generic, NFData)

instance (Pretty a, Pretty (Kind a), PrettyCfg (Type TyNameWithKind a), PrettyCfg (Term TyNameWithKind NameWithType a)) => PrettyCfg (TypeError a) where
    prettyCfg _ InternalError               = "Internal error."
    prettyCfg cfg (KindMismatch x ty k k')  = "Kind mismatch at" <+> pretty x <+> "in type" <+> squotes (prettyCfg cfg ty) <> ". Expected kind" <+> squotes (pretty k) <+> ", found kind" <+> squotes (pretty k')
    prettyCfg cfg (TypeMismatch x t ty ty') = "Type mismatch at" <+> pretty x <+> "in term" <> hardline <> indent 2 (squotes (prettyCfg cfg t)) <> "." <> hardline <> "Expected type" <> hardline <> indent 2 (squotes (prettyCfg cfg ty)) <> "," <> hardline <> "found type" <> hardline <> indent 2 (squotes (prettyCfg cfg ty'))
    prettyCfg _ OutOfGas                    = "Type checker ran out of gas."

data Error a = ParseError (ParseError a)
             | RenameError (RenameError a)
             | TypeError (TypeError a)
             | NormalizationError (NormalizationError TyName Name a)
             deriving (Show, Eq, Functor, Generic, NFData)

class IsError f where

    asError :: f a -> Error a

    asLeft :: f a -> Either (Error a) b
    asLeft = Left . asError

    convertError :: Either (f a) b -> Either (Error a) b
    convertError = first asError

    collectErrors :: (IsError g) => Either (f a) (Either (g a) b) -> Either (Error a) b
    collectErrors (Left x)          = asLeft x
    collectErrors (Right (Left x))  = asLeft x
    collectErrors (Right (Right x)) = Right x

instance IsError Error where
    asError = id

instance IsError ParseError where
    asError = ParseError

instance IsError RenameError where
    asError = RenameError

instance IsError TypeError where
    asError = TypeError

instance IsError (NormalizationError TyName Name) where
    asError = NormalizationError

instance (Pretty a, Pretty (Kind a), PrettyCfg (Term TyNameWithKind NameWithType a), PrettyCfg (Term TyName Name a), PrettyCfg (Type TyNameWithKind a), PrettyCfg (Type TyName a)) => PrettyCfg (Error a) where
    prettyCfg cfg (ParseError e)         = prettyCfg cfg e
    prettyCfg cfg (RenameError e)        = prettyCfg cfg e
    prettyCfg cfg (TypeError e)          = prettyCfg cfg e
    prettyCfg cfg (NormalizationError e) = prettyCfg cfg e
