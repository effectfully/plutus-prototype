{-# LANGUAGE OverloadedStrings #-}

module Language.PlutusCore.Pretty.Classic where

import qualified Data.ByteString.Lazy           as BSL
import           Data.Functor.Foldable
import           Language.PlutusCore.Lexer.Type
import           Language.PlutusCore.PrettyCfg
import           Language.PlutusCore.Type
import           PlutusPrelude

prettyClassicKind :: Kind a -> Doc ann
prettyClassicKind = cata a where
    a TypeF{}             = "(type)"
    a SizeF{}             = "(size)"
    a (KindArrowF _ k k') = parens ("fun" <+> k <+> k')

instance (PrettyCfg (f a), PrettyCfg (g a)) => PrettyCfg (Program f g a) where
    prettyCfg cfg (Program _ v t) = parens' ("program" <+> pretty v <//> prettyCfg cfg t)

instance PrettyCfg (Constant a) where
    prettyCfg _ (BuiltinInt _ s i)  = pretty s <+> "!" <+> pretty i
    prettyCfg _ (BuiltinSize _ s)   = pretty s
    prettyCfg _ (BuiltinBS _ s b)   = pretty s <+> "!" <+> prettyBytes b
    prettyCfg cfg (BuiltinName _ n) = prettyCfg cfg n

instance (PrettyCfg (f a), PrettyCfg (g a)) => PrettyCfg (Term f g a) where
    prettyCfg cfg = cata a where
        a (ConstantF _ b)    = parens' ("con" </> prettyCfg cfg b)
        a (ApplyF _ t t')    = brackets' (vsep' [t, t'])
        a (VarF _ n)         = prettyCfg cfg n
        a (TyAbsF _ n k t)   = parens' ("abs" </> vsep' [prettyCfg cfg n, pretty k, t])
        a (TyInstF _ t ty)   = braces' (vsep' [t, prettyCfg cfg ty])
        a (LamAbsF _ n ty t) = parens' ("lam" </> vsep' [prettyCfg cfg n, prettyCfg cfg ty, t]) -- FIXME: only do the </> thing when there's a line break in the `vsep'` part?
        a (UnwrapF _ t)      = parens' ("unwrap" </> t)
        a (WrapF _ n ty t)   = parens' ("wrap" </> vsep' [prettyCfg cfg n, prettyCfg cfg ty, t])
        a (ErrorF _ ty)      = parens' ("error" </> prettyCfg cfg ty)

instance (PrettyCfg (f a)) => PrettyCfg (Type f a) where
    prettyCfg cfg = cata a where
        a (TyAppF _ t t')     = brackets (t <+> t')
        a (TyVarF _ n)        = prettyCfg cfg n
        a (TyFunF _ t t')     = parens ("fun" <+> t <+> t')
        a (TyFixF _ n t)      = parens ("fix" <+> prettyCfg cfg n <+> t)
        a (TyForallF _ n k t) = parens ("all" <+> prettyCfg cfg n <+> pretty k <+> t)
        a (TyBuiltinF _ n)    = parens ("con" <+> pretty n)
        a (TyIntF _ n)        = parens ("con" <+> pretty n)
        a (TyLamF _ n k t)    = parens ("lam" <+> prettyCfg cfg n <+> pretty k <+> t)
