{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.PlutusCore.Pretty.Classic where

import           Data.Functor.Foldable
import           Language.PlutusCore.Lexer.Type
import           Language.PlutusCore.Name
import           Language.PlutusCore.PrettyCfg
import           Language.PlutusCore.Type
import           PlutusPrelude

newtype Classic a = Classic
    { unClassic :: a
    } deriving (Eq, Show, NFData, Pretty)

instance Pretty (Kind (Classic a)) where
    pretty = cata a where
        a TypeF{}             = "(type)"
        a SizeF{}             = "(size)"
        a (KindArrowF _ k k') = parens ("fun" <+> k <+> k')

instance (PrettyCfg (f (Classic a)), PrettyCfg (g (Classic a))) =>
             PrettyCfg (Program f g (Classic a)) where
    prettyCfg cfg (Program _ v t) =
        parens' ("program" <+> pretty v <//> prettyCfg cfg t)

instance Pretty (Constant (Classic a)) where
    pretty (BuiltinInt _ s i) = pretty s <+> "!" <+> pretty i
    pretty (BuiltinSize _ s)  = pretty s
    pretty (BuiltinBS _ s b)  = pretty s <+> "!" <+> prettyBytes b
    pretty (BuiltinName _ n)  = pretty n

instance (PrettyCfg (f (Classic a)), PrettyCfg (g (Classic a))) => PrettyCfg (Term f g (Classic a)) where
    prettyCfg cfg = cata a where
        a (ConstantF _ b)    = parens' ("con" </> pretty b)
        a (ApplyF _ t t')    = brackets' (vsep' [t, t'])
        a (VarF _ n)         = prettyCfg cfg n
        a (TyAbsF _ n k t)   = parens' ("abs" </> vsep' [prettyCfg cfg n, pretty k, t])
        a (TyInstF _ t ty)   = braces' (vsep' [t, prettyCfg cfg ty])
        -- FIXME: only do the </> thing when there's a line break in the `vsep'` part?
        a (LamAbsF _ n ty t) = parens' ("lam" </> vsep' [prettyCfg cfg n, prettyCfg cfg ty, t])
        a (UnwrapF _ t)      = parens' ("unwrap" </> t)
        a (WrapF _ n ty t)   = parens' ("wrap" </> vsep' [prettyCfg cfg n, prettyCfg cfg ty, t])
        a (ErrorF _ ty)      = parens' ("error" </> prettyCfg cfg ty)

instance (PrettyCfg (f (Classic a))) => PrettyCfg (Type f (Classic a)) where
    prettyCfg cfg = cata a where
        a (TyAppF _ t t')     = brackets (t <+> t')
        a (TyVarF _ n)        = prettyCfg cfg n
        a (TyFunF _ t t')     = parens ("fun" <+> t <+> t')
        a (TyFixF _ n t)      = parens ("fix" <+> prettyCfg cfg n <+> t)
        a (TyForallF _ n k t) = parens ("all" <+> prettyCfg cfg n <+> pretty k <+> t)
        a (TyBuiltinF _ n)    = parens ("con" <+> pretty n)
        a (TyIntF _ n)        = parens ("con" <+> pretty n)
        a (TyLamF _ n k t)    = parens ("lam" <+> prettyCfg cfg n <+> pretty k <+> t)

instance PrettyCfg (TyNameWithKind (Classic a)) where
    prettyCfg cfg@(RenderConfig _ True) (TyNameWithKind (TyName tn@(Name (_, k) _ _))) =
        parens (prettyCfg cfg tn <+> ":" <+> pretty k)
    prettyCfg cfg@(RenderConfig _ False) (TyNameWithKind tn) = prettyCfg cfg tn

instance PrettyCfg (NameWithType (Classic a)) where
    prettyCfg cfg@(RenderConfig _ True) (NameWithType n@(Name (_, ty) _ _)) =
        parens (prettyCfg cfg n <+> ":" <+> prettyCfg cfg ty)
    prettyCfg cfg@(RenderConfig _ False) (NameWithType n) = prettyCfg cfg n
