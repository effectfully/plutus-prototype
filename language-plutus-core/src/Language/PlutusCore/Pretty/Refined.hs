{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.PlutusCore.Pretty.Classic where

import qualified Data.ByteString.Lazy           as BSL
import           Data.Functor.Foldable
import           Language.PlutusCore.Lexer.Type
import           Language.PlutusCore.PrettyCfg
import           Language.PlutusCore.Type
import           PlutusPrelude

-- data RenderContext
--     = RenderInner
--     | RenderOuter

data RenderInner  -- id
data RenderOuter  -- parens

-- renderInner :: f a

newtype Refined rc a = Refined
    { unRefined :: a
    }

instance Pretty a => Pretty (Refined RenderInner a) where
    pretty (Refined x) = pretty x

instance Pretty a => Pretty (Refined RenderOuter a) where
    pretty (Refined x) = parens' $ pretty x

-- instance Pretty (Kind (Refined a)) where
--     pretty kind = cata a kind id where
--         a TypeF{}             con = "*"
--         a SizeF{}             con = "size"
--         a (KindArrowF _ k k') con = con $ k parens <+> "->" + k id

-- instance (PrettyCfg (Refined (f a)), PrettyCfg (Refined (g a))) => PrettyCfg (Refined (Program f g a)) where
--     prettyCfg cfg (Program _ v t) = parens' ("program" <+> pretty v <//> prettyCfg cfg t)

-- instance PrettyCfg (Refined (Constant a)) where
--     prettyCfg _ (BuiltinInt _ s i)  = pretty s <+> "!" <+> pretty i
--     prettyCfg _ (BuiltinSize _ s)   = pretty s
--     prettyCfg _ (BuiltinBS _ s b)   = pretty s <+> "!" <+> prettyBytes b
--     prettyCfg cfg (BuiltinName _ n) = prettyCfg cfg n

-- instance (PrettyCfg (Refined (f a)), PrettyCfg (Refined (g a))) => PrettyCfg (Refined (Term f g a)) where
--     prettyCfg cfg = cata a where
--         a (ConstantF _ b)    = parens' ("con" </> prettyCfg cfg b)
--         a (ApplyF _ t t')    = brackets' (vsep' [t, t'])
--         a (VarF _ n)         = prettyCfg cfg n
--         a (TyAbsF _ n k t)   = parens' ("abs" </> vsep' [prettyCfg cfg n, pretty k, t])
--         a (TyInstF _ t ty)   = braces' (vsep' [t, prettyCfg cfg ty])
--         a (LamAbsF _ n ty t) = parens' ("lam" </> vsep' [prettyCfg cfg n, prettyCfg cfg ty, t]) -- FIXME: only do the </> thing when there's a line break in the `vsep'` part?
--         a (UnwrapF _ t)      = parens' ("unwrap" </> t)
--         a (WrapF _ n ty t)   = parens' ("wrap" </> vsep' [prettyCfg cfg n, prettyCfg cfg ty, t])
--         a (ErrorF _ ty)      = parens' ("error" </> prettyCfg cfg ty)

-- instance (PrettyCfg (Refined (f a))) => PrettyCfg (Refined (Type f a)) where
--     prettyCfg cfg = parens $ cata a ty id where
--        a (TyAppF _ t t')     con = con $ t id <+> t' parens
--        a (TyVarF _ n)        _   = prettyCfg cfg n
--        a (TyFunF _ t t')     con = con $ t parens <+> "->" <+> t' parens
--        a (TyFixF _ n t)      con = con $ "fix" <+> prettyCfg cfg n <+> "." <+> t id
--        a (TyForallF _ n k t) con = con $ "all" <+> prettyCfg cfg n <+> pretty k <+> "." <+> t id
--        a (TyBuiltinF _ n)    _   = pretty n
--        a (TyIntF _ n)        _   = pretty n
--        a (TyLamF _ n k t)    con =
--            con $ "\\" <+> parens (prettyCfg cfg n <+> ":" <+> pretty k) <+> "->" <+> t id

-- instance PrettyCfg (Refined (TyNameWithKind a)) where
--     prettyCfg cfg@(Configuration _ True) (TyNameWithKind (TyName tn@(Name (_, k) _ _))) = parens (prettyCfg cfg tn <+> ":" <+> pretty k)
--     prettyCfg cfg@(Configuration _ False) (TyNameWithKind tn) = prettyCfg cfg tn

-- instance PrettyCfg (Refined (NameWithType a)) where
--     prettyCfg cfg@(Configuration _ True) (NameWithType n@(Name (_, ty) _ _)) = parens (prettyCfg cfg n <+> ":" <+> prettyCfg cfg ty)
--     prettyCfg cfg@(Configuration _ False) (NameWithType n) = prettyCfg cfg n
