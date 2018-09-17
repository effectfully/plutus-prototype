{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.PlutusCore.Pretty.Refined where

import           Data.Text.Prettyprint.Doc.Internal (enclose)
import           Language.PlutusCore.Lexer.Type     hiding (builtin, int, name)
import           Language.PlutusCore.PrettyCfg
import           Language.PlutusCore.Type
import           PlutusPrelude

data DocComplexity
    = ElementaryDoc
    | CompositionalDoc

data DocWithComplexity ann = DocWithComplexity (Doc ann) DocComplexity

data RenderEnclosed
    = RenderEnclosedParens
    | RenderEnclosedBraces

data RenderContext
    = RenderBot
    | RenderEnclosed RenderEnclosed
    | RenderTop

elementaryDoc :: Doc ann -> DocWithComplexity ann
elementaryDoc doc = DocWithComplexity doc ElementaryDoc

compositionalDoc :: Doc ann -> DocWithComplexity ann
compositionalDoc doc = DocWithComplexity doc CompositionalDoc

renderEnclosed :: RenderEnclosed -> Doc ann -> Doc ann
renderEnclosed RenderEnclosedParens = parens
renderEnclosed RenderEnclosedBraces = enclose "{" "}"

renderByDocComplexity :: RenderContext -> DocComplexity -> Doc ann -> Doc ann
renderByDocComplexity RenderBot           _                = id
renderByDocComplexity (RenderEnclosed _ ) ElementaryDoc    = id
renderByDocComplexity (RenderEnclosed re) CompositionalDoc = renderEnclosed re
renderByDocComplexity RenderTop           _                = parens

renderDocWithComplexity :: RenderContext -> DocWithComplexity ann -> Doc ann
renderDocWithComplexity renderContext (DocWithComplexity doc docComplexity) =
    renderByDocComplexity renderContext docComplexity doc

inBot :: DocWithComplexity ann -> Doc ann
inBot = renderDocWithComplexity RenderBot

inParens :: DocWithComplexity ann -> Doc ann
inParens = renderDocWithComplexity $ RenderEnclosed RenderEnclosedParens

inBraces :: DocWithComplexity ann -> Doc ann
inBraces = renderDocWithComplexity $ RenderEnclosed RenderEnclosedBraces

inTop :: DocWithComplexity ann -> Doc ann
inTop = renderDocWithComplexity RenderTop

newtype Refined a = Refined
    { unRefined :: a
    }

instance Pretty (Kind (Refined a)) where
    pretty = inTop . go where
        go Type{}            = elementaryDoc "*"
        go Size{}            = elementaryDoc "size"
        go (KindArrow _ k l) = compositionalDoc $ inParens (go k) <+> "->" <+> inBot (go l)

-- instance (PrettyCfg (f (Refined a)), PrettyCfg (g (Refined a))) => PrettyCfg (Program f g (Refined a)) where
--     prettyCfg cfg (Program _ v t) = parens' ("program" <+> pretty v <//> prettyCfg cfg t)

instance Pretty (Constant (Refined a)) where
    pretty = inTop . elementaryDoc . go where
        go (BuiltinInt _ size int) = pretty size <+> "!" <+> pretty int
        go (BuiltinSize _ size)    = pretty size
        go (BuiltinBS _ size bs)   = pretty size <+> "!" <+> prettyBytes bs
        go (BuiltinName _ name)    = pretty name

instance (PrettyCfg (f (Refined a))) => PrettyCfg (Type f (Refined a)) where
    prettyCfg cfg = inTop . go where
       go (TyApp _ fun arg)         = compositionalDoc $ inBot (go fun) <+> inParens (go arg)
       go (TyVar _ name)            = elementaryDoc $ prettyCfg cfg name
       go (TyFun _ tyIn tyOut)      = compositionalDoc $ inParens (go tyIn) <+> "->" <+> inBot (go tyOut)
       go (TyFix _ name body)       = compositionalDoc $
           "fix" <+> prettyCfg cfg name <> "." <+> inBot (go body)
       go (TyForall _ name kind ty) = compositionalDoc $
           "all" <+> parens (prettyCfg cfg name <+> "::" <+> pretty kind) <+> "." <+> inBot (go ty)
       go (TyBuiltin _ builtin)     = compositionalDoc $ pretty builtin
       go (TyInt _ size)            = elementaryDoc $ pretty size
       go (TyLam _ name kind ty)    = compositionalDoc $
           "\\" <+> parens (prettyCfg cfg name <+> "::" <+> pretty kind) <+> "->" <+> inBot (go ty)

instance (PrettyCfg (f (Refined a)), PrettyCfg (g (Refined a))) => PrettyCfg (Term f g (Refined a)) where
    prettyCfg cfg = inTop . go where
        go (Constant _ con)         = elementaryDoc $ pretty con
        go (Apply _ fun arg)        = compositionalDoc $ inBot (go fun) <+> inParens (go arg)
        go (Var _ name)             = elementaryDoc $ prettyCfg cfg name
        go (TyAbs _ name kind body) = compositionalDoc $
            "/\\" <> parens (prettyCfg cfg name <+> ":" <+> pretty kind) <+> "->" <+> inBot (go body)
        go (TyInst _ fun ty)        = compositionalDoc $
            inBot (go fun) <+> inBraces (elementaryDoc $ prettyCfg cfg ty)
        go (LamAbs _ name ty body)  = compositionalDoc $
            "\\" <> parens (prettyCfg cfg name <+> ":" <+> prettyCfg cfg ty) <+> "->" <+> inBot (go body)
        go (Unwrap _ term)          = compositionalDoc $ "unwrap" <+> inParens (go term)
        go (Wrap _ name ty term)    = compositionalDoc $
            "wrap" <+> prettyCfg cfg name <+> prettyCfg cfg ty <+> inParens (go term)
        go (Error _ ty)             = compositionalDoc $
            "error" <+> inBraces (elementaryDoc $ prettyCfg cfg ty)

-- instance PrettyCfg (Refined (TyNameWithKind a)) where
--     prettyCfg cfg@(Configuration _ True) (TyNameWithKind (TyName tn@(Name (_, k) _ _))) = parens (prettyCfg cfg tn <+> ":" <+> pretty k)
--     prettyCfg cfg@(Configuration _ False) (TyNameWithKind tn) = prettyCfg cfg tn

-- instance PrettyCfg (Refined (NameWithType a)) where
--     prettyCfg cfg@(Configuration _ True) (NameWithType n@(Name (_, ty) _ _)) = parens (prettyCfg cfg n <+> ":" <+> prettyCfg cfg ty)
--     prettyCfg cfg@(Configuration _ False) (NameWithType n) = prettyCfg cfg n






-- prettyString $ KindArrow (Refined ()) (KindArrow (Refined ()) (Type (Refined ())) (Type (Refined ()))) (Type (Refined ()))


-- instance Pretty a => Pretty (Refined RenderPlain a) where
--     pretty (Refined x) = pretty x

-- instance Pretty a => Pretty (Refined RenderParens a) where
--     pretty (Refined x) = parens $ pretty x

-- instance Pretty a => Pretty (Refined RenderParensAlign a) where
--     pretty (Refined x) = parens' $ pretty x

-- refinedAs
--     :: Coercible (f (Refined render a)) (f (Refined render' a))
--     => render' -> f (Refined render a) -> f (Refined render' a)
-- refinedAs _ = coerce

-- -- refinedAs :: render -> Refined render' a -> Refined render a
-- -- refinedAs _ = coerce

-- -- prettyRefined :: f (Refined render a) -> (f a -> Doc ann) -> Doc ann
-- -- prettyRefined = undefined

-- prettyRefined :: render -> Doc ann -> Doc ann
-- prettyRefined = undefined

-- -- k :: Doc ann
-- -- k :: Doc (Refined render ann)

-- -- coerceAnn :: f a -> f b

-- instance Pretty (Kind (Refined render a)) where
--     -- pretty kind = prettyRefined $ cata a kind where
--     --     a TypeF{}             = "*"       -- Outer  Inner
--                                              -- Parens Plain -> id
--                                              -- Top    Plain -> parens
--     --     a SizeF{}             = "size"
--     --     a (KindArrowF _ k k') = coerce $ refinedAs RenderParens k <+> "->" <+> refinedAs RenderPlain k'
--                                              -- Top    Parens -> parens
--                                              -- Parens Parens -> parens
--                                              -- Plain  Parens -> id
--     pretty Type{}            = undefined  -- prettyRefined RenderPlain "*" :: Doc a
--                                --                                          :: Doc a
--     pretty Size{}            = undefined
--     pretty (KindArrow _ k l) = undefined
