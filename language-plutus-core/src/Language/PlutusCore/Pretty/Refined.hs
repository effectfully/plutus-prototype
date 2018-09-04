
prettyRefinedType :: PrettyCfg (f a) => Configuration -> Type f a -> Doc ann
prettyRefinedType cfg ty = parens $ cata a ty id where
    a (TyAppF _ t t')     con = con $ t id <+> t' parens
    a (TyVarF _ n)        _   = prettyCfg cfg n
    a (TyFunF _ t t')     con = con $ t parens <+> "->" <+> t' parens
    a (TyFixF _ n t)      con = con $ "fix" <+> prettyCfg cfg n <+> "." <+> t id
    a (TyForallF _ n k t) con = con $ "all" <+> prettyCfg cfg n <+> pretty k <+> "." <+> t id
    a (TyBuiltinF _ n)    _   = pretty n
    a (TyIntF _ n)        _   = pretty n
    a (TyLamF _ n k t)    con =
        con $ "\\" <+> parens (prettyCfg cfg n <+> ":" <+> pretty k) <+> "->" <+> t id
