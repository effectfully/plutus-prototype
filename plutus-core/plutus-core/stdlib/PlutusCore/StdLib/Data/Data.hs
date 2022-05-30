-- | Built-in @pair@ and related functions.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module PlutusCore.StdLib.Data.Data
    ( dataTy
    , caseData
    ) where

import Prelude hiding (uncurry)

import PlutusCore.Core
import PlutusCore.Data
import PlutusCore.Default
import PlutusCore.MkPlc
import PlutusCore.Name
import PlutusCore.Quote

-- | @Data@ as a built-in PLC type.
dataTy :: uni `Contains` Data => Type TyName uni ()
dataTy = mkTyBuiltin @_ @Data ()

-- | Pattern matching over 'Data' inside PLC.
-- TODO: fix
--
-- > \(d : data) ->
-- >     /\(r :: *) ->
-- >      \(fConstr : integer -> list data -> r)
-- >       (fMap : list (pair data data) -> r)
-- >       (fList : list data -> r)
-- >       (fI : integer -> r)
-- >       (fB : bytestring -> r) ->
-- >           chooseData
-- >               d
-- >               {unit -> r}
-- >               (\(u : unit) -> uncurry {integer} {list data} {r} fConstr (unConstrB d))
-- >               (\(u : unit) -> fMap (unMapB d))
-- >               (\(u : unit) -> fList (unListB d))
-- >               (\(u : unit) -> fI (unIB d))
-- >               (\(u : unit) -> fB (unBB d))
-- >               unitval
caseData :: TermLike term TyName Name DefaultUni DefaultFun => term ()
caseData = runQuote $ do
    d <- freshName "d"
    r <- freshTyName "r"
    -- TODO: well, we still want to be lazy, but since every constructor of 'Data' expects at least
    -- one argument, we don't need to lazify them with @unit@.
    return
        . lamAbs () d dataTy
        . tyAbs () r (Type ())
        . apply () (tyInst () (builtin () CaseData) $ TyVar () r)
        $ var () d
