module Language.UntypedPlutusCore where

import           Language.PlutusCore.Core     as Export (Builtin (..), BuiltinName (..), DynamicBuiltinName (..))
import           Language.PlutusCore.Universe

data Term ann name uni
    = Constant ann (Some (ValueOf uni))
    | Builtin ann (Builtin ann)
    | Var ann
    | LamAbs ann name (Term ann name uni)
    | Apply ann (Term ann name uni) (Term ann name uni)
    | Delay ann (Term ann name uni)
    | Force ann (Term ann name uni)
    | Error ann
