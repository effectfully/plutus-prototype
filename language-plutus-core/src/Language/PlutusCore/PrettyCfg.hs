-- | This module contains the 'PrettyCfg' typeclass, a more sophisticated
-- typeclass for pretty-printing that allows us to dump debug information only
-- when wanted.
{-# LANGUAGE DefaultSignatures #-}
module Language.PlutusCore.PrettyCfg ( PrettyCfg (..)
                                     , RenderConfig (..)
                                     -- * Helper functions
                                     , prettyCfgString
                                     , prettyCfgText
                                     , debugText
                                     , defaultCfg
                                     , debugCfg
                                     , renderCfg
                                     ) where

import qualified Data.Text                               as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.String (renderString)
import           PlutusPrelude

newtype RenderDebug = RenderDebug
    { unRenderDebug :: Bool
    }

data RenderConfigClassic = RenderConfigClassic
    { _renderConfigDebug      :: RenderDebug
    , _renderConfigAnnotation :: Bool
    }

newtype RenderConfigRefined = RenderConfigRefined
    { _renderConfigRefinedDebug :: RenderDebug
    }

data RenderConfig
    = RenderConfigClassic RenderConfigClassic
    | RenderConfigRefined RenderConfigRefined

class PrettyCfg cfg a where
    prettyCfg :: cfg -> a -> Doc ann
    default prettyCfg :: Pretty a => cfg -> a -> Doc ann
    prettyCfg _ = pretty

instance PrettyCfg Bool
instance PrettyCfg Integer

instance PrettyCfg a => PrettyCfg [a] where
    prettyCfg cfg = list . fmap (prettyCfg cfg)

renderCfg :: PrettyCfg a => RenderConfig -> a -> T.Text
renderCfg = render .* prettyCfg

-- | Render a 'Program' as strict 'Text', using 'defaultCfg'
prettyCfgText :: PrettyCfg a => a -> T.Text
prettyCfgText = render . prettyCfg defaultCfg

defaultCfg :: RenderConfig
defaultCfg = RenderConfig False False False

debugCfg :: RenderConfig
debugCfg = RenderConfig True False True

debugText :: PrettyCfg a => a -> T.Text
debugText = render . prettyCfg debugCfg

prettyCfgString :: PrettyCfg a => a -> String
prettyCfgString = renderString . layoutPretty defaultLayoutOptions . prettyCfg defaultCfg
