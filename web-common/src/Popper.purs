module Popper
  ( module Popper.Types
  , module ExportedInternal
  , defaultModifiers
  , defaultPreventOverflow
  ) where

import Popper.Types
import Prelude
import Popper.Internal (createPopper, forceUpdate, destroyPopper, arrow, computeStyles, applyStyles, eventListeners, popperOffsets, offset, preventOverflow) as ExportedInternal

defaultModifiers :: Array Modifier
defaultModifiers =
  [ ExportedInternal.computeStyles
      { gpuAcceleration: true
      , adaptive: true
      , roundOffsets: true
      }
  , ExportedInternal.applyStyles
  , ExportedInternal.eventListeners
      { scroll: true
      , resize: true
      }
  , ExportedInternal.popperOffsets
  ]

defaultPreventOverflow :: PreventOverflowOptions
defaultPreventOverflow =
  { mainAxis: true
  , altAxis: false
  , padding: pAll 0.0
  , boundary: ClippinParents
  , altBoundary: false
  , rootBoundary: ViewportBoundary
  , tether: true
  , tetherOffset: TetherOffset 0.0
  }
