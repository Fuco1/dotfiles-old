module StackSetExtra
       ( withScreen
       , withOtherScreen
       , shiftAndView
       , shiftAndViewAtOther
       , shiftToOtherScreen
       ) where

import Control.Monad (liftM2)

import XMonad
import qualified XMonad.StackSet as W

-- | Modify the current window list with a pure function in context of a named
-- screen, then refresh.  See also 'XMonad.Operations.windows'.
withScreen :: ScreenId -- ^ ID of the target screen.  If such doesn't exist, this operation is NOOP
              -> (WorkspaceId -> WindowSet -> WindowSet)
              -> X ()
withScreen n f = screenWorkspace n >>= flip whenJust (windows . f)

-- | Transform pure function to one running in context of the other screen.  The
-- function takes 'WorkspaceId' of the workspace current on the other screen.
-- This only works in case of two physical displays.
withOtherScreen :: (WorkspaceId -> WindowSet -> WindowSet) -> WindowSet -> WindowSet
withOtherScreen f st = case W.visible st of
  []      -> st
  [other] -> let ws = W.tag . W.workspace $ other in f ws st
  _       -> st


-- | Shift the current active window to specified workspace, then select it on
-- the current screen.
shiftAndView :: WorkspaceId -> WindowSet -> WindowSet
shiftAndView = liftM2 (.) W.greedyView W.shift

-- For those wondering, liftM2 (.) f g = \x -> f x . g x

-- | Shift the current active window to specified workspace, then select it on
-- the other screen.
shiftAndViewAtOther :: WorkspaceId -> WindowSet -> WindowSet
shiftAndViewAtOther wid = withOtherScreen (\ows -> W.view wid . W.view ows . W.shift wid)

-- | Shift the current active window to the workspace active on the other screen
-- and select it immediately.
shiftToOtherScreen :: WindowSet -> WindowSet
shiftToOtherScreen = withOtherScreen $ liftM2 (.) W.view W.shift
