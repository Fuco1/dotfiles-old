module Constants where

import XMonad
import XMonad.Actions.CopyWindow (copyToAll)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Prompt
import XMonad.Util.Loggers

import Utils
import Workspaces

prompt :: XPConfig
prompt = defaultXPConfig { font = "xft:Monospace-12:narrow"
                         , bgColor = "black"
                         , fgColor = "#888a85"
                         , fgHLight = "#eeeeec"
                         , bgHLight = "#3465a4"
                         , borderColor = "#008800"
                         , promptKeymap = emacsLikeXPKeymap
                         , height = 25
                         , searchPredicate = subseq
                         , alwaysHighlight = True
                         }

workspaces :: [WorkspaceId]
workspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0",
              "q", "w", "e", "r"]

workspaceKeys = [xK_1 .. xK_9] ++ [xK_0, xK_q, xK_w, xK_e, xK_r]

-- | Just like 'withWorkspaces' but supply default workspace and key lists.
withWorkspacesD :: ButtonMask -> (WorkspaceId -> WindowSet -> WindowSet) -> [((ButtonMask, KeySym), X ())]
withWorkspacesD = withWorkspaces Constants.workspaces workspaceKeys

layout = tiled ||| Full
  where
    tiled = Tall 1 (3/100) (2/3)

printer = defaultPP { ppCurrent         = xmobarColor "#fcaf3e" ""
                    , ppVisible         = xmobarColor "#d3d7cf" ""
                    , ppHidden          = id
                    , ppHiddenNoWindows = const ""
                    , ppUrgent          = xmobarColor "#cc0000" ""
                    , ppSep             = "│"
                    , ppWsSep           = ""
                    , ppTitle           = xmobarColor "#8cc4ff" ""
                    , ppLayout          = (:[]) . head
                    , ppOrder           = id
                    , ppExtras          = []
                    , ppSort            = getSortByMyCompare
                    }

manageHook = (composeOne . concat $
    [ [ isDialog -?> doFloat
      , transience
      , isFullscreen -?> doFullFloat ]
    , [ className =? c -?> doFloat | c <- myCFloats ]
    , [ title     =? t -?> doFloat | t <- myTFloats ]
    , [ resource  =? r -?> doFloat | r <- myRFloats ]
    , [ className =? "Xfce4-notifyd" -?> doIgnore <+> doF copyToAll ]
    ])
    <+> manageDocks
    <+> XMonad.manageHook defaultConfig
    where
        myCFloats = []
        myTFloats = ["GLFW-b-demo"]
        myRFloats = []
