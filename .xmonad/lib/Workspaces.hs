module Workspaces
       ( withWorkspaces
       ) where

import XMonad

-- | Map a function under keys to workspaces
withWorkspaces :: [WorkspaceId] -- ^ List of workspaces.
                  -> [KeySym] -- ^ List of keys.  Key at nth position
                              -- corresponds to nth workspace.
                  -> ButtonMask -- ^ Modifier mask.  Pass 0 for no mask.
                  -> (WorkspaceId -> WindowSet -> WindowSet)
                  -> [((ButtonMask, KeySym), X ())]
withWorkspaces wids keys mask f = [ ((mask, key), windows $ f ws) | (ws, key) <- zip wids keys]

