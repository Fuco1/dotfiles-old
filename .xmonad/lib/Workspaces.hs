module Workspaces
       ( withWorkspaces
       , getMyCompare
       , getSortByMyCompare
       ) where

import XMonad
import XMonad.Util.WorkspaceCompare (getWsCompare, mkWsSort, WorkspaceCompare, WorkspaceSort)

import qualified XMonad.StackSet as W
import qualified StackSetExtra   as WX

-- | Map a function under keys to workspaces
withWorkspaces :: [WorkspaceId] -- ^ List of workspaces.
                  -> [KeySym] -- ^ List of keys.  Key at nth position
                              -- corresponds to nth workspace.
                  -> ButtonMask -- ^ Modifier mask.  Pass 0 for no mask.
                  -> (WorkspaceId -> WindowSet -> WindowSet)
                  -> [((ButtonMask, KeySym), X ())]
withWorkspaces wids keys mask f = [ ((mask, key), windows $ f ws) | (ws, key) <- zip wids keys]

-- | Compare workspaces by visibility, screenid and workspace index.
getMyCompare :: X WorkspaceCompare
getMyCompare = do
    w <- gets windowset
    wsCompare <- getWsCompare
    return $ \a b -> case (WX.isOnScreen a w, WX.isOnScreen b w) of
        (True, True)   -> WX.cmpByScreenId w a b
        (False, False) -> wsCompare a b
        (True, False)  -> LT
        (False, True)  -> GT

-- | Sort workspaces by visibility, screenid and workspace index.
getSortByMyCompare :: X WorkspaceSort
getSortByMyCompare = mkWsSort getMyCompare
