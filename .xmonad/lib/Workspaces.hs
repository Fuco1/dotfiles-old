{-# LANGUAGE DeriveDataTypeable #-}
module Workspaces
       ( withWorkspaces
       , getMyCompare
       , getSortByMyCompare
       , ScreenOrder(..)
       ) where

import XMonad
import XMonad.Util.WorkspaceCompare (getWsCompare, mkWsSort, WorkspaceCompare, WorkspaceSort)

import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
import qualified StackSetExtra   as WX

data ScreenOrder = ScreenOrder [ScreenId] deriving Typeable
instance ExtensionClass ScreenOrder where
  initialValue = ScreenOrder [2,0,1]

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
