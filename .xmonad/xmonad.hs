import System.Exit
import System.IO
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.WorkspaceCompare
import qualified XMonad.StackSet as W

my_workspaces :: [WorkspaceId]
my_workspaces = ["tc1", "irc", "op1", "em1", "ank", "ws6", "ws7", "ws8", "ws9", "ws0",
                 "tc2", "pdf", "op2", "em2"]
-- 1,..,9,0
-- a,s,d,f

my_layout = tiled ||| Full
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 2/3
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

myPP = defaultPP { ppCurrent         = xmobarColor "#fcaf3e" "" . wrap "[" "]"
                 , ppVisible         = xmobarColor "#d3d7cf" "" . wrap "(" ")"
                 , ppHidden          = id
                 , ppHiddenNoWindows = const ""
                 , ppUrgent          = xmobarColor "#cc0000" "" . wrap "*" "*"
                 , ppSep             = "│"
                 , ppWsSep           = " "
                 , ppTitle           = xmobarColor "#8cc4ff" "" . shorten 80
                 , ppLayout          = (:[]) . head
                 , ppOrder           = id
                 , ppExtras          = []
                 }

main = do
       xmproc <- spawnPipe "/home/matus/.cabal/bin/xmobar -x 1 /home/matus/.xmobarrc"
       xmonad $ defaultConfig
                {
                  manageHook = manageDocks <+> manageHook defaultConfig
                , layoutHook = avoidStruts $ smartBorders $ my_layout
                , logHook = dynamicLogWithPP myPP { ppOutput = hPutStrLn xmproc }
                , modMask = mod4Mask
                , borderWidth        = 1
                , terminal           = "urxvtc"
                , normalBorderColor  = "#000000"
                , focusedBorderColor = "#008800"
                , workspaces = my_workspaces
                } `additionalKeysP`
                [
                  ("<XF86AudioPlay>", spawn "mpc toggle")
                , ("<XF86AudioStop>", spawn "mpc stop")
                , ("<XF86AudioPrev>", spawn "mpc prev")
                , ("<XF86AudioNext>", spawn "mpc next")
                , ("<XF86AudioRaiseVolume>", spawn "amixer -q sset Master 3%+")
                , ("<XF86AudioLowerVolume>", spawn "amixer -q sset Master 3%-")
                , ("<XF86AudioMute>", spawn "amixer -D pulse -q set Master toggle")
                , ("M2-<Backspace>", toggleWS)
                , ("M2-S-<Pause>", io (exitWith ExitSuccess))
                , ("M2-<Pause>",
                   spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
                ] `additionalKeys`
                (
                  [
                  ((mod2Mask, xK_c), kill)
                , ((mod2Mask, xK_p), spawn "dmenu_run -p 'Run> ' -b -nf '#888a85' -nb '#2e3436' -sf '#eeeeec' -sb '#3465a4'")
                , ((mod2Mask, xK_period), screenWorkspace 0 >>= flip whenJust (windows . W.view))
                , ((mod2Mask, xK_comma), screenWorkspace 1 >>= flip whenJust (windows . W.view))
                , ((mod2Mask .|. shiftMask, xK_period), screenWorkspace 0 >>=
                                                        flip whenJust (windows . W.shift))
                , ((mod2Mask .|. shiftMask, xK_comma), screenWorkspace 1 >>=
                                                       flip whenJust (windows . W.shift))
                , ((mod4Mask, xK_p), windows W.focusDown)
                , ((mod4Mask, xK_n), windows W.focusUp)
                , ((mod4Mask .|. shiftMask, xK_p), windows W.swapDown)
                , ((mod4Mask .|. shiftMask, xK_n), windows W.swapUp)
                , ((mod4Mask, xK_b), sendMessage ToggleStruts)
                ] ++
                [((m .|. mod4Mask, k), windows $ f i)
                | (i, k) <- zip my_workspaces workspaceKeys
                , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]] ++
                [((m .|. mod2Mask, k), windows $ f i)
                | (i, k) <- zip my_workspaces workspaceKeys
                , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
                )
         where
           workspaceKeys = [xK_1 .. xK_9] ++ [xK_0, xK_q, xK_w, xK_e, xK_r]


-- XF86AudioPlay
-- XF86AudioStop
-- XF86AudioPrev
-- XF86AudioNext

-- brno letisko LKTB
-- sliac letisko LZSL
-- http://www.airports-worldwide.info/index.php?page=airport
