import System.Exit (exitSuccess)
import System.IO (hPutStrLn)
import System.Process (readProcess)
import XMonad
import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Actions.CycleWindows (cycleRecentWindows)
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, ppOutput)
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, ToggleStruts (..))
import XMonad.Hooks.UrgencyHook (withUrgencyHook, NoUrgencyHook (..))
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)
import XMonad.Prompt.Window (windowPromptGoto)
import XMonad.Util.EZConfig (additionalKeys, additionalKeysP)
import XMonad.Util.Run (spawnPipe)
import qualified XMonad.StackSet as W

import qualified Constants
import MPD
import Utils
import StackSetExtra as WX

main = do
       xmproc <- spawnPipe "/home/matus/.cabal/bin/xmobar -x 1 /home/matus/.xmonad/xmobarrc"
       xmonad $ ewmh $ withUrgencyHook NoUrgencyHook defaultConfig
                {
                  manageHook         = Constants.manageHook
                , layoutHook         = avoidStruts $ smartBorders Constants.layout
                , logHook            = dynamicLogWithPP Constants.printer { ppOutput = hPutStrLn xmproc }
                , handleEventHook    = handleEventHook defaultConfig <+> fullscreenEventHook <+> docksEventHook
                , modMask            = mod4Mask
                , borderWidth        = 1
                , terminal           = "urxvtc"
                , normalBorderColor  = "#000000"
                , focusedBorderColor = "#008800"
                , workspaces         = Constants.workspaces
                } `additionalKeysP`
                [ ("<XF86AudioPlay>", MPD.toggle)
                , ("<XF86AudioStop>", MPD.stop)
                , ("<XF86AudioPrev>", MPD.previous)
                , ("<XF86AudioNext>", MPD.next)
                , (leader <%> "t",       MPD.toggle)
                , (leader <%> "s",       MPD.stop)
                , (leader <%> "p",       MPD.previous)
                , (leader <%> "n",       MPD.next)
                , (leader <%> "d",       MPD.deleteCurrent)
                , (leader <%> "c",       MPD.clear)
                , (leader <%> "<F9>",    MPD.playPlaylist Clear)
                , (leader <%> "<F10>",   MPD.playArtist Clear)
                , (leader <%> "<F11>",   MPD.playDirectory Clear)
                , (leader <%> "u" <%> "<F9>",  MPD.playPlaylist Add)
                , (leader <%> "u" <%> "<F10>", MPD.playArtist Add)
                , (leader <%> "u" <%> "<F11>", MPD.playDirectory Add)
                , (leader <%> "<F12>",   MPD.jumpToTrack)
                , ("<XF86AudioRaiseVolume>", spawn "amixer -q sset Master 3%+")
                , ("<XF86AudioLowerVolume>", spawn "amixer -q sset Master 3%-")
                , ("<XF86AudioMute>",        spawn "amixer -q -D pulse sset Master toggle")
                , (leader <%> "<Insert>",    spawn "amixer -q -D pulse sset Master toggle")
                , (leader <%> "<F7>",        spawn "/home/matus/bin/toggle-touchpad")
                , ("<XF86Sleep>", spawn "sudo pm-suspend")
                , ("<Print>" <%> "<Print>", spawn "/home/matus/bin/take-screenshot")
                , ("<Print>" <%> "u" <%> "<Print>", spawn "/home/matus/bin/take-screenshot noupload")
                , (leader <%> "<f1>" <%> "<f1>", spawn "xfce4-settings-manager")
                , (leader <%> "<f1>" <%> "<f2>", spawn "xfce4-appfinder")
                , ("M2-<Backspace>", toggleWS)
                , ("M2-S-<Pause>", io exitSuccess)
                , ("M2-<Pause>", recompileXMonad)
                , ("M2-p", runOrRaisePrompt Constants.prompt)
                , (leader <%> leader, windowPromptGoto Constants.prompt)
                , ("M2-c", kill)
                , ("M2-,", withScreen 0 W.view)
                , ("M2-.", withScreen 1 W.view)
                , ("M2-S-,", withScreen 0 W.shift)
                , ("M2-S-.", withScreen 1 W.shift)
                , ("M2-/", windows WX.shiftToOtherScreen)
                , ("M4-p", windows W.focusDown)
                , ("M4-n", windows W.focusUp)
                , ("M4-P", windows W.swapDown)
                , ("M4-N", windows W.swapUp)
                , ("M2-[", windows W.focusDown)
                , ("M2-]", windows W.focusUp)
                , ("M2-=", cycleRecentWindows [xK_Alt_R] xK_equal xK_minus)
                , ("M4-b", sendMessage ToggleStruts)
                , ("M4-B", broadcastMessage ToggleStruts >> refresh)
                ] `additionalKeys`
                (
                  [((m .|. mod4Mask, k), windows $ f i)
                | (i, k) <- zip Constants.workspaces workspaceKeys
                , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]] ++
                  [((m .|. mod2Mask, k), windows $ f i)
                | (i, k) <- zip Constants.workspaces workspaceKeys
                , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]] ++
                  [((mod2Mask .|. controlMask, k), windows $ WX.shiftAndView i)
                | (i, k) <- zip Constants.workspaces workspaceKeys ] ++
                  [((mod4Mask .|. mod2Mask, k), windows $ WX.shiftAndViewAtOther i)
                | (i, k) <- zip Constants.workspaces workspaceKeys ]
                )
         where
           workspaceKeys = [xK_1 .. xK_9] ++ [xK_0, xK_q, xK_w, xK_e, xK_r]
           leader = "<Pause>"

-- brno letisko LKTB
-- sliac letisko LZSL
-- http://www.airports-worldwide.info/index.php?page=airport
