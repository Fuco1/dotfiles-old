import System.Exit (exitSuccess)
import System.IO (hPutStrLn)
import System.Process (readProcess)
import XMonad
import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Actions.CycleWindows (cycleRecentWindows)
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, ppOutput)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhDesktopsStartup, fullscreenEventHook)
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, ToggleStruts (..))
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.UrgencyHook (withUrgencyHook, NoUrgencyHook (..))
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Prompt.RunOrRaise (runOrRaisePrompt)
import XMonad.Prompt.Window (windowPromptGoto)
import XMonad.Util.EZConfig (additionalKeys, additionalKeysP)
import XMonad.Util.Run (spawnPipe)
import qualified XMonad.StackSet as W

import qualified Constants as C
import MPD
import Utils
import StackSetExtra as WX
import Workspaces
import PulseAudio

main = do
       xmproc <- spawnPipe "/home/matus/.cabal/bin/xmobar -x 1 /home/matus/.xmonad/xmobarrc"
       xmonad $ ewmh $ withUrgencyHook NoUrgencyHook defaultConfig
                {
                  manageHook         = C.manageHook
                , layoutHook         = avoidStruts $ smartBorders C.layout
                , startupHook        = ewmhDesktopsStartup >> setWMName "LG3D"
                , logHook            = dynamicLogWithPP C.printer { ppOutput = hPutStrLn xmproc }
                , handleEventHook    = handleEventHook defaultConfig <+> fullscreenEventHook <+> docksEventHook
                , modMask            = mod4Mask
                , borderWidth        = 1
                , terminal           = "urxvtc -e fish"
                , normalBorderColor  = "#000000"
                , focusedBorderColor = "#008800"
                , workspaces         = C.workspaces
                } `additionalKeysP`
                [ ("<XF86AudioPlay>", MPD.toggle)
                , ("<XF86AudioStop>", MPD.stop)
                , ("<XF86AudioPrev>", MPD.previous)
                , ("<XF86AudioNext>", MPD.next)
                , (leader <%> "t",        MPD.toggle)
                , (leader <%> "s",        MPD.stop)
                , (leader <%> "p",        MPD.previous)
                , (leader <%> "n",        MPD.next)
                , (leader <%> "<Print>",  MPD.toggle)
                , (leader <%> "-",        MPD.stop)
                , (leader <%> "<Delete>", MPD.next)
                , (leader <%> "d",        MPD.deleteCurrent)
                , (leader <%> "c",        MPD.clear)
                , (leader <%> "<F9>",     MPD.playPlaylist Clear)
                , (leader <%> "<F10>",    MPD.playArtist Clear)
                , (leader <%> "<F11>",    MPD.playDirectory Clear)
                , (leader <%> "u" <%> "<F9>",  MPD.playPlaylist Add)
                , (leader <%> "u" <%> "<F10>", MPD.playArtist Add)
                , (leader <%> "u" <%> "<F11>", MPD.playDirectory Add)
                , (leader <%> "<F12>",   MPD.jumpToTrack)
                , ("<XF86AudioRaiseVolume>", spawn "amixer -q sset Master 3%+")
                , ("<XF86AudioLowerVolume>", spawn "amixer -q sset Master 3%-")
                , ("<XF86AudioMute>",        spawn "amixer -q -D pulse sset Master toggle")
                , (leader <%> "m", muteSinkInput)
                , (leader <%> "v", setVolumeSinkInput)
                , (leader <%> "<Insert>",    spawn "amixer -q -D pulse sset Master toggle")
                , (leader <%> "<F7>",        spawn "/home/matus/bin/toggle-touchpad")
                , ("<XF86Sleep>", spawn "sudo pm-suspend")
                , ("<Print>" <%> "<Print>", spawn "/home/matus/bin/take-screenshot")
                , ("<Print>" <%> "u" <%> "<Print>", spawn "/home/matus/bin/take-screenshot noupload")
                , (leader <%> "<F1>" <%> "<F1>", spawn "xfce4-settings-manager")
                , (leader <%> "<F1>" <%> "<F2>", spawn "xfce4-appfinder")
                , ("M2-<Backspace>", toggleWS)
                , ("M2-S-<Pause>", io exitSuccess)
                , ("M2-<Pause>", recompileXMonad)
                , ("M2-p", runOrRaisePrompt C.prompt)
                , (leader <%> leader, windowPromptGoto C.prompt)
                , ("M2-c", kill)
                , ("M2-,", withScreen main W.view)
                , ("M2-.", withScreen aux W.view)
                , ("M2-S-,", withScreen main W.shift)
                , ("M2-S-.", withScreen aux W.shift)
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
                  [ (mod2Mask,                   W.greedyView)
                  , (mod4Mask,                   W.greedyView)
                  , ((shiftMask .|. mod2Mask),   W.shift)
                  , ((shiftMask .|. mod4Mask),   W.shift)
                  , ((controlMask .|. mod2Mask), WX.shiftAndView)
                  , ((mod4Mask .|. mod2Mask),    WX.shiftAndViewAtOther)
                  ]
                  >>= (uncurry C.withWorkspacesD)
                )
         where
           leader = "<Pause>"
           main = 0
           aux = 1

-- brno letisko LKTB
-- sliac letisko LZSL
-- http://www.airports-worldwide.info/index.php?page=airport
