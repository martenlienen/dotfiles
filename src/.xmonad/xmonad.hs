import System.IO
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Paste (pasteString)
import XMonad.Util.Run (spawnPipe)

main = xmonad =<< bar config
  where
    config = myConfig `additionalKeysP` myKeys
    bar = statusBar "xmobar" myPP toggleXmobarKey

-- Set WM name, so that java swing applications work
myConfig = defaultConfig { modMask = mod4Mask,
                           startupHook = setWMName "LG3D",
                           focusedBorderColor = "#8f9d6a",
                           normalBorderColor = "#323537",
                           borderWidth = 2,
                           layoutHook = layouts
                         }

dbusMsg msg = "dbus-send --system --print-reply --dest=org.freedesktop.login1 /org/freedesktop/login1 \"org.freedesktop.login1.Manager." ++ msg ++ "\" boolean:true > /home/cqql/log"

maim opts = "maim " ++ opts ++ " ~/$(date +%F-%T).png"

myKeys = [ ("<XF86AudioRaiseVolume>", spawn "amixer -c 0 -- sset Master '2.00dB+'")
         , ("<XF86AudioLowerVolume>", spawn "amixer -c 0 -- sset Master '1.00dB-'")
         , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 10 -time 0")
         , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 10 -time 0")
         , ("M-<F10>", spawn (dbusMsg "PowerOff"))
         , ("M-<F11>", spawn (dbusMsg "Reboot"))
         , ("M-<F12>", spawn (dbusMsg "Suspend"))
         , ("M-s", spawn (maim ""))
         , ("M-S-s", spawn (maim "-s"))
         , ("M-S-e", spawn "emacs")
         , ("M-S-i", spawn "chromium")
         , ("M-S-f", spawn "firefox") ]

myPP = defaultPP { ppCurrent = xmobarColor "white" "" . wrap "<" ">",
                   ppHidden = xmobarColor "#a7a7a7" "",
                   ppHiddenNoWindows = xmobarColor "#323537" "",
                   ppUrgent = xmobarColor "red" "",
                   ppLayout = xmobarColor "#f9ee98" "",
                   ppTitle = xmobarColor "#afc4db" "" . shorten 80,
                   ppSep = xmobarColor "#5f5a60" "" " / "
                 }

toggleXmobarKey :: XConfig t -> (KeyMask, KeySym)
toggleXmobarKey XConfig{modMask = modM} = (modM, xK_b)
