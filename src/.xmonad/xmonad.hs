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
myConfig = defaultConfig { modMask = mod4Mask, startupHook = setWMName "LG3D" }

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
         , ("M-e", spawn "emacs")
         , ("M-c", spawn "chromium")
         , ("M-f", spawn "firefox") ]

myPP = defaultPP { ppCurrent = xmobarColor "white" "" . wrap "<" ">",
                   ppHidden = xmobarColor "#3E5247" "",
                   ppHiddenNoWindows = xmobarColor "#3E5247" "",
                   ppUrgent = xmobarColor "red" "",
                   ppLayout = xmobarColor "blue" "",
                   ppTitle = xmobarColor "green" "" . shorten 80,
                   ppSep = xmobarColor "purple" "" " # "
                 }

toggleXmobarKey :: XConfig t -> (KeyMask, KeySym)
toggleXmobarKey XConfig{modMask = modM} = (modM, xK_b)
