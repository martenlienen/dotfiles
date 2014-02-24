import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

main = xmonad =<< bar myConfig
  where
    bar = statusBar "xmobar" myPP toggleXmobarKey

myConfig = defaultConfig { modMask = mod4Mask
                         }

myPP = defaultPP { ppCurrent = xmobarColor "white" "" . wrap "<" ">",
                   ppHidden = xmobarColor "gray" "",
                   ppHiddenNoWindows = xmobarColor "gray" "",
                   ppUrgent = xmobarColor "red" "",
                   ppLayout = xmobarColor "blue" "",
                   ppTitle = xmobarColor "green" "" . shorten 80,
                   ppSep = xmobarColor "purple" "" " # "
                 }

toggleXmobarKey :: XConfig t -> (KeyMask, KeySym)
toggleXmobarKey XConfig{modMask = modM} = (modM, xK_b)
