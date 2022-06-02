--Aviik's Xmonad Config 060122
import XMonad
import qualified XMonad.StackSet as W
import XMonad.ManageHook
import Data.Ratio 
--LAYOUT
import XMonad.Layout.ThreeColumns
import XMonad.Layout.IndependentScreens
import XMonad.Layout.MessageControl
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.LimitWindows
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spiral
import XMonad.Layout.Spacing
import XMonad.Layout.PerScreen

--UTILITY
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.SpawnOnce
import XMonad.Util.NamedScratchpad

--HOOKS
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
--ACTIONS
import XMonad.Actions.CycleWS  (shiftNextScreen, shiftPrevScreen, WSType(..), nextScreen, prevScreen, toggleWS)
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
aviikVfxSW      = ["Krita", "Blender", "Houdini", "Nuke"]
aviikGameGuis   = ["Godot"]
aviikInspVfx    = ["rvplayer", "djv"]

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset
scratchpads = [
        NS "ytop" "st -e ytop" (title =? "ytop") defaultFloating ]

transparentHook :: X ()
transparentHook = fadeInactiveLogHook fadeAmount
    where fadeAmount = 0.8

main :: IO ()
main =  xmonad
     . ewmhFullscreen
     . ewmh
     $ myConfig

myConfig = def
    { modMask    = mod4Mask      -- Rebind Mod to the Super key
    , terminal  = "alacritty"
    , workspaces = withScreens 2 myWorkspaces
    , focusFollowsMouse  = False
    , focusedBorderColor = "#000000"
    , borderWidth        = 1
    , layoutHook = myLayout      -- Use custom layouts
    , manageHook = myManageHook  -- Match on certain windows
    , startupHook = myStartupHook
    , logHook = transparentHook
    }
  `additionalKeysP`
    [ ("M-r"            ,       spawn "xmonad --recompile"      ) 
    , ("M-S-r"          ,       spawn "xmonad --recompile && xmonad --restart")        -- Restarts xmonad
    , ("M-C-s"          ,       unGrab *> spawn "scrot -s"      )
    , ("M-f"            ,       spawn "firefox"                 )
    , ("M-z>"           ,       toggleWS                        )
    , ("M-<Left>"       ,       nextScreen                      )       -- Switch focus to next monitor
    , ("M-<Right>"      ,       prevScreen                      )       -- Switch focus to prev monitor
    , ("M-S-<Left>"     ,       shiftNextScreen  >> nextScreen  )
    , ("M-S-<Right>"    ,       shiftPrevScreen  >> prevScreen  )
    , ("M-C-<up>"       ,       sendMessage NextLayout )             -- Switch to next layout
    , ("M-C-t"           ,      namedScratchpadAction scratchpads "htop")
    ]

myManageHook :: ManageHook
myManageHook = composeAll . concat $
        [ [ className =? "blender" --> doCenterFloat]
        , [ isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"        --> doCenterFloat]
        , [ isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_NOTIFICATION"  --> doCenterFloat]
        , [ isInProperty "_NET_WM_WINDOW_TYPE" "_KDE_NET_WM_WINDOW_TYPE_OVERRIDE"  --> doCenterFloat]
        , [isDialog            --> doFloat]
        , [namedScratchpadManageHook scratchpads]

    ] -- <+> namedScratchpadManageHook scratchpads

tallayout = renamed [Replace "tall"]
           $ smartBorders 
           $ limitWindows 12
           $ spacingWithEdge 10 
           $ aviik_adaptiveTall 
           where landscapeSide = (ResizableTall 1 (3/100) (1/2) [])
                 potraitSide= Mirror (Tall 1 (3/100) (60/100))
                 aviik_adaptiveTall = ifWider 1080 landscapeSide potraitSide 


threeRowColumn = renamed [Replace "threeRow"]
                $ limitWindows 7
                $ spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True
                $ aviik_adaptive_row_column
                where colum_side = (ThreeCol 1 (3/100) (1/2))
                      row_side   = Mirror (ThreeCol 1 (3/100) (1/2))
                      aviik_adaptive_row_column = ifWider 1080 colum_side row_side

layoutSpiral = renamed [Replace "spiral"]
                $limitWindows 6
                $ adaptive_fibonacci
                where lndscape_monitor = (spiral (125 % 146))
                      potrait_monitor = Mirror lndscape_monitor
                      adaptive_fibonacci = ifWider 1080 lndscape_monitor potrait_monitor 

myLayout = tallayout ||| Full ||| threeRowColumn


myStartupHook = do
  spawnOnce "feh --bg-max --randomize ~/.wallpaper/* &"



