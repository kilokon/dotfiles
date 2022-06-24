--Aviik's Xmonad Config 060122

import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
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
import qualified XMonad.Actions.FlexibleResize as Flex
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.CycleWS  (shiftNextScreen, shiftPrevScreen,
                                WSType(..), nextScreen, prevScreen,
                                toggleWS, prevWS, nextWS)
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras



myWorkspaces = withScreens 2 ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
aviikVfxSW      = ["Krita", "Blender", "Houdini", "Nuke"]
aviikGameGuis   = ["Godot"]
aviikInspVfx    = ["rvplayer", "djv"]

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset
scratchpads = [
        NS "python" "alacritty --title python3 -e python3" (title =? "python3") (customFloating $ W.RationalRect 0 0 1 (3/12))
        ]

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
    , terminal  = "wezterm"
    , workspaces = myWorkspaces
    , focusFollowsMouse  = False
    , focusedBorderColor = "#000000"
    , borderWidth        = 1
    , keys               = myKeys
    , layoutHook = myLayout      -- Use custom layouts
    , manageHook = myManageHook  -- Match on certain windows
    , mouseBindings     = myMouseBindings
    , startupHook = myStartupHook
--    , logHook = transparentHook
    }
  `additionalKeysP`
    [ ("M-r"            ,       spawn "xmonad --recompile"      ) 
    , ("M-q"            ,       spawn "xmonad --recompile && xmonad --restart")        -- Restarts xmonad
    , ("M-S-<Return>"   ,       spawn "wezterm"               )
    , ("M-p"            ,       spawn "hmenu -f ~/.secrets/hmenu_history")
    , ("M-<Print>"        ,       unGrab *> spawn "scrot ~/.images/screen_prints/%Y-%m-%d-%T-screenshot.png"      )
 --   , ("<Print>"        ,       withPrefixArgument takeScreenshot)
    , ("M-f"            ,       spawn "firefox"                 )
    , ("M-c"            ,       kill)
    , ("M-z>"           ,       toggleWS                        )
    , ("M-<Return>"     ,       windows W.swapMaster            )
    , ("M-<Up>"         ,       windows W.focusUp               )
    , ("M-<Down>"       ,       windows W.focusDown             )
    , ("M-<Left>"       ,       nextScreen                      )       -- Switch focus to next monitor
    , ("M-<Right>"      ,       prevScreen                      )       -- Switch focus to prev monitor
    , ("M-S-<Left>"     ,       shiftNextScreen  >> nextScreen  )
    , ("M-S-<Right>"    ,       shiftPrevScreen  >> prevScreen  )
    , ("M-S-f"          ,       withFocused $ windows . W.sink  ) 
    , ("M-C-t"          ,       namedScratchpadAction scratchpads "python")
    ]
--    where


-- keyBindings conf = let modm = modMask conf in M.fromList $
--     {- lots of other keybindings -}
--     [((m .|. modm, k), windows $ onCurrentScreen f i)
--         | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
--         , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

myManageHook :: ManageHook
myManageHook = composeAll . concat $
        [ [ className =? "blender"       --> doCenterFloat        ]
        , [ className =? "Navigator"     --> doFloat              ]
        , [ className =? "Firefox"       --> doShift        "3"   ]
        , [ isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"        --> doCenterFloat]
        , [ isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_NOTIFICATION"  --> doCenterFloat]
        , [ isInProperty "_NET_WM_WINDOW_TYPE" "_KDE_NET_WM_WINDOW_TYPE_OVERRIDE"  --> doCenterFloat]
        , [ isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_UTILITY"       --> doIgnore]
        , [isDialog     --> doFloat ]
        , [namedScratchpadManageHook scratchpads]

    ] -- <+> namedScratchpadManageHook scratchpads

tallayout = renamed [Replace "tall"]
           $ smartBorders 
           $ limitWindows 12
           $ spacingWithEdge 10 
           $ aviik_adaptiveTall 
           where landscapeSide = ResizableTall 1 (3/100) (1/2) []
                 potraitSide= Mirror (Tall 1 (3/100) (60/100))
                 aviik_adaptiveTall = ifWider 1080 landscapeSide potraitSide 


threeRowColumn = renamed [Replace "threeRow"]
                $ limitWindows 7
                $ spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True
                $ aviik_adaptive_row_column
                where colum_side = ThreeCol 1 (3/100) (1/2)
                      row_side   = Mirror (ThreeCol 1 (3/100) (1/2))
                      aviik_adaptive_row_column = ifWider 1080 colum_side row_side

layoutSpiral = renamed [Replace "spiral"]
                $limitWindows 6
                $ adaptive_fibonacci
                where lndscape_monitor = spiral (125 % 146)
                      potrait_monitor = Mirror lndscape_monitor
                      adaptive_fibonacci = ifWider 1080 lndscape_monitor potrait_monitor 

myLayout = threeRowColumn ||| tallayout ||| layoutSpiral ||| Full



myStartupHook = do
        spawnOnce "feh --randomize --bg-fill ~/.images/wallpapers/lndscp/* ~/.images/wallpapers/tall/* &"
 -- spawnOnce "feh --bg-max --randomize --xinerama-index 0 ~/.wallpaper/landscape_orientation/* &"
  --spawnOnce "feh --randomize --bg-fill --no-fehbg --xinerama-index 0 ~/.wallpaper/landscape_orientation/* &"
  --spawnOnce "feh --randomize --bg-fill --no-fehbg --xinerama-index 1 ~/.wallpaper/potrait_orientation/* &"

  
-- Mouse bindings:
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
        [ ((modMask .|. shiftMask, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster) -- set the window to floating mode and move by dragging
        , ((modMask, button2), \w -> focus w >> windows W.shiftMaster) -- raise the window to the top of the stack
        , ((modMask, button3), \w -> focus w >> Flex.mouseResizeWindow w) -- set the window to floating mode and resize by dragging
        , ((modMask, button4), const prevWS) -- switch to previous workspace
        , ((modMask, button5), const nextWS) -- switch to next workspace
        -- , ((modMask, button9), (toggleWS)) -- switch to next workspace
        ]

-- keybindings
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $ 
        ((modm,               xK_space ), sendMessage NextLayout)
        :
        [((m .|. modm, k), windows $ onCurrentScreen f i)
            | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
            , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

