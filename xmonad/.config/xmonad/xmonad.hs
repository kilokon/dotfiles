
import XMonad
import System.Directory
import System.IO 
import System.Exit (exitSuccess)
import Data.Monoid
--import qualified system_constants as C
import XMonad.Layout.Gaps
import XMonad.Layout.IndependentScreens (countScreens, withScreens, onCurrentScreen)
--Layout
--import XMonad.Layout.GridVariants (Grid(Grid))
--import XMonad.Layout.Renamed (renamed, Rename(..))

import XMonad.Layout.NoBorders
import XMonad.Actions.CycleWS  (moveTo, shiftTo, WSType(..), nextScreen, prevScreen, toggleWS)
import qualified XMonad.StackSet as W
import XMonad.Layout.Spacing 
import XMonad.Layout.LayoutCombinators 
import XMonad.Layout.Spiral
import Data.Ratio -- this makes the '%' operator available (optional)
--import XMonad.Layout.Spiral
import XMonad.Layout.GridVariants

import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras


term_aviik = "alacritty" --my preferred terminal
workspaces_aviik = ["1","2","3","4","5","6","7","8","9"]


myLayouts = layoutTall ||| layoutSpiral ||| layoutGrid ||| layoutMirror ||| layoutFull
     where
       layoutTall = Tall 1 (3/100) (1/2)
       --layoutSpiral = spiral (125 % 146)
       layoutGrid = Grid
       layoutMirror = Mirror (Tall 1 (3/100) (3/5))
       layoutFull = Full

-- myLayouts = spacing 10 $
--                 tiled ||| spiral ||| Mirror tiled ||| Full
--         where
--                 tiled   = Tall 1 (3/100) (1/2)
--                 spiral  = spiral


--myStartupHook = do
--  spawnOnce "feh --bg-max --randomize ~/.wallpaper/* &"

--myLayoutHook = avoidStruts $


myKeys :: [(String, X ())]
myKeys =[ ("M-q"        ,       spawn "xmonad --recompile")                            -- Recompiles xmonad
        , ("M-S-r"      ,       spawn "xmonad --recompile && xmonad --restart")        -- Restarts xmonad
        , ("M-p"  ,       spawn "dmenu_run -hp alacritty,emacs,firefox,thunar")
    -- Workspaces
        , ("M-z"        ,       toggleWS  )
        , ("M-<Left>"   ,       nextScreen)  -- Switch focus to next monitor
        , ("M-<Right>"  ,       prevScreen)  -- Switch focus to prev monitor
        ]

main :: IO()
main = do 
        nScreens <- countScreens
        xmonad $ def {
                  terminal           = term_aviik
                , focusFollowsMouse  = False
                , borderWidth        = 1 
                , layoutHook         = myLayouts
                , workspaces         = withScreens nScreens workspaces_aviik
                --, startupHook        = myStartupHook
                } `additionalKeysP` myKeys
