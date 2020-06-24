-- Main/utilities
import XMonad
import Data.Monoid
import Data.List
import System.Exit
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import XMonad.Util.EZConfig
import XMonad.Util.Run(spawnPipe)
import System.IO


-- Xfce compatability (turns out that config.gnome is the same for xfce but better for gnome)
import XMonad.Config.Gnome
import XMonad.Hooks.ManageDocks

-- Chose workspace to spawn a window on
import XMonad.Actions.SpawnOn

-- Better cycling through workspaces and screens
import XMonad.Actions.CycleWS

-- No border if in full screen mode
import XMonad.Layout.NoBorders

-- More layouts
import XMonad.Layout.ResizableTile
import XMonad.Layout.StackTile

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

-- Handle fullscreen
import XMonad.Hooks.EwmhDesktops

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.

isInfixOfQuery :: String -> Query String -> Query Bool
isInfixOfQuery needle haystackQuery = fmap (\haystack -> (isInfixOf haystack needle)) haystackQuery

myManageHook =
  composeAll
  [
    resource  =? "desktop_window" --> doIgnore
  , className =? "Tk" --> doFloat
  , className =? "Octave"          --> doFloat
  , className =? "Gnuplot"          --> doFloat
  -- Virtual box is currently completely broken if you don't allow it to resize
  , className =? "VirtualBox" --> doFloat

  , className =? "SchemeGraphics" --> doFloat
  , className =? "zoom" --> doFloat

  -- Popups
  , className =? "Zenity" --> doFloat
  , className =? "davidshepherd7.xtip" --> doFloat

  -- Gloss games
  , title =? "Pong" --> doFloat
  , title =? "Conway" --> doFloat

  -- Robot framework dialogs
  , className =? "Toplevel" --> doFloat

  -- Android Emulator crap
  , isInfixOfQuery "Android Emulator -" title --> doFloat

  , isFullscreen --> doFullFloat
  ]
  <+> manageHook gnomeConfig -- keep gnome/xfce compatability settings

------------------------------------------------------------------------
-- Layouts

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- * NOTE: XMonad.Hooks.EwmhDesktops users must remove the obsolete
-- ewmhDesktopsLayout modifier from layoutHook. It no longer exists.
-- Instead use the 'ewmh' function from that module to modify your
-- defaultConfig as a whole. (See also logHook, handleEventHook, and
-- startupHook ewmh notes.)
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.

myLayout = avoidStruts(tiled ||| fullWidth |||
                       noBorders Full)
  where
    -- Default tiling algorithm: partitions the screen into two panes
    tiled   = Tall 1 delta ratio

    -- Layout with roughly even sized windows, for the first few anyway..
    fullWidth = Mirror (Tall 1 delta ratio)

    -- Default proportion of screen occupied by master pane
    ratio   = 1/2

    -- Percent of screen to increment by when resizing panes
    delta   = 10/100

------------------------------------------------------------------------
-- Event handling

-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.

-- Handle fullscreen properly and keep gnome compatability settings.
myEventHook = fullscreenEventHook <+> handleEventHook gnomeConfig

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add EWMH logHook actions to your custom log hook by
-- combining it with ewmhDesktopsLogHook.
--

-- myLogHook xmproc = dynamicLogWithPP $ xmobarPP
--                        { ppOutput = hPutStrLn xmproc
--                        , ppTitle = xmobarColor "green" "" . shorten 50
--                        }

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.

myStartupHook = do
  startupHook gnomeConfig


------------------------------------------------------------------------
-- Workspaces

myWorkspaces = ["1", "2", "3", "dump"]

------------------------------------------------------------------------
-- Now run xmonad with these settings and some others given here

main = do
     -- xmproc <- spawnPipe "xmobar ~/.xmobarrc"
     xmonad $ ewmh gnomeConfig
        { modMask    = mod4Mask -- use super as modifier key
        , borderWidth = 2
        , manageHook = myManageHook
        , workspaces = myWorkspaces
        , layoutHook = smartBorders( myLayout )
        -- , logHook = myLogHook xmproc >> logHook gnomeConfig -- Attempt
        -- to combine both log hooks... Doesn't quite work
        , logHook = logHook gnomeConfig
        , startupHook = myStartupHook
        , normalBorderColor = "#242424" -- pale blue
        , focusedBorderColor = "#87ceeb" -- pale grey
        , handleEventHook = myEventHook
        }
        -- Unbind some keys
        `removeKeysP` ["M-S-q" ,"M-e", "M-r", "M-S-e", "M-S-r", "M-.", "M-,", "M-t", "M-m", "M-p", "M-P", "M-k"]
        `additionalKeysP` myKeys
        `removeMouseBindings` [(mod4Mask, button3)]

-- Some additional keybinds, mostly inspired by chromes tab management
-- keybinds and whatever else I was used to using.
myKeys = [
  -- next window - normal alt-tab
  -- ("M1-<Tab>", windows W.focusDown)

  -- close focused window
  ("M-w", kill)
  , ("M-S-w", spawn "xkill")


  -- movement
  , ("M-<R>", sendMessage Expand)
  , ("M-<L>", sendMessage Shrink)
  , ("M-<D>", sendMessage Expand)
  , ("M-<U>", sendMessage Shrink)
  , ("M-h", windows W.focusUp)
  , ("M-i", windows W.focusUp)


  -- multiple screens
  , ("M-x", nextScreen)
  , ("M-S-x", shiftNextScreen)
  , ("M-z", swapNextScreen)

  -- Quit xmonad (e.g. if we have no gnome session this is the only way to log out)
  , ("M-S-q", io (exitWith ExitSuccess))

  -- go to previous workspace
  , ("M-<Backspace>", toggleWS)

  -- Next/previous workspace
  , ("M-e", nextWS)
  , ("M-n", prevWS)
  , ("M-S-e", shiftToNext)
  , ("M-S-n", shiftToPrev)


  -- Floats
  , ("M-u", withFocused $ windows . W.sink)
  ]
  ++ -- ++ combines the two lists

     -- Create a list of bindings:
     -- set all "M-number" to W.view that number
     [ (otherModMasks ++ "M-" ++ [key], action tag)
       | (tag, key)  <- zip myWorkspaces "123456789"
       , (otherModMasks, action) <- [ ("", windows . W.view)
       , ("S-", windows . W.shift)]]

  ++
  [
    ("<KP_Insert>", windows W.focusUp)
    , ("<KP_Delete>", windows W.focusDown)

    , ("<KP_Left>", prevWS)
    , ("<KP_Begin>", nextScreen)
    , ("<KP_Right>", nextWS)

    , ("<KP_Home>", shiftToPrev)
    , ("<KP_Up>", shiftNextScreen)
    , ("<KP_Prior>", shiftToNext)

    , ("<KP_Subtract>", kill)
  ]
