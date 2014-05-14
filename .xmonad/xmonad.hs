-- Main/utilities
import XMonad
import Data.Monoid
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
-- Applications

-- If possible just launch a new emacs client frame with the most recent
-- not currently shown buffer. If not possible then probably the emacs
-- daemon is not running. So run a new emacs daemon (via bash so that it
-- inherits all the shell variables from .bashrc), then launch an
-- emacsclient frame.
myEditor = "emacsclient -c -n -e '(switch-to-buffer nil)' || (bash -l -c \"emacs --daemon\" && emacsclient -c -n )"

-- App for taking notes, use deft in emacs
myNotes = "emacsclient -c -n -e '(new-clean-deft)'"

-- Try various terminals till one works.
myTerminal = "urxvt || xfce4-terminal || gnome-terminal"

-- Terminal running fish shell
myFish = "urxvt -e fish || gnome-terminal -e fish"

-- Terminal shortcuts, don't really use non-urxvt terminals anymore so
-- can't be bothered with the || stuff...
oomphTerminal = "urxvt -e /bin/sh -c 'cd ~/oomph-lib && /bin/bash'"

micromagTerminal = "urxvt -e /bin/sh -c 'cd ~/oomph-lib/user_drivers/micromagnetics && /bin/bash'"
driverTerminal = "urxvt -e /bin/sh -c 'cd ~/oomph-lib/user_drivers/micromagnetics/control_scripts/driver && /bin/bash'"
optDriverTerminal = "urxvt -e /bin/sh -c 'cd ~/optoomph/user_drivers/micromagnetics/control_scripts && /bin/bash'"

-- Try various browsers until one works.
myBrowser = "google-chrome || chromium || chromium-browser || firefox"

-- Run dmenu for executables with whatever other stuff I've set up.
-- myLauncher =  "~/.xmonad/dmenu/my-dmenu-run.sh"
myLauncher = "dmenu_run"

-- Run dmenu for folders and open in whatever gui file manager we have.
guiFolderOpen = "~/.xmonad/dmenu/dfoldermenu.sh"

-- Try various locking mechanisms until one works.
myLockScreen = "mate-screensaver-command -l || xflock4 || gnome-screensaver-command -l"

myiPythonCommand = "bash -l -c \"ipython3\""
myiPython = "urxvt -e " ++ myiPythonCommand ++
            " || gnome-terminal -e " ++ myiPythonCommand

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

myManageHook =
  composeAll
  [ className =? "Gimp"           --> doFloat
  , resource  =? "desktop_window" --> doIgnore
  , className =? "Paraview" --> doShift "pv"
  , className =? "Mendeleydesktop" --> doShift "mly"
  , className =? "Tk" --> doFloat
  -- , className =? "Tk" --> doShift "plots"
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

myLayout = avoidStruts(tiled ||| Mirror matlabsucks ||| noBorders Full)
  where
    -- Default tiling algorithm: partitions the screen into two panes
    tiled   = Tall nmaster delta ratio

    -- Layout with roughly even sized windows, for the first few anyway..
    matlabsucks = Tall 2 delta ratio

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio   = 1/2

    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

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

myWorkspaces = ["1","2","3","4","5","6","plots",
                "pv","mly"]

------------------------------------------------------------------------
-- Now run xmonad with these settings and some others given here

main = do
     -- xmproc <- spawnPipe "xmobar ~/.xmobarrc"
     xmonad $ gnomeConfig
        { terminal   = myTerminal
        , modMask    = mod4Mask -- use super as modifier key
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
        `removeKeysP` ["M-S-q" ,"M-e", "M-r", "M-S-e", "M-S-r", "M-.", "M-,"]
        `additionalKeysP` myKeys

-- Some additional keybinds, mostly inspired by chromes tab management
-- keybinds and whatever else I was used to using.
myKeys = [
  -- next window - normal alt-tab
  -- ("M1-<Tab>", windows W.focusDown)

  -- close focused window
  ("M-w", kill)
  , ("M-S-w", spawn "xkill")

    -- run things
  , ("M-t", spawn myTerminal)
  -- , ("M-f", spawn myFish)
  , ("M-f", spawn myEditor)
  , ("M-p", spawn myLauncher)
  , ("M-r", spawn oomphTerminal)
  , ("M-s", spawn micromagTerminal)
  , ("M-d", spawn driverTerminal)
  , ("M-a", spawn optDriverTerminal)
  , ("M-m", spawn myNotes)

  , ("M-y", spawn myBrowser)
  , ("M-,", spawn myiPython)
    
    
    -- movement
  , ("M-<R>", sendMessage Expand)
  , ("M-<L>", sendMessage Shrink)
  , ("M-e", windows W.focusDown)
  , ("M-i", windows W.focusUp)


    -- multiple screens
  , ("M-x", nextScreen)
  , ("M-S-x", shiftNextScreen)
  , ("M-z", swapNextScreen)

    -- Lock screen
  , ("M-=", spawn myLockScreen)

    -- Quit xmonad (e.g. if we have no gnome session this is the only way to log out)
  , ("M-S-q", io (exitWith ExitSuccess))

    -- go to previous workspace
  , ("M-<Backspace>", toggleWS)

    -- Next/previous workspace
  , ("M-o", nextWS)
  , ("M-n", prevWS)
  , ("M-S-o", shiftToNext)
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
