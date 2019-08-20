-- xmonad config used by Malcolm MD
-- https://github.com/randomthought/xmonad-config

import           System.IO
import           System.Exit
-- import System.Taffybar.Hooks.PagerHints (pagerHints)

import qualified Data.List                     as L

import           XMonad
import           XMonad.Actions.Navigation2D
import           XMonad.Actions.UpdatePointer

import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.EwmhDesktops                ( ewmh )



import           XMonad.Layout.Gaps
import           XMonad.Layout.Accordion
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.BinarySpacePartition
                                               as BSP

import           XMonad.Prompt
import           XMonad.Prompt.ConfirmPrompt
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.Spacing
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoFrillsDecoration
import           XMonad.Layout.Renamed
import           XMonad.Layout.Simplest
import           XMonad.Layout.Grid
import           XMonad.Layout.SubLayouts
import           XMonad.Layout.WindowNavigation
import           XMonad.Layout.ZoomRow

import           XMonad.Util.Run                          ( spawnPipe )
import           XMonad.Util.EZConfig                     ( additionalKeys )
import           XMonad.Util.Cursor


import           Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet               as W
import qualified Data.Map                      as M

-- TODO: Scratchpad terminal

----------------------------mupdf--------------------------------------------
-- Terminimport XMonad.Hooks.EwmhDesktopsal
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal = "x-terminal-emulator"

-- The command to lock the screen or show the screensaver.
myScreensaver = "dm-tool switch-to-greeter"

-- The command to take a selective screenshot, where you select
-- what you'd like to capture on the screen.
mySelectScreenshot = "select-screenshot"

-- The command to take a fullscreen screenshot.
myScreenshot = "xfce4-screenshooter"

-- The command to use as a launcher, to launch commands that don't have
-- preset keybindings.
myLauncher = "rofi -show"




------------------------------------------------------------------------
-- Workspaces
-- The default number of workspaces (virtual screens) and their names.
--
data Workspace = WorkspaceWWW | WorkspaceWork | WorkspaceTerm | WorkspaceChat

instance Show Workspace where
  -- set $ws_www     "1  "
  -- set $ws_work    "2  "
  -- set $ws_term    "3  "
  -- set $ws_chat    "4  "
  show WorkspaceWWW  = "1: web"
  show WorkspaceWork = "2: work"
  show WorkspaceTerm = "3: term"
  show WorkspaceChat = "4: chat"

myWorkspaces =
  (show <$> [WorkspaceWWW, WorkspaceWork, WorkspaceTerm, WorkspaceChat])
    ++ (show <$> [5 .. 9])

------------------------------------------------------------------------
-- Window rules
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
--
myManageHook = composeAll
  [ resource =? "desktop_window" --> doIgnore
  , isFullscreen --> (doF W.focusDown <+> doFullFloat)
  -- , resource =? "gpicview" --> doCenterFloat
  -- , className =? "Google-chrome" --> doShift "2:web"
  -- , className =? "Galculator" --> doCenterFloat
  -- , className =? "Steam" --> doCenterFloat
  -- , className =? "Gimp" --> doCenterFloat
  -- , className =? "MPlayer" --> doCenterFloat
  -- , className =? "Pavucontrol" --> doCenterFloat
  -- , className =? "Mate-power-preferences" --> doCenterFloat
  -- , className =? "Xfce4-power-manager-settings" --> doCenterFloat
  -- , className =? "VirtualBox" --> doShift "4:vm"
  -- , className =? "Xchat" --> doShift "5:media"
  -- , className =? "stalonetray" --> doIgnore
  -- added (andys8)
  , className =? "Chromium-browser" --> doShift (show WorkspaceWWW)
  , className =? "Rambox" --> doShift (show WorkspaceChat)
  ]



------------------------------------------------------------------------
-- Layouts
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.

outerGaps = 10
myGaps = gaps [(U, outerGaps), (R, outerGaps), (L, outerGaps), (D, outerGaps)]
addSpace = renamed [CutWordsLeft 2] . spacing gap
-- tab = avoidStruts $ renamed [Replace "Tabbed"] $ addTopBar $ myGaps $ tabbed
--   shrinkText
--   myTabTheme

bsp =
  avoidStruts
    $ renamed [CutWordsLeft 1]
    $ addTopBar
    $ windowNavigation
    $ renamed [Replace "BSP"]
    $ addTabs shrinkText myTabTheme
    $ subLayout [] Simplest
    $ myGaps
    $ addSpace BSP.emptyBSP

grid =
  renamed [Replace "Grid"] $ avoidStruts $ addTopBar $ addSpace $ myGaps Grid

layouts = bsp ||| grid ||| avoidStruts Simplest

myLayout = smartBorders $ mkToggle (NOBORDERS ?? FULL ?? EOT) layouts

myNav2DConf = def { defaultTiledNavigation = centerNavigation
                  , floatNavigation        = centerNavigation
                  , screenNavigation       = lineNavigation
                  , layoutNavigation       = [("Full", centerNavigation)
    -- line/center same results   ,("Tabs", lineNavigation)
    --                            ,("Tabs", centerNavigation)
                                                                        ]
                  , unmappedWindowRect     = [("Full", singleWindowRect)
    -- works but breaks tab deco  ,("Tabs", singleWindowRect)
    -- doesn't work but deco ok   ,("Tabs", fullScreenRect)
                                                                        ]
                  }


------------------------------------------------------------------------
-- Colors and borders

-- Color of current window title in xmobar.
xmobarTitleColor = "#C678DD"

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = "#51AFEF"

-- Width of the window border in pixels.
myBorderWidth = 0

myNormalBorderColor = "#000000"
myFocusedBorderColor = active

base03 = "#002b36"
base02 = "#073642"
base01 = "#586e75"
base00 = "#657b83"
base0 = "#839496"
base1 = "#93a1a1"
base2 = "#eee8d5"
base3 = "#fdf6e3"
yellow = "#b58900"
orange = "#cb4b16"
red = "#dc322f"
magenta = "#d33682"
violet = "#6c71c4"
blue = "#268bd2"
cyan = "#2aa198"
green = "#859900"

-- sizes
gap = 10
topbar = 10
border = 0
prompt = 20
status = 20

active = blue
activeWarn = red
inactive = base02
focusColor = blue
unfocusColor = base02

-- myFont      = "-*-Zekton-medium-*-*-*-*-160-*-*-*-*-*-*"
-- myBigFont   = "-*-Zekton-medium-*-*-*-*-240-*-*-*-*-*-*"
-- myFont = "xft:Zekton:size=9:bold:antialias=true"
myFont = "xft:SauceCodePro Nerd Font:size=10:bold:antialias=true"
-- myBigFont = "xft:Zekton:size=9:bold:antialias=true"
-- myWideFont =
--   "xft:Eurostar Black Extended:" ++ "style=Regular:pixelsize=180:hinting=true"

-- this is a "fake title" used as a highlight bar in lieu of full borders
-- (I find this a cleaner and less visually intrusive solution)
topBarTheme = def { fontName            = myFont
                  , inactiveBorderColor = base03
                  , inactiveColor       = base03
                  , inactiveTextColor   = base03
                  , activeBorderColor   = active
                  , activeColor         = active
                  , activeTextColor     = active
                  , urgentBorderColor   = red
                  , urgentTextColor     = yellow
                  , decoHeight          = topbar
                  }

addTopBar = noFrillsDeco shrinkText topBarTheme

myTabTheme = def { fontName            = myFont
                 , activeColor         = active
                 , inactiveColor       = base02
                 , activeBorderColor   = active
                 , inactiveBorderColor = base02
                 , activeTextColor     = base03
                 , inactiveTextColor   = base00
                 }

------------------------------------------------------------------------
-- Key bindings
--
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask = mod4Mask
altMask = mod1Mask

myKeys conf@XConfig { XMonad.modMask = modMask } =
  M.fromList
    $
  ----------------------------------------------------------------------
  -- Custom key bindings
  --

  -- Start a terminal.  Terminal to start is specified by myTerminal variable.
       [

      -- terminal
         ( (modMask, xK_Return)
         , spawn $ XMonad.terminal conf
         )
      -- file browser
       , ( (modMask, xK_n)
         , spawn "xdg-open ."
         )
      -- browser
       , ( (modMask .|. shiftMask, xK_Return)
         , spawn "x-www-browser"
         )
      -- kill focused window
       , ((modMask, xK_Escape), kill)
       , ((modMask .|. shiftMask, xK_Escape), kill)
       , ((modMask, xK_q), kill)
       , ( (altMask, xK_F4)
         , kill
         )
      -- select application with rofi
       , ((modMask, xK_d), spawn "rofi -modi drun,run -show drun -show-icons")
       , ( (modMask .|. shiftMask, xK_d)
         , spawn "rofi -show run -i -display-run \"$ \""
         )
      -- quit xmonad
       , ( (modMask .|. shiftMask, xK_e)
         , confirmPrompt greenXPConfig "Exit xmonad and logoff"
           $ io (exitWith ExitSuccess)
         )
      -- restart xmonad
       , ( (modMask .|. shiftMask, xK_r)
         , restart "xmonad" True
         )
      -- poweroff computer
       , ( (0, xF86XK_PowerDown)
         , confirmPrompt greenXPConfig "Poweroff" $ spawn "poweroff"
         )
       , ( (0, xF86XK_PowerOff)
         , confirmPrompt greenXPConfig "Poweroff" $ spawn "poweroff"
         )
       , ( (controlMask .|. modMask .|. altMask, xK_p)
         , confirmPrompt greenXPConfig "Poweroff" $ spawn "poweroff"
         )

      -- change focus
       -- , ((modMask, xK_h), windows W.focusLeft)
       -- , ((modMask, xK_j), windows W.focusDown)
       -- , ( (modMask, xK_k)
       --   , windows W.focusUp
       --   )
       -- , ( (modMask, xK_l)
       --   , windows W.focusRight
       --   )

  -- Spawn the launcher using command specified by myLauncher.
  -- Use this to launch programs without a key binding.
       -- , ( (modMask, xK_p)
       --   , spawn myLauncher
       --   )

  -- Take a selective screenshot using the command specified by mySelectScreenshot.
       , ( (modMask .|. shiftMask, xK_p)
         , spawn mySelectScreenshot
         )

  -- Take a full screenshot using the command specified by myScreenshot.
       , ( (modMask .|. controlMask .|. shiftMask, xK_p)
         , spawn myScreenshot
         )

  -- Toggle current focus window to fullscreen
       , ( (modMask, xK_f)
         , sendMessage $ Toggle FULL
         )

  -- Mute volume.
       , ( (0, xF86XK_AudioMute)
         , spawn "amixer -q set Master toggle"
         )

  -- Decrease volume.
       , ( (0, xF86XK_AudioLowerVolume)
         , spawn "amixer -q set Master 5%-"
         )

  -- Increase volume.
       , ( (0, xF86XK_AudioRaiseVolume)
         , spawn "amixer -q set Master 5%+"
         )

  -- Audio previous.
       , ( (0, 0x1008FF16)
         , spawn ""
         )

  -- Play/pause.
       , ( (0, 0x1008FF14)
         , spawn ""
         )

  -- Audio next.
       , ( (0, 0x1008FF17)
         , spawn ""
         )

  -- Eject CD tray.
       , ( (0, 0x1008FF2C)
         , spawn "eject -T"
         )
  --------------------------------------------------------------------
  -- Custom (andys8)


  --------------------------------------------------------------------
  -- "Standard" xmonad key bindings
  --

  -- Close focused window.
       -- , ( (modMask .|. shiftMask, xK_c)
       --   , kill
       --   )

  -- Cycle through the available layout algorithms.
       , ( (modMask, xK_space)
         , sendMessage NextLayout
         )

  --  Reset the layouts on the current workspace to default.
       , ( (modMask .|. shiftMask, xK_space)
         , setLayout $ XMonad.layoutHook conf
         )

  -- Resize viewed windows to the correct size.
       -- , ( (modMask, xK_n)
       --   , refresh
       --   )


  -- Move focus to the master window.
       , ( (modMask, xK_m)
         , windows W.focusMaster
         )

  -- Swap the focused window and the master window.
  -- andys8: Removed because conflict
       -- , ( (modMask, xK_Return)
       --   , windows W.swapMaster
       --   )

  -- Swap the focused window with the next window.
       , ( (modMask .|. shiftMask, xK_j)
         , windows W.swapDown
         )

  -- Swap the focused window with the previous window.
       , ( (modMask .|. shiftMask, xK_k)
         , windows W.swapUp
         )

  -- Shrink the master area.
       , ( (modMask, xK_h)
         , sendMessage Shrink
         )

  -- Expand the master area.
       , ( (modMask, xK_l)
         , sendMessage Expand
         )

  -- Push window back into tiling.
       , ( (modMask, xK_t)
         , withFocused $ windows . W.sink
         )

  -- Increment the number of windows in the master area.
       , ( (modMask, xK_comma)
         , sendMessage (IncMasterN 1)
         )

  -- Decrement the number of windows in the master area.
       , ((modMask, xK_period), sendMessage (IncMasterN (-1)))

  -- Toggle the status bar gap.
  -- TODO: update this binding with avoidStruts, ((modMask, xK_b),
       ]
    ++

  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
       [ ((m .|. modMask, k), windows $ f i)
       | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
       , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
       ]
    ++

  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
       -- [ ( (m .|. modMask, key)
       --   , screenWorkspace sc >>= flip whenJust (windows . f)
       --   )
       -- | (key, sc) <- zip [xK_w, xK_e, xK_r] [0 ..]
       -- , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
       -- ]


    -- ++
  -- Bindings for manage sub tabs in layouts please checkout the link below for reference
  -- https://hackage.haskell.org/package/xmonad-contrib-0.13/docs/XMonad-Layout-SubLayouts.html
       [
    -- Tab current focused window with the window to the left
         ( (modMask .|. controlMask, xK_h)
         , sendMessage $ pullGroup L
         )
    -- Tab current focused window with the window to the right
       , ( (modMask .|. controlMask, xK_l)
         , sendMessage $ pullGroup R
         )
    -- Tab current focused window with the window above
       , ( (modMask .|. controlMask, xK_k)
         , sendMessage $ pullGroup U
         )
    -- Tab current focused window with the window below
       , ( (modMask .|. controlMask, xK_j)
         , sendMessage $ pullGroup D
         )

  -- Tab all windows in the current workspace with current window as the focus
       , ( (modMask .|. controlMask, xK_m)
         , withFocused (sendMessage . MergeAll)
         )
  -- Group the current tabbed windows
       , ( (modMask .|. controlMask, xK_u)
         , withFocused (sendMessage . UnMerge)
         )

  -- Toggle through tabes from the right
       , ((modMask, xK_Tab), onGroup W.focusDown')
       ]

    ++
  -- Some bindings for BinarySpacePartition
  -- https://github.com/benweitzman/BinarySpacePartition
       [ ((modMask .|. controlMask, xK_Right), sendMessage $ ExpandTowards R)
       , ( (modMask .|. controlMask .|. shiftMask, xK_Right)
         , sendMessage $ ShrinkFrom R
         )
       , ((modMask .|. controlMask, xK_Left), sendMessage $ ExpandTowards L)
       , ( (modMask .|. controlMask .|. shiftMask, xK_Left)
         , sendMessage $ ShrinkFrom L
         )
       , ((modMask .|. controlMask, xK_Down), sendMessage $ ExpandTowards D)
       , ( (modMask .|. controlMask .|. shiftMask, xK_Down)
         , sendMessage $ ShrinkFrom D
         )
       , ((modMask .|. controlMask, xK_Up), sendMessage $ ExpandTowards U)
       , ( (modMask .|. controlMask .|. shiftMask, xK_Up)
         , sendMessage $ ShrinkFrom U
         )
       , ((modMask, xK_r), sendMessage BSP.Rotate)
       , ((modMask, xK_s), sendMessage BSP.Swap)
  -- , ((modMask,                               xK_n     ), sendMessage BSP.FocusParent)
  -- , ((modMask .|. controlMask,               xK_n     ), sendMessage BSP.SelectNode)
  -- , ((modMask .|. shiftMask,                 xK_n     ), sendMessage BSP.MoveNode)
       ]

------------------------------------------------------------------------
-- Mouse bindings
--
-- Focus rules
-- True if your focus should follow your mouse cursor.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myMouseBindings XConfig { XMonad.modMask = modMask } = M.fromList
  [
    -- mod-button1, Set the window to floating mode and move by dragging
    ( (modMask, button1)
    , \w -> focus w >> mouseMoveWindow w
    )

    -- mod-button2, Raise the window to the top of the stack
  , ( (modMask, button2)
    , \w -> focus w >> windows W.swapMaster
    )

    -- mod-button3, Set the window to floating mode and resize by dragging
  , ((modMask, button3), \w -> focus w >> mouseResizeWindow w)

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]


------------------------------------------------------------------------
-- Status bars and logging
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--


------------------------------------------------------------------------
-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
  setWMName "LG3D"
  spawn "bash ~/.xmonad/startup.sh"
  setDefaultCursor xC_left_ptr


------------------------------------------------------------------------
-- Run xmonad with all the defaults we set up.
--
main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.config"
  -- xmproc <- spawnPipe "taffybar"
  xmonad
    $ docks
    $ withNavigation2DConfig myNav2DConf
    $ additionalNav2DKeys
        (xK_Up, xK_Left, xK_Down, xK_Right)
        [(mod4Mask, windowGo), (mod4Mask .|. shiftMask, windowSwap)]
        False
    $ additionalNav2DKeys
        (xK_k, xK_h, xK_j, xK_l)
        [(mod4Mask, windowGo), (mod4Mask .|. shiftMask, windowSwap)]
        False
    $ ewmh
         -- $ pagerHints -- uncomment to use taffybar
    $ defaults
        { logHook =
          dynamicLogWithPP xmobarPP
              { ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
                              . wrap "[" "]"
              , ppTitle   = xmobarColor xmobarTitleColor "" . shorten 50
              , ppSep     = "   "
              , ppOutput  = hPutStrLn xmproc
              }
            >> updatePointer (0.75, 0.75) (0.75, 0.75)
        }

------------------------------------------------------------------------
-- Combine it all together
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = def {
    -- simple stuff
                 terminal           = myTerminal
               , focusFollowsMouse  = myFocusFollowsMouse
               , borderWidth        = myBorderWidth
               , modMask            = myModMask
               , workspaces         = myWorkspaces
               , normalBorderColor  = myNormalBorderColor
               , focusedBorderColor = myFocusedBorderColor

    -- key bindings
               , keys               = myKeys
               , mouseBindings      = myMouseBindings

    -- hooks, layouts
               , layoutHook         = myLayout
    -- handleEventHook    = E.fullscreenEventHook,
               , handleEventHook    = fullscreenEventHook
               , manageHook         = manageDocks <+> myManageHook
               , startupHook        = myStartupHook
               }
