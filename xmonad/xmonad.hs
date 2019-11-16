import           System.Exit
import           System.IO

import           Data.List
import qualified Data.Map
import           Data.Maybe
import           Graphics.X11.ExtraTypes.XF86
import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Actions.Navigation2D
import           XMonad.Actions.OnScreen
import           XMonad.Actions.PhysicalScreens
import           XMonad.Actions.SinkAll
import           XMonad.Actions.UpdatePointer
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops                ( ewmh )
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.WorkspaceHistory as WH
import           XMonad.Layout.BinarySpacePartition
                                               as BSP
import           XMonad.Layout.Grid
import           XMonad.Layout.IndependentScreens         ( countScreens )
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders
import           XMonad.Layout.NoFrillsDecoration
import           XMonad.Layout.OneBig
import           XMonad.Layout.Renamed
import           XMonad.Layout.Simplest
import           XMonad.Layout.Spacing
import           XMonad.Layout.SubLayouts
import           XMonad.Layout.Tabbed
import           XMonad.Layout.WindowNavigation
import           XMonad.Prompt
import           XMonad.Prompt.ConfirmPrompt
import qualified XMonad.StackSet               as W
import           XMonad.Util.Cursor
import           XMonad.Util.Paste
import           XMonad.Util.Run                          ( spawnPipe )

-- Workspaces --
data Workspace
  = WorkspaceWWW
  | WorkspaceWork
  | WorkspaceTerm
  | WorkspaceChat
  | Workspace5
  | Workspace6
  deriving (Enum)

instance Show Workspace where
  show WorkspaceWWW  = "1 \62056" -- "1  "
  show WorkspaceWork = "2 \58911" -- "2  "
  show WorkspaceTerm = "3 \61728" -- "3  "
  show WorkspaceChat = "4 \61574" -- "4  "
  show Workspace5    = "5 \62003" -- "5  "
  show Workspace6    = "6 \63231" -- "6  "

myWorkspaces :: [String]
myWorkspaces = show <$> [WorkspaceWWW ..]

toClickableWorkspace ws =
  "<action=`xdotool key super+" ++ show (index + 1) ++ "`>" ++ ws ++ "</action>"
  where index = fromMaybe 0 $ elemIndex ws myWorkspaces

-- Window rules --
myManageHook = composeAll
  [ resource =? "desktop_window" --> doIgnore
  , className =? "Chromium-browser" --> doShift (show WorkspaceWWW)
  , className =? "Code" --> doShift (show WorkspaceWork)
  , className =? "Rambox" --> doShift (show WorkspaceChat)
  , className =? "Screenruler" --> doFloat
  , className =? "Slack" --> doShift (show WorkspaceChat)
  , className =? "jetbrains-idea" --> doShift (show WorkspaceWork)
  , className =? "jetbrains-idea-ce" --> doShift (show WorkspaceWork)
  , className =? "qutebrowser" --> doShift (show WorkspaceWWW)
  ]

-- Layouts --
addSpace = spacingRaw True (Border 5 5 5 5) True (Border 10 10 10 10) True

addTopBar = noFrillsDeco shrinkText topBarTheme

bsp =
  renamed [CutWordsLeft 1]
    $ addTopBar
    $ windowNavigation
    $ renamed [Replace "BSP"]
    $ addTabs shrinkText myTabTheme
    $ subLayout [] Simplest
    $ addSpace BSP.emptyBSP

grid = renamed [Replace "Grid"] $ addTopBar $ addSpace Grid

oneBig = renamed [Replace "OneBig"] $ addTopBar $ addSpace $ OneBig 0.75 0.65

layouts = bsp ||| oneBig ||| grid

myLayout =
  avoidStruts $ smartBorders $ mkToggle (NOBORDERS ?? FULL ?? EOT) layouts

myNav2DConf = def { defaultTiledNavigation = centerNavigation
                  , floatNavigation        = centerNavigation
                  , screenNavigation       = lineNavigation
                  , layoutNavigation       = [("Full", centerNavigation)]
                  , unmappedWindowRect     = [("Full", singleWindowRect)]
                  }

-- Theme --
active = "#ff79c6"
inactive = "#6272a4"
urgent = "#dc322f"
xmobarActiveWorkspaceColor = "#ff79c6"
xmobarTitleColor = "#8be9fd"
xmobarLayoutColor = "#ffb86c"

myFont = "xft:SauceCodePro Nerd Font:size=10:bold:antialias=true"

topBarTheme = def { fontName            = myFont
                  , inactiveBorderColor = inactive
                  , inactiveColor       = inactive
                  , inactiveTextColor   = inactive
                  , activeBorderColor   = active
                  , activeColor         = active
                  , activeTextColor     = active
                  , urgentBorderColor   = urgent
                  , urgentTextColor     = urgent
                  , decoHeight          = 6
                  }

myTabTheme = def { fontName            = myFont
                 , activeColor         = active
                 , activeTextColor     = active
                 , inactiveColor       = inactive
                 , inactiveTextColor   = inactive
                 , activeBorderColor   = active
                 , inactiveBorderColor = inactive
                 }

-- Key bindings --
altMask = mod1Mask

nothing = 0

confirm = confirmPrompt amberXPConfig

fileBrowser = spawn "xdg-open ."

quteWebBrowser = spawn "qutebrowser"
defaultWebBrowser = spawn "x-www-browser"

rofiApplications = "rofi -modi drun,run -show drun -show-icons"

rofiRun = "rofi -show run -i -display-run \"$ \""

lock = "i3lock-fancy -p"

rofiPass =
  "gopass ls --flat | rofi -dmenu | xargs --no-run-if-empty gopass show -c"

screenshotFile = "maim -s --hidecursor ~/Pictures/screenshot-$(date +%s).png"

screenshotClipboard =
  "maim -s --hidecursor --format png /dev/stdout | xclip -selection clipboard -t image/png"

poweroffComputer = confirm "poweroff" $ spawn "poweroff"

brightnessUp = spawn "xrandr --output eDP-1 --brightness 1.0"

brightnessDown = spawn "xrandr --output eDP-1 --brightness 0.6"

exitXmonad = confirm "exit xmonad and logoff" $ io exitSuccess

restartXmonad = restart "xmonad" True

setMonitors :: Int -> X ()
setMonitors i = spawn ("autorandr -l " <> show i)

updateMonitors = spawn "autorandr -c"

invertXColors = spawn "xrandr-invert-colors"

viewWorkspace nScreens workspace = do
  screenId <- toScreenId nScreens workspace
  windows $ viewOnScreen screenId workspace

moveToWorkspace = windows . W.shift

toggleLastWorkspace nScreens = do
  (_ : lastWorkspace : _) <- WH.workspaceHistory
  viewWorkspace nScreens lastWorkspace

myKeys nScreens conf@XConfig { modMask = modMask, terminal = terminal, workspaces = workspaces }
  = Data.Map.fromList
    $  [ ((modMask, xK_Return)                       , spawn terminal)
       , ((modMask, xK_n)                            , fileBrowser)
       , ((modMask .|. shiftMask, xK_Return)         , quteWebBrowser)
       , ((modMask .|. controlMask, xK_Return)       , defaultWebBrowser)
       , ((modMask, xK_Escape)                       , kill)
       , ((modMask .|. shiftMask, xK_Escape)         , kill)
       , ((modMask, xK_q)                            , kill)
       , ((altMask, xK_F4)                           , kill)
       , ((modMask, xK_d)                            , spawn rofiApplications)
       , ((modMask .|. shiftMask, xK_d)              , spawn rofiRun)
       , ((modMask .|. shiftMask, xK_e)              , exitXmonad)
       , ((modMask .|. shiftMask, xK_r)              , restartXmonad)
       , ((nothing, xF86XK_PowerDown)                , poweroffComputer)
       , ((nothing, xF86XK_PowerOff)                 , poweroffComputer)
       , ((controlMask .|. modMask .|. altMask, xK_p), poweroffComputer)
       , ((modMask .|. altMask, xK_l)                , spawn lock)
       , ((modMask, xK_Tab)                          , moveTo Next NonEmptyWS)
       , ((modMask .|. shiftMask, xK_Tab)            , moveTo Prev NonEmptyWS)
       , ((modMask, xK_i), toggleLastWorkspace nScreens)
       , ((modMask .|. altMask, xK_p)                , spawn rofiPass)
       , ((nothing, xK_Print), spawn screenshotClipboard)
       , ((shiftMask, xK_Print)                      , spawn screenshotFile)
       , ((nothing, xF86XK_MonBrightnessUp)          , brightnessUp)
       , ((nothing, xF86XK_MonBrightnessDown)        , brightnessDown)
       , ((nothing, xK_Insert)                       , pasteSelection)
       , ((modMask, xK_f), sendMessage $ Toggle FULL)
       , ((modMask .|. shiftMask, xK_f)              , sendMessage ToggleStruts)
       , ((modMask, xK_space)                        , sendMessage NextLayout)
       , ((modMask .|. controlMask, xK_space)        , sinkAll)
       , ((modMask .|. shiftMask, xK_space), setLayout (layoutHook conf))
       , ((modMask .|. controlMask, xK_l), sendMessage $ ExpandTowards R)
       , ((modMask .|. controlMask, xK_h), sendMessage $ ExpandTowards L)
       , ((modMask .|. controlMask, xK_j), sendMessage $ ExpandTowards D)
       , ((modMask .|. controlMask, xK_k), sendMessage $ ExpandTowards U)
       , ((modMask, xK_r)                            , sendMessage BSP.Rotate)
       , ((modMask, xK_s)                            , sendMessage BSP.Swap)
       , ((modMask .|. altMask, xK_0)                , updateMonitors)
       , ((modMask .|. altMask, xK_1)                , setMonitors 1)
       , ((modMask .|. altMask, xK_2)                , setMonitors 2)
       , ((modMask .|. altMask, xK_3)                , setMonitors 3)
       , ((modMask .|. altMask, xK_r)                , restartXmonad)
       , ((modMask .|. altMask, xK_i)                , invertXColors)
       ]
    ++ [ ((modifier, key), action workspace)
       | (workspace, key   ) <- zip workspaces [xK_1 .. xK_6]
       , (modifier , action) <-
         [ (modMask              , viewWorkspace nScreens)
         , (modMask .|. shiftMask, moveToWorkspace)
         ]
       ]

-- Mouse Bindings --
myMouseBindings XConfig { modMask = modMask } = Data.Map.fromList
  [ ( (modMask .|. controlMask, button1)
    , \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster
    )
  , ((modMask .|. controlMask, button2), \w -> focus w >> windows W.shiftMaster)
  , ( (modMask .|. controlMask, button3)
    , \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster
    )
  ]

-- Startup --
myStartupHook = do
  setWMName "LG3D"
  spawn "bash ~/.xmonad/startup.sh"
  setDefaultCursor xC_left_ptr

monitorSetupHook nScreens workspaces = mconcat
  (updateScreen nScreens <$> reverse workspaces)
 where
  updateScreen nScreens workspace = do
    screenId <- toScreenId nScreens workspace
    windows $ viewOnScreen screenId workspace

-- Main --
main = do
  xmproc   <- spawnPipe "xmobar ~/.xmonad/xmobar.config"
  nScreens <- countScreens
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
    $ def
        { keys               = myKeys nScreens
        , mouseBindings      = myMouseBindings
        , logHook            = myXmobar xmproc
                               >> updatePointer (0.75, 0.75) (0.75, 0.75)
                               >> WH.workspaceHistoryHook
        , terminal           = "x-terminal-emulator"
        , focusFollowsMouse  = True
        , borderWidth        = 0
        , modMask            = mod4Mask
        , workspaces         = myWorkspaces
        , normalBorderColor  = active
        , focusedBorderColor = inactive
        , layoutHook         = myLayout
        , manageHook         = manageDocks <+> myManageHook
        , startupHook = myStartupHook <+> monitorSetupHook nScreens myWorkspaces
        }

myXmobar xmproc = dynamicLogWithPP xmobarPP
  { ppCurrent = xmobarColor xmobarActiveWorkspaceColor ""
  , ppVisible = toClickableWorkspace
  , ppHidden  = toClickableWorkspace
  , ppLayout  = xmobarColor xmobarLayoutColor ""
  , ppUrgent  = xmobarColor urgent ""
  , ppTitle   = xmobarColor xmobarTitleColor "" . shorten 50
  , ppWsSep   = " | "
  , ppSep     = replicate 6 ' '
  , ppOutput  = hPutStrLn xmproc
  }

-- Screens --
toScreenId :: Int -> String -> X ScreenId
toScreenId nScreens ws = fromMaybe (S 0)
  <$> getScreen horizontalScreenOrderer (toPhysicalScreen nScreens ws)

toPhysicalScreen :: Int -> String -> PhysicalScreen
toPhysicalScreen 2 ws | ws == show WorkspaceWork = P 1
                      | otherwise                = P 0
toPhysicalScreen 3 ws | ws == show WorkspaceWWW  = P 0
                      | ws == show WorkspaceWork = P 1
                      | otherwise                = P 2
toPhysicalScreen _ _ = P 0
