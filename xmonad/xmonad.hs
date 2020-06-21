import           Data.List
import           Data.Maybe
import           Graphics.X11.ExtraTypes.XF86
import           System.Exit
import           System.IO
import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Actions.Navigation2D
import           XMonad.Actions.OnScreen
import           XMonad.Actions.PhysicalScreens
import           XMonad.Actions.SinkAll
import           XMonad.Actions.UpdatePointer
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops      ( ewmh )
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.WorkspaceHistory as WH
import           XMonad.Layout.BinarySpacePartition
                                               as BSP
import           XMonad.Layout.Grid
import           XMonad.Layout.IndependentScreens
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
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Paste
import           XMonad.Util.Run                ( spawnPipe )
import qualified Data.Map                      as Map
import qualified XMonad.StackSet               as W

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
  show WorkspaceWWW  = "1 \62056 " -- "1  "
  show WorkspaceWork = "2 \58911 " -- "2  "
  show WorkspaceTerm = "3 \61728 " -- "3  "
  show WorkspaceChat = "4 \61574 " -- "4  "
  show Workspace5    = "5 \62003 " -- "5  "
  show Workspace6    = "6 \63231 " -- "6  "

myWorkspaces :: [String]
myWorkspaces = show <$> [WorkspaceWWW ..]

renderWorkspace color ws = withColor clickable
 where
  index     = fromMaybe 0 $ elemIndex ws myWorkspaces
  action    = "xdotool key super+" ++ show (index + 1)
  clickable = "<action=`" ++ action ++ "`>" ++ ws ++ "</action>"
  withColor = xmobarColor color ""

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

zenMode = renamed [Replace "Zen"] $ addTopBar $ zenSpace Grid
 where
  zenSpace =
    spacingRaw False (Border 100 100 500 500) True (Border 10 10 10 10) True

layouts = bsp ||| oneBig ||| grid ||| zenMode

myLayout =
  mkToggle1 NBFULL $ avoidStruts $ smartBorders $ mkToggle1 FULL layouts

-- Theme --

myFont = "xft:Iosevka Nerd Font:size=14:bold:antialias=true"
active = "#ff79c6"
inactive = "#6272a4"
urgent = "#dc322f"
xmobarWs = "#6272a4"
xmobarWsActive = "#ff79c6"
xmobarWsUrgent = "#ff5555"
xmobarWsInactive = "#44475a"
xmobarWsSep = "#44475a"
xmobarTitle = "#8be9fd"
xmobarLayout = "#ffb86c"

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

myKeys nScreens conf@XConfig { modMask = modMask, terminal = terminal, workspaces = workspaces }
  = Map.fromList
    $  [ ((modMask, xK_Return)                , spawn terminal)
       , ((modMask, xK_n)                     , fileBrowser)
       , ((modMask .|. shiftMask, xK_Return)  , quteWebBrowser)
       , ((modMask .|. controlMask, xK_Return), chromiumWebBrowser)
       , ((modMask, xK_Escape)                , kill)
       , ((modMask .|. shiftMask, xK_Escape)  , kill)
       , ((modMask, xK_q)                     , kill)
       , ((altMask, xK_F4)                    , kill)
       , ((modMask, xK_d)                     , spawn rofiApplications)
       , ((modMask .|. shiftMask, xK_d)       , spawn rofiRun)
       , ((nothing, xF86XK_PowerDown)         , poweroffComputer)
       , ((nothing, xF86XK_PowerOff)          , poweroffComputer)
       , ((controlMask .|. modMask .|. altMask, xK_Escape), poweroffComputer)
       , ((modMask .|. altMask, xK_l)         , spawn "lock")
       , ((controlMask .|. modMask .|. altMask, xK_BackSpace), spawn "lock")
       , ((controlMask .|. modMask .|. altMask, xK_space), suspend)
       , ((modMask, xK_Tab)                   , moveTo Next NonEmptyWS)
       , ((modMask .|. shiftMask, xK_Tab)     , moveTo Prev NonEmptyWS)
       , ((modMask, xK_i)                     , toggleLastWorkspace nScreens)
       , ((modMask, xK_p)                     , spawn passwordTool)
       , ((modMask .|. altMask, xK_p)         , spawn passwordTool)
       , ((modMask .|. altMask, xK_s)         , spawn "script-rofi")
       , ((nothing, xK_Print)                 , spawn screenshotClipboard)
       , ((altMask, xK_Print)                 , spawn screenshotWholeScreen)
       , ((shiftMask, xK_Print)               , spawn screenshotFile)
       , ((nothing, xF86XK_MonBrightnessUp)   , setBrightness "1.0")
       , ((nothing, xF86XK_MonBrightnessDown) , setBrightness "0.6")
       , ((nothing, xK_Insert)                , pasteSelection)
       , ((modMask, xK_f)                     , sendMessage $ Toggle FULL)
       , ((modMask .|. shiftMask, xK_f)       , sendMessage $ Toggle NBFULL)
       , ((modMask, xK_space)                 , sendMessage NextLayout)
       , ((modMask .|. controlMask, xK_space) , sinkAll)
       , ((modMask .|. shiftMask, xK_space)   , setLayout (layoutHook conf))
       , ((modMask .|. controlMask, xK_l)     , sendMessage $ ExpandTowards R)
       , ((modMask .|. controlMask, xK_h)     , sendMessage $ ExpandTowards L)
       , ((modMask .|. controlMask, xK_j)     , sendMessage $ ExpandTowards D)
       , ((modMask .|. controlMask, xK_k)     , sendMessage $ ExpandTowards U)
       , ((modMask, xK_r)                     , sendMessage BSP.Rotate)
       , ((modMask, xK_s)                     , sendMessage BSP.Swap)
       , ((modMask .|. altMask, xK_0)         , updateMonitors)
       , ((modMask .|. altMask, xK_1)         , setMonitors 1)
       , ((modMask .|. altMask, xK_2)         , setMonitors 2)
       , ((modMask .|. altMask, xK_3)         , setMonitors 3)
       , ((modMask .|. altMask, xK_4)         , setMonitors 4)
       , ((modMask .|. altMask, xK_5)         , setMonitors 5)
       , ((modMask .|. altMask, xK_r)         , restartXmonad)
       , ((modMask .|. shiftMask, xK_r)       , restartXmonad)
       , ((modMask, xK_e)                     , spawn "emoji")
       , ((modMask .|. altMask, xK_e)         , exitXmonad)
       , ((modMask .|. shiftMask, xK_e)       , exitXmonad)
       , ((modMask .|. altMask, xK_i)         , invertXColors)
       , ( (modMask, xK_minus)
         , namedScratchpadAction scratchPads (name $ head scratchPads)
         )
       , ( (modMask .|. shiftMask, xK_minus)
         , namedScratchpadAction scratchPads (name $ last scratchPads)
         )
       ]
    ++ [ ((modifier, key), action workspace)
       | (workspace, key   ) <- zip workspaces [xK_1 .. xK_6]
       , (modifier , action) <-
         [ (modMask              , viewWorkspace nScreens)
         , (modMask .|. shiftMask, moveToWorkspace)
         ]
       ]

-- Mouse Bindings --

myMouseBindings XConfig { modMask = modMask } = Map.fromList
  [ ( (modMask .|. controlMask, button1)
    , \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster
    )
  , ((modMask .|. controlMask, button2), \w -> focus w >> windows W.shiftMaster)
  , ( (modMask .|. controlMask, button3)
    , \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster
    )
  ]

-- Commands --

term = "st"
fileBrowser = spawn "xdg-open ."
quteWebBrowser = spawn "qutebrowser"
chromiumWebBrowser = spawn "chromium-browser"
rofiApplications = "rofi -modi drun,run -show drun -show-icons"
rofiRun = "rofi -show run -i -display-run \"$ \""
passwordTool = "lastpass-rofi || keepassx || exit 1"

screenshotFile = "maim -s --hidecursor ~/Pictures/screenshot-$(date +%s).png"
screenshotWholeScreen =
  "maim --hidecursor ~/Pictures/screenshot-$(date +%s).png"
screenshotClipboard =
  "maim -s --hidecursor --format png /dev/stdout | xclip -selection clipboard -t image/png"

setBrightness b = spawn $ brightness "eDP-1" b ++ " || " ++ brightness "eDP1" b
  where brightness d b = "xrandr --output " ++ d ++ " --brightness " ++ b

invertXColors = spawn "xrandr-invert-colors"

suspend = spawn "systemctl suspend"
poweroffComputer = confirm "poweroff" $ spawn "poweroff"
exitXmonad = confirm "exit xmonad and logoff" $ io exitSuccess
restartXmonad = restart "xmonad" True

setMonitors :: Int -> X ()
setMonitors i = spawn ("autorandr --load " <> show i)

updateMonitors = spawn "autorandr --change"

viewWorkspace nScreens workspace = do
  screenId <- toScreenId nScreens workspace
  windows $ viewOnScreen screenId workspace

moveToWorkspace = windows . W.shift

toggleLastWorkspace nScreens = do
  (_ : lastWorkspace : _) <- WH.workspaceHistory
  viewWorkspace nScreens lastWorkspace

confirm = confirmPrompt conf
 where
  conf =
    greenXPConfig { font = myFont, height = 60, position = CenteredAt 0.5 0.3 }

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

-- Scratchpads --

scratchPads :: [NamedScratchpad]
scratchPads = [createSP "sp_primary", createSP "sp_secondary"]
 where
  createSP name =
    let spawnTerm  = term ++ " -n " ++ name
        findTerm   = resource =? name
        manageTerm = customFloating $ W.RationalRect 0.2 0.2 0.6 0.6
    in  NS name spawnTerm findTerm manageTerm

-- Window rules --

myManageHook = composeAll
  [ resource =? "desktop_window" --> doIgnore
  , resource =? "stalonetray" --> doIgnore
  , className =? "Chromium-browser" --> doShift (show WorkspaceWWW)
  , className =? "Code" --> doShift (show WorkspaceWork)
  , className =? "Rambox" --> doShift (show WorkspaceChat)
  , className =? "Screenruler" --> doFloat
  , className =? "Slack" --> doShift (show WorkspaceChat)
  , className =? "Zenity" --> doFloat
  , className =? "jetbrains-idea" --> doShift (show WorkspaceWork)
  , className =? "jetbrains-idea-ce" --> doShift (show WorkspaceWork)
  , className =? "qutebrowser" --> doShift (show WorkspaceWWW)
  , title =? "Battery Warning" --> doFloat
  ]

myNav2DConf = def { defaultTiledNavigation = centerNavigation
                  , floatNavigation        = centerNavigation
                  , screenNavigation       = lineNavigation
                  , layoutNavigation       = [("Full", centerNavigation)]
                  , unmappedWindowRect     = [("Full", singleWindowRect)]
                  }

-- Startup --

myStartupHook = do
  setWMName "LG3D"
  spawn "bash ~/.xmonad/startup.sh"

monitorSetupHook nScreens workspaces = mconcat
  (updateScreen nScreens <$> reverse workspaces)
 where
  updateScreen nScreens workspace = do
    screenId <- toScreenId nScreens workspace
    windows $ viewOnScreen screenId workspace

myLogHook xmproc =
  myXmobar xmproc
    >> updatePointer (0.75, 0.75) (0.75, 0.75)
    >> WH.workspaceHistoryHook

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
        , logHook            = myLogHook xmproc
        , terminal           = term
        , focusFollowsMouse  = True
        , borderWidth        = 0
        , modMask            = mod4Mask
        , workspaces         = myWorkspaces
        , normalBorderColor  = active
        , focusedBorderColor = inactive
        , layoutHook         = myLayout
        , manageHook         = manageDocks
                               <+> myManageHook
                               <+> namedScratchpadManageHook scratchPads
        , startupHook = myStartupHook <+> monitorSetupHook nScreens myWorkspaces
        }

myXmobar xmproc = dynamicLogWithPP xmobarPP
  { ppCurrent         = renderWorkspace xmobarWsActive
  , ppVisible         = renderWorkspace xmobarWs
  , ppHidden          = renderWorkspace xmobarWs
  , ppHiddenNoWindows = renderWorkspace xmobarWsInactive
  , ppUrgent          = renderWorkspace xmobarWsUrgent
  , ppLayout          = xmobarColor xmobarLayout ""
  , ppTitle           = xmobarColor xmobarTitle "" . shorten 80
  , ppWsSep           = xmobarColor xmobarWsSep "" " | "
  , ppSep             = replicate 6 ' '
  , ppOutput          = hPutStrLn xmproc
  }
