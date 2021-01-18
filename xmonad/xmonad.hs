import           Data.List
import qualified Data.Map                      as Map
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
import           XMonad.Util.Paste
import           XMonad.Util.Run                ( spawnPipe )
import           XMonad.Util.Scratchpad
import           XMonad.Util.WorkspaceCompare

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

screenBorder' = Border 5 5 5 5
windowBorder' = Border 10 10 10 10
addSpace = spacingRaw True screenBorder' True windowBorder' True

addTopBar = noFrillsDeco shrinkText topBarTheme

bsp =
  renamed [CutWordsLeft 1]
    $ addTopBar
    $ windowNavigation
    $ renamed [Replace "BSP"]
    $ addTabs shrinkText topBarTheme
    $ subLayout [] Simplest
    $ addSpace BSP.emptyBSP

grid = renamed [Replace "Grid"] $ addTopBar $ addSpace Grid

oneBig = renamed [Replace "OneBig"] $ addTopBar $ addSpace $ OneBig 0.75 0.65

zen = renamed [Replace "Zen"] $ addTopBar $ zenSpace Grid
 where
  zenScreenBorder = Border 100 100 500 500
  zenSpace        = spacingRaw False zenScreenBorder True windowBorder' True

layouts = bsp ||| oneBig ||| grid ||| zen

myLayout = mkToggle1 NBFULL $ avoidStruts $ mkToggle1 FULL layouts

-- Theme --

myFont = "xft:Iosevka Nerd Font:size=12:bold:antialias=true"
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
                  , activeColor         = active
                  , inactiveColor       = inactive
                  , urgentColor         = urgent
                  , activeTextColor     = active
                  , inactiveTextColor   = inactive
                  , urgentTextColor     = urgent
                  , activeBorderColor   = active
                  , inactiveBorderColor = inactive
                  , urgentBorderColor   = urgent
                  , decoHeight          = 6
                  }

-- Key bindings --

altMask = mod1Mask
nothing = 0


myKeys nScreens conf@XConfig { modMask = modMask, terminal = terminal, workspaces = workspaces }
  = Map.fromList
    $  [ ((modMask, xK_Return)                , spawn terminal)
       , ((modMask, xK_n)                     , fileBrowser)
       , ((modMask .|. altMask, xK_n)         , spawn "dunstctl close-all")
       , ((modMask .|. shiftMask, xK_Return)  , quteWebBrowser)
       , ((modMask .|. controlMask, xK_Return), chromiumWebBrowser)
       , ((modMask, xK_Escape)                , kill)
       , ((modMask .|. shiftMask, xK_Escape)  , kill)
       , ((modMask, xK_q)                     , kill)
       , ((modMask, xK_BackSpace)             , kill)
       , ((altMask, xK_F4)                    , kill)
       , ((modMask, xK_d)                     , spawn rofiApplications)
       , ((modMask .|. shiftMask, xK_d)       , spawn rofiRun)
       , ((nothing, xF86XK_PowerDown)         , poweroffComputer)
       , ((nothing, xF86XK_PowerOff)          , poweroffComputer)
       , ((controlMask .|. modMask .|. altMask, xK_Escape), poweroffComputer)
       , ((modMask .|. altMask, xK_l)         , spawn "lock")
       , ((controlMask .|. modMask .|. altMask, xK_BackSpace), spawn "lock")
       , ((controlMask .|. modMask .|. altMask, xK_space), suspend)
       , ((modMask, xK_Tab), workspaceInDirection nScreens Next)
       , ((modMask, xK_Shift_R), workspaceInDirection nScreens Next)
       , ((modMask .|. shiftMask, xK_Tab), workspaceInDirection nScreens Prev)
       , ((modMask, xK_i)                     , toggleLastWorkspace nScreens)
       , ((modMask, xK_p)                     , spawn passwordTool)
       , ((modMask .|. altMask, xK_p)         , spawn passwordTool)
       , ((modMask .|. shiftMask, xK_p)       , spawn otpTool)
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
       , ((controlMask .|. modMask .|. altMask, xK_Shift_R), updateMonitors)
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
       , ((modMask, xK_minus)                 , toggleScratchpad)
       , ((controlMask, xK_F2)                , suspend)
       , ((controlMask, xK_F3)                , sendKey controlMask xK_d)
       , ((controlMask, xK_F4)                , typeText "¯\\_(ツ)_/¯")
       , ((controlMask, xK_F6)                , typeText "(┛ಠ_ಠ)┛彡┻━┻")
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
chromiumWebBrowser = spawn "chromium"
rofiApplications = "rofi -modi drun,run -show drun -show-icons"
rofiRun = "rofi -show run -i -display-run \"$ \""
passwordTool = "lastpass-rofi || keepassx || exit 1"
otpTool = "passmenu-otp --type || exit 1"

screenshotFile =
  "maim -s --hidecursor ~/Pictures/screenshots/screenshot-$(date +%s).png"
screenshotWholeScreen =
  "maim --hidecursor ~/Pictures/screenshots/screenshot-$(date +%s).png"
screenshotClipboard =
  "maim -s --hidecursor --format png /dev/stdout | xclip -selection clipboard -t image/png"

setBrightness b = spawn $ brightness "eDP-1" b ++ " || " ++ brightness "eDP1" b
  where brightness d b = "xrandr --output " ++ d ++ " --brightness " ++ b

invertXColors = spawn "xrandr-invert-colors"
suspend = spawn "systemctl suspend"
poweroffComputer = confirm "poweroff" $ spawn "poweroff"
exitXmonad = confirm "exit xmonad and logoff" $ io exitSuccess
restartXmonad = restart "xmonad" True
setMonitors i = spawn ("autorandr --load " <> show (i :: Int))
updateMonitors = spawn "autorandr --change"
typeText x = spawn $ "xdotool sleep 0.3 type '" ++ x ++ "'"

viewWorkspace nScreens workspace = do
  screenId <- toScreenId nScreens workspace
  windows $ viewOnScreen screenId workspace

moveToWorkspace = windows . W.shift

toggleScratchpad = scratchpadSpawnActionCustom $ term ++ " -s -n scratchpad"

toggleLastWorkspace nScreens = do
  (_ : lastWorkspace : _) <- WH.workspaceHistory
  viewWorkspace nScreens lastWorkspace

workspaceInDirection nScreens direction = doTo
  direction
  NonEmptyWS
  ((. scratchpadFilterOutWorkspace) <$> getSortByIndex)
  (viewWorkspace nScreens)

confirm = confirmPrompt c
 where
  c = def { font              = myFont
          , position          = Top
          , height            = 25
          , promptBorderWidth = 0
          , fgColor           = "#f1fa8c"
          , bgColor           = "#282a36"
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

-- Window rules --

myManageHook = composeAll
  [ resource =? "desktop_window" --> doIgnore
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

scratchpadHook = scratchpadManageHook (W.RationalRect 0.2 0.2 0.6 0.6)

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
        , normalBorderColor  = inactive
        , focusedBorderColor = active
        , layoutHook         = myLayout
        , manageHook         = manageDocks <+> myManageHook <+> scratchpadHook
        , startupHook = myStartupHook <+> monitorSetupHook nScreens myWorkspaces
        }

myXmobar xmproc = dynamicLogWithPP xmobarPP
  { ppCurrent         = renderWorkspace xmobarWsActive
  , ppVisible         = renderWorkspace xmobarWs
  , ppHidden          = renderWorkspace xmobarWs
  , ppHiddenNoWindows = renderWorkspace xmobarWsInactive
  , ppUrgent          = renderWorkspace xmobarWsUrgent
  , ppLayout          = xmobarColor xmobarLayout ""
  , ppTitle           = xmobarColor xmobarTitle "" . shorten 50
  , ppWsSep           = xmobarColor xmobarWsSep "" " | "
  , ppSep             = replicate 6 ' '
  , ppOutput          = hPutStrLn xmproc
  }
