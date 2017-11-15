-- import           XMonad.Layout.Gaps
import           Data.Monoid
import           Data.Word
import           System.IO
import           XMonad
-- import           XMonad.Actions.Eval
import           XMonad.Actions.SpawnOn
import           XMonad.Actions.Warp
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.FadeInactive
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Spacing
import           XMonad.Layout.Tabbed
import           XMonad.Prompt
import           XMonad.Prompt.Shell
import           XMonad.Prompt.AppendFile
import           XMonad.Prompt.Input
import           XMonad.Prompt.Pass
import           XMonad.Prompt.XMonad
import           XMonad.Util.EZConfig       (additionalKeys, additionalKeysP,    removeKeys)
import           XMonad.Util.NamedWindows
import           XMonad.Util.Paste
import           XMonad.Util.Run            (spawnPipe, safeSpawn)
import           Graphics.X11.Xlib
import           Graphics.X11.Xlib.Extras
import           System.Exit
import qualified Data.List                  as L (group)
import qualified Data.Map as M
import qualified XMonad.StackSet            as S

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar /home/emmanuel/.xmobarrc"
  -- I need to modify so that it only spawns the process if it is not
  -- already running.

  -- spawn "compton"
  xmonad $
    withUrgencyHook LibNotifyUrgencyHook $
    docks $
    def
    { terminal = myTerminal
    , modMask = myModMask
    , keys = myKeys
    , workspaces = myWorkSpaces
    , borderWidth = myBorderWidth
    , focusFollowsMouse = myFocusFollowsMouse
    , normalBorderColor = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , handleEventHook = fullscreenEventHook <+> setTransparentHook
    -- , manageHook = myManageHook <+> manageHook def
    , layoutHook = avoidStruts myLayout
    , logHook =
      dynamicLogWithPP
        xmobarPP
        { ppOutput = hPutStrLn xmproc
        , ppTitle = xmobarColor "green" "" . shorten 50
        , ppSep = " | "
        } <+>
      myLogHook
    } `additionalKeysP`
    myEmacsKeys

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {modMask = modm}) =
  M.fromList $
  [ ((modm .|. shiftMask, xK_Return), spawn $ terminal conf)
  , ((modm, xK_o), spawn "dmenu_run")
  , ((modm .|. shiftMask, xK_o), spawn "gmrun")
  , ((modm .|. shiftMask, xK_c), kill)
  , ((modm, xK_space), sendMessage NextLayout)
  , ((modm .|. shiftMask, xK_space), setLayout $ layoutHook conf)
  , ((modm, xK_r), refresh)
  , ((modm, xK_Tab), windows S.focusDown)
  , ((modm .|. shiftMask, xK_Tab), windows S.focusUp)
  , ((modm, xK_t), windows S.focusDown)
  , ((modm, xK_n), windows S.focusUp)
  , ((modm, xK_m), windows S.focusMaster)
  , ((modm, xK_Return), windows S.swapMaster)
  , ((modm .|. shiftMask, xK_t), windows S.swapDown)
  , ((modm .|. shiftMask, xK_n), windows S.swapUp)
  , ((modm, xK_h), sendMessage Shrink)
  , ((modm, xK_s), sendMessage Expand)
  , ((modm .|. controlMask, xK_t), withFocused $ windows . S.sink)
  , ((modm, xK_comma), sendMessage (IncMasterN 1))
  , ((modm, xK_period), sendMessage (IncMasterN (-1)))
  , ((modm .|. shiftMask, xK_q), io (exitWith ExitSuccess))
  , ((modm, xK_q), spawn "xmonad --recompile; xmonad --restart")
  , ((modm .|. shiftMask, xK_c), kill)
  , ((modm .|. shiftMask, xK_z), spawn "xlock -mode matrix")
  , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
  , ((modm, xK_Right), spawn "amixer sset Master 4%+")
  , ((modm, xK_Left), spawn "amixer sset Master 4%-")
  , ((modm, xK_Down), spawn "amixer sset Master toggle")
  , ( (modm .|. controlMask, xK_n)
    , appendFilePrompt def "/home/emmanuel/Dropbox/org/DumpList.org")
  , ((0, xK_Print), spawn "scrot")
  , ((modm .|. controlMask, xK_x), xmonadPrompt def)
  , ((0, xK_Insert), pasteSelection)]

-- Misc. Keys
myEmacsKeys :: [(String,X ())]
myEmacsKeys =
  zip viewKeys (worker S.greedyView myWorkSpaces) ++
  zip shiftKeys (worker S.shift myWorkSpaces) ++ miscKeys
  where modWorkChord = "M4-c" ++ " "
        (viewKeys,shiftKeys) =
          (,) <$> map (modWorkChord ++) <*> map ((++) $ modWorkChord ++ "S-") $
          baseWorkSpaceKeys
        baseWorkSpaceKeys = L.group "htnsgcrl*"
        worker f = map (windows . f)
        miscKeys =
          [(modWorkChord ++ "e",spawn "emacsclient -c")
          ,("<XF86AudioRaiseVolume>",spawn "amixer sset Master 4%+")
          ,("<XF86AudioLowerVolume>",spawn "amixer sset Master 4%-")
          ,("<XF86AudioMute>",spawn "amixer sset Master toggle")
          ,("M4-g g",spawnHere (myTerminal ++ " -e ghci"))
          ,("M4-g f",spawnHere "firefox")
          ,("M4-g c",spawnHere "chromium")
          ,("M4-g z",spawnHere "zathura")
          ,("M4-g q",spawnHere "qutebrowser")
          ,("M4-b",banish UpperRight)
          ,("M4-S-b",banishScreen UpperRight)]

-- Hooks to manage floating windows
myManageHook
  :: Query (Endo WindowSet)
myManageHook =
  composeAll
    [isFullscreen --> doFloat
    ,className =? "Gimp" --> doFloat
    ,className =? "VNC Viewer" --> doFloat
    ,className =? "hl_linux" --> doFloat
    ,className =? "Vlc" --> doFloat
    ,className =? "Firefox" --> doShift "firefox/qutebrowser"
    ,className =? "chromium" --> doShift "chrome-netflix"
    ,(className =? "Firefox" <&&> resource =? "Dialog") --> doFloat
    ,className =? "Xmessage" --> doFloat]

myTerminal :: String
myTerminal = "urxvt"
-- myTerminal = "gnome-terminal"
-- myTerminal = "urxvt -fn otf:Source Code Pro:size12:autohint=true:antialias=true"
-- myTerminal = "dbus-launch gnome-terminal" -- this is a bandaid.
-- myTerminal = "gnome-terminal"

myWorkSpaces :: [String]
myWorkSpaces =
  ["firefox/qutebrowser"
  ,"prog-mode"
  ,"erc"
  ,"zsh"
  ,"org-latex"
  ,"zathura/prog-mode2"
  ,"chrome-netflix"
  ,"random"
  ,"zathura/prog-mode3"]

myModMask :: KeyMask
myModMask = mod4Mask

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- Border colors for unfocused and focused windows.
myNormalBorderColor :: String
myNormalBorderColor = "#dddddd"

myFocusedBorderColor :: String
myFocusedBorderColor = "#ff0000"

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
  where fadeAmount = 0.7

-- gappedLayout = gaps [(U,18), (R,23),(D,23),(L,23)] $ Tall 1 (3/100) (1/2)

myLayout = spacing 10 $ tiled ||| tabbed shrinkText myTabConfig ||| Mirror tiled
-- gappedLayout ||| noBorders Full ||| smartBorders (Mirror tiled)
-- Default tiling algorithm partitions the screen into two panes
  where
    tiled = Tall nmaster delta ratio
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio = 1 / 2
    -- Percent of screen to increment by when resizing panes
    delta = 3 / 100
    myTabConfig =
      def
      { inactiveBorderColor = "#FF0000"
      , activeTextColor = "#00FF00"
      }

myBorderWidth :: Dimension
myBorderWidth = 1

data LibNotifyUrgencyHook =
  LibNotifyUrgencyHook
  deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
  urgencyHook LibNotifyUrgencyHook w = do
    name <- getName w
    wins <- gets windowset
    whenJust (S.findTag w wins) (flash name)
    where
      flash name index =
        spawn $
        "/usr/bin/dbus-launch /usr/bin/notify-send " ++ "Workspace " ++ index ++ "Activity in: " ++ show name

setTransparentHook :: Event -> X All
setTransparentHook ConfigureEvent {ev_event = createNotify
                                  ,ev_window = id} = do
  setOpacity id opacity
  return (All True)
  where
    opacityFloat = 0.9
    opacity = floor $ fromIntegral (maxBound :: Word32) * opacityFloat
    setOpacity id op =
      spawn $
      "xprop -id " ++
      show id ++
      " -f -NET_WM_WINDOW_OPACITY 32c -set _NET_WM_WINDOW_OPACITY" ++ show op
setTransparentHook _ = return (All True)
