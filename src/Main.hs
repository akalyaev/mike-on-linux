{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Control.Exception
import           Data.Text                (Text)
import           Graphics.UI.AppIndicator
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.General.StockItems
import           Notification

main :: IO ()
main = do
    _ <- initGUI

    actionGroup <- actionGroupNew ("AppActions" :: Text)
    uim         <- uiManagerNew
    indicator   <- appIndicatorNew ("mike-indicator" :: Text)
                                   ("indicator-messages" :: Text)
                                   AppIndicatorCategoryApplicationStatus

    actionGroupAddActions actionGroup buttons
    actionGroupAddToggleActions actionGroup togls
    uiManagerInsertActionGroup uim actionGroup 0
    catch (uiManagerAddUiFromString uim uiInfo >> return ())
          (\e -> putStrLn $ "Failed to build menus: "
                         ++ show (e :: SomeException))

    Just indicatorMenu <- uiManagerGetWidget uim ("/ui/MenuBar" :: Text)
    set indicator [appIndicatorAttentionIconName :=
                       Just ("indicator-messages-new" :: Text)
                  ]
    appIndicatorSetStatus indicator AppIndicatorStatusActive
    appIndicatorSetMenu indicator (castToMenu indicatorMenu)

    -- 50ms timeout, so GHC will get a chance to scheule about 20 times a second
    -- which gives reasonable latency without the polling generating too much
    -- cpu load.
    timeoutAddFull (yield >> return True) priorityDefaultIdle 50

    t <- startTimer

    mainGUI

buttons :: [ActionEntry]
buttons = [
    ActionEntry "X:Y until Rest" "_X:Y until Rest" (Just stockMediaPause)
                 Nothing Nothing
                 (activateAction "ShowSettings"),
    ActionEntry "Restart Timer" "_Restart Timer" (Just stockMediaRewind)
                 Nothing Nothing
                 (activateAction "ShowSettings"),
    ActionEntry "Ready to Rest" "_Ready to Rest" Nothing
                 Nothing Nothing
                 (activateAction "ShowSettings"),
    ActionEntry "Settings" "_Settings" Nothing
                 Nothing Nothing
                 (activateAction "ShowSettings"),
    ActionEntry "Quit" "_Quit" (Just "application-exit")
                 Nothing Nothing
                 mainQuit
    ]

togls :: [ToggleActionEntry]
togls = [
    ToggleActionEntry "Show Time in Menu Bar" "_Show Time in Menu Bar" (Just "show-time-in-menu-bar")
                      Nothing Nothing
                      (toggleSetting "show-time") False
    ]

uiInfo :: Text
uiInfo = "<ui>\
         \  <popup name='MenuBar'>\
         \    <menuitem action='X:Y until Rest'/>\
         \    <menuitem action='Restart Timer'/>\
         \    <menuitem action='Ready to Rest'/>\
         \    <separator/>\
         \    <menuitem action='Settings'/>\
         \    <menuitem action='Show Time in Menu Bar'/>\
         \    <menuitem action='Quit'/>\
         \  </popup>\
         \</ui>"

activateAction :: Text -> IO ()
activateAction name = do
    dialog <- messageDialogNew Nothing [DialogDestroyWithParent]
                               MessageInfo ButtonsClose name
    _ <- on dialog response (\_ -> widgetDestroy dialog)
    widgetShow dialog

toggleSetting :: String -> IO ()
toggleSetting name = putStrLn ("TODO")

startTimer :: IO ThreadId
startTimer = forkIO $ runTimer 25

runTimer :: Integer -> IO ()
runTimer 0 = giveYourEeysABreak
runTimer t = do
      putStrLn $ show t ++ " seconds left"
      threadDelay second
      runTimer (t-1)

second :: Int
second = 1000000
