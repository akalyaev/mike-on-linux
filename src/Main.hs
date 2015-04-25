{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception
import           Data.Text                (Text)
import           Graphics.UI.AppIndicator
import           Graphics.UI.Gtk

main :: IO ()
main = do
    _ <- initGUI

    actionGroup <- actionGroupNew ("AppActions" :: Text)
    uim         <- uiManagerNew
    indicator   <- appIndicatorNew ("mike-indicator" :: Text)
                                   ("indicator-messages" :: Text)
                                   AppIndicatorCategoryApplicationStatus

    actionGroupAddActions actionGroup entries
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

    mainGUI

entries :: [ActionEntry]
entries = [
    ActionEntry "X:Y until Rest" "_X:Y until Rest" (Just "time-until-rest")
                 Nothing Nothing
                 (activateAction "Open"),
    ActionEntry "Pause Timer" "_Pause Timer" (Just "pause-timer")
                 Nothing Nothing
                 (activateAction "Open"),
    ActionEntry "Restart Timer" "_Restart Timer" (Just "restart-timer")
                 Nothing Nothing
                 (activateAction "Open"),
    ActionEntry "Ready to Rest" "_Ready to Rest" (Just "ready-to-rest")
                 Nothing Nothing
                 (activateAction "Open"),
    ActionEntry "Settings" "_Settings" (Just "settings")
                 Nothing Nothing
                 (activateAction "Open"),
    ActionEntry "Show Time in Menu Bar" "_Show Time in Menu Bar" (Just "show-time-in-menu-bar")
                 Nothing Nothing
                 (activateAction "Open"),
    ActionEntry "Show Animated Progress in Menu Bar" "_Show Animated Progress in Menu Bar" (Just "show-animated-progress-in-menu-bar")
                 Nothing Nothing
                 (activateAction "Open"),
    ActionEntry "Quit" "_Quit" (Just "application-exit")
                 Nothing Nothing
                 mainQuit
    ]

uiInfo :: Text
uiInfo = "<ui>\
         \  <popup name='MenuBar'>\
         \    <menuitem action='X:Y until Rest'/>\
         \    <menuitem action='Pause Timer'/>\
         \    <menuitem action='Restart Timer'/>\
         \    <menuitem action='Ready to Rest'/>\
         \    <separator/>\
         \    <menuitem action='Settings'/>\
         \    <menuitem action='Show Time in Menu Bar'/>\
         \    <menuitem action='Show Animated Progress in Menu Bar'/>\
         \    <menuitem action='Quit'/>\
         \  </popup>\
         \</ui>"

activateAction :: Text -> IO ()
activateAction name = do
    dialog <- messageDialogNew Nothing [DialogDestroyWithParent]
                               MessageInfo ButtonsClose name
    _ <- on dialog response (\_ -> widgetDestroy dialog)
    widgetShow dialog
