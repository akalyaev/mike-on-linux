{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception
import           Data.Text                (Text)
import           Graphics.UI.AppIndicator
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.General.StockItems

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

    mainGUI

buttons :: [ActionEntry]
buttons = [
    ActionEntry "X:Y until Rest" "_X:Y until Rest" (Just stockMediaPause)
                 Nothing Nothing
                 (activateAction "Open"),
    ActionEntry "Restart Timer" "_Restart Timer" (Just stockMediaRewind)
                 Nothing Nothing
                 (activateAction "Open"),
    ActionEntry "Ready to Rest" "_Ready to Rest" (Just stockMediaNext)
                 Nothing Nothing
                 (activateAction "Open"),
    ActionEntry "Settings" "_Settings" Nothing
                 Nothing Nothing
                 (activateAction "Open"),
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
