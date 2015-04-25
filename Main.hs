{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception
import           Data.Text                (Text)
import           Graphics.UI.AppIndicator
import           Graphics.UI.Gtk

main :: IO ()
main = do
    _ <- initGUI

    window      <- windowNew
    table       <- tableNew 1 5 False
    sw          <- scrolledWindowNew Nothing Nothing
    contents    <- textViewNew
    statusBar   <- statusbarNew
    actionGroup <- actionGroupNew ("AppActions" :: Text)
    uim         <- uiManagerNew
    indicator   <- appIndicatorNew ("example-simple-client" :: Text)
                                   ("indicator-messages" :: Text)
                                   AppIndicatorCategoryApplicationStatus

    set window [ windowTitle := ("Indicator Demo" :: Text)
               , windowIconName := ("indicator-messages-new" :: Text)
               ]
    _ <- on window objectDestroy mainQuit
    containerAdd window table

    actionGroupAddActions actionGroup entries
    uiManagerInsertActionGroup uim actionGroup 0
    _ <- windowAddAccelGroup window `fmap` uiManagerGetAccelGroup uim
    catch (uiManagerAddUiFromString uim uiInfo >> return ())
          (\e -> putStrLn $ "Failed to build menus: "
                         ++ show (e :: SomeException))
    Just menuBar <- uiManagerGetWidget uim ("/ui/MenuBar" :: Text)
    widgetShow menuBar
    tableAttach table menuBar 0 1 0 1 [Expand, Fill] [] 0 0

    scrolledWindowSetPolicy sw PolicyAutomatic PolicyAutomatic
    scrolledWindowSetShadowType sw ShadowIn
    tableAttach table sw 0 1 3 4 [Expand, Fill] [Expand, Fill] 0 0
    windowSetDefaultSize window 200 200
    widgetGrabFocus contents
    containerAdd sw contents

    tableAttach table statusBar 0 1 4 5 [Expand, Fill] [] 0 0

    widgetShowAll window

    Just indicatorMenu <- uiManagerGetWidget uim ("/ui/IndicatorPopup" :: Text)
    set indicator [appIndicatorAttentionIconName :=
                       Just ("indicator-messages-new" :: Text)
                  ]
    appIndicatorSetStatus indicator AppIndicatorStatusActive
    appIndicatorSetMenu indicator (castToMenu indicatorMenu)

    mainGUI

entries :: [ActionEntry]
entries = [
    ActionEntry "FileMenu" "_File" Nothing
                 Nothing Nothing
                 (return ()),
    ActionEntry "New" "_New" (Just "document-new")
                 (Just "<control>N") (Just "Create a new file")
                 (activateAction "New"),
    ActionEntry "Open" "_Open" (Just "document-open")
                 (Just "<control>D") (Just "Open a file")
                 (activateAction "Open"),
    ActionEntry "Save" "_Save" (Just "document-save")
                 (Just "<control>S") (Just "Save file")
                 (activateAction "Save"),
    ActionEntry "Quit" "_Quit" (Just "application-exit")
                 (Just "<control>Q") (Just "Exit the application")
                 mainQuit
    ]

uiInfo :: Text
uiInfo = "<ui>\
         \  <menubar name='MenuBar'>\
         \    <menu action='FileMenu'>\
         \      <menuitem action='New'/>\
         \      <menuitem action='Open'/>\
         \      <menuitem action='Save'/>\
         \      <separator/>\
         \      <menuitem action='Quit'/>\
         \    </menu>\
         \  </menubar>\
         \  <popup name='IndicatorPopup'>\
         \    <menuitem action='New' />\
         \    <menuitem action='Open' />\
         \    <menuitem action='Save' />\
         \    <menuitem action='Quit' />\
         \  </popup>\
         \</ui>"

activateAction :: Text -> IO ()
activateAction name = do
    dialog <- messageDialogNew Nothing [DialogDestroyWithParent]
                               MessageInfo ButtonsClose name
    _ <- on dialog response (\_ -> widgetDestroy dialog)
    widgetShow dialog
