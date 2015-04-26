module Notification where

import DBus.Notify
import DBus.Client (connectSession)

giveYourEeysABreak :: IO ()
giveYourEeysABreak = do
  client <- connectSession
  do
    let startNote = appNote { summary="Mike Says"
                            , body=(Just $ Text "Give your eyes a break") }
    notification <- notify client startNote
    putStrLn "Done"
  where
    appNote = blankNote { appName="Mike" }
