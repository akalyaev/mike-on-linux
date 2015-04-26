module Notification where

import Libnotify

giveYourEeysABreak :: IO ()
giveYourEeysABreak = do
  display $
       summary "Mike Says"
    <> body "Give your eyes a break"
    <> icon "face-embarrassed"
    <> timeout Infinite
    <> urgency Critical
  putStrLn "Done"
    -- <> action "blob" "Say \"blop\"" blopCallback
    -- <> action "flop" "Say \"flop\"" (flopCallback l)

-- blopCallback :: Notification -> t -> IO Notification
-- blopCallback n _ = do
--   close n
--   putStrLn "Thanks!"
--   display (reuse n <> summary "" <> body "Pretty please, say \"blop\"!")

-- flopCallback :: MainLoop -> Notification -> t -> IO ()
-- flopCallback l n _ = do
--   close n
--   putStrLn "Pfft.."

