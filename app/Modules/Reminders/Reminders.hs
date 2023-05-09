import Data.Time.Clock (UTCTime, getCurrentTime)

data Reminder = Reminder { description :: String, dueDate :: UTCTime }

newtype Reminders = Reminders [Reminder]

addReminder :: String -> Reminders -> IO Reminders
addReminder desc (Reminders rs) = do
  now <- getCurrentTime
  let reminder = Reminder desc now
  return $ Reminders (reminder : rs)

removeReminder :: Int -> Reminders -> Reminders
removeReminder i (Reminders rs) = Reminders (remove i rs)

listReminders :: Reminders -> [Reminder]
listReminders (Reminders rs) = rs

remove :: Int -> [a] -> [a]
remove n xs = take n xs ++ drop (n+1) xs

safeIndex :: Int -> [a] -> Maybe a
safeIndex i xs
  | i < 0 || i >= length xs = Nothing
  | otherwise              = Just (xs !! i)

update :: Int -> a -> [a] -> [a]
update i x xs = take i xs ++ [x] ++ drop (i+1) xs
