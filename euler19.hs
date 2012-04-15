import Data.Time.Calendar
import Data.Time.Calendar.WeekDate

length $ filter (\(y,w,dayOfWeek) -> dayOfWeek == 7) [toWeekDate $ fromGregorian year mon 1 | mon <- [1..12], year <- [1901..2000]]
