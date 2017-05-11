module Utils
    (
      parseUTC
    ) where

import           Data.Time

dateFormat :: String
dateFormat = iso8601DateFormat (Just "%H:%M:%S%Z")

parseUTC :: String -> Maybe UTCTime
parseUTC = parseTimeM True defaultTimeLocale dateFormat
