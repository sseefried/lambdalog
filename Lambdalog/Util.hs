module Lambdalog.Util where

import Hakyll
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTime, formatTime)
import System.FilePath (takeFileName)
import System.FilePath.Posix
import System.Locale (TimeLocale, defaultTimeLocale)


renderDateFields :: (String, String, String) -- ^ Format to use on the (day, month,year) fields
                 -> (String, String, String)      -- ^ Default value
                 -> Page a      -- ^ Target page
                 -> Page a      -- ^ Resulting page
renderDateFields (dayFormat, monthFormat, yearFormat) defaultValue page =
  (setField "day" day) . (setField "month" month) . (setField "year" year) $ page

  where
    (day, month, year) = renderDate' (getField "path" page)
    aux fmt time = formatTime defaultTimeLocale fmt time
    renderDate' :: String -> (String, String, String)
    renderDate' filePath = fromMaybe defaultValue $ do
        let dateString = intercalate "-" $ take 3
                       $ splitAll "-" $ takeFileName filePath
        time <- parseTime defaultTimeLocale
                          "%Y-%m-%d"
                          dateString :: Maybe UTCTime
        return $ (aux dayFormat time, aux monthFormat time, aux yearFormat time)

setDisqusId :: Page String -> Page String
setDisqusId page = setField key disqusId page
  where
    key = "disqusId"
    path = getField "path" page
    disqusId = fromMaybe (take 50 . takeBaseName $ path) $ getFieldMaybe key page
