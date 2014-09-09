module Lambdalog.Util where

import Hakyll
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTime, formatTime)
import System.FilePath (takeFileName)
import System.FilePath.Posix
import System.Locale (TimeLocale, defaultTimeLocale)
import Control.Applicative
import Data.Monoid


dayFieldContext :: Context a
dayFieldContext = field "day" $ \item -> do
  let (day, _, _) = splitDate . toFilePath . itemIdentifier $ item
  return day

monthFieldContext :: Context a
monthFieldContext = field "month" $ \item -> do
  let (_, month, _) = splitDate . toFilePath . itemIdentifier $ item
  return month

yearFieldContext :: Context a
yearFieldContext = field "year" $ \item -> do
  let (_, _, year) = splitDate . toFilePath . itemIdentifier $ item
  return year

dateContext :: Context a
dateContext = mconcat [dayFieldContext, monthFieldContext, yearFieldContext]

postContext :: Tags -> Context String
postContext tags =
  mconcat [ tagsField "prettytags" tags
          , dateContext
          , dateField "date" "%d %b %Y"
          -- FIXME: set this properly
          , field "disqusId" $ const . return $ "notareallydisqusId"
          , defaultContext ]

splitDate :: String -> (String, String, String)
splitDate filePath = fromMaybe defaultValue $ do
        let dateString = intercalate "-" $ take 3
                       $ splitAll "-" $ takeFileName filePath
        time <- parseTime defaultTimeLocale
                          "%Y-%m-%d"
                          dateString :: Maybe UTCTime
        return $ (aux "%e" time, aux "%b" time, aux "%Y" time)
  where
    aux fmt time = formatTime defaultTimeLocale fmt time
    defaultValue = ("1", "Jan", "2001")

feedCtx :: Context String
feedCtx = mconcat
    [ bodyField "description"
    , defaultContext
    ]

--setDisqusId ::  Item String -> Item String
--setDisqusId = setDisqusIdGen ""

--setDraftDisqusId :: Item String -> Item String
--setDraftDisqusId = setDisqusIdGen "draft-"

--setDisqusIdGen :: String -> Item String -> Item String
--setDisqusIdGen prefix page = setField key disqusId page
--  where
--    key = "disqusId"
--    path = getField "path" page
--    disqusId :: String
--    disqusId = prefix ++ (fromMaybe (take 80 . takeBaseName $ path) $ getFieldMaybe key page)
