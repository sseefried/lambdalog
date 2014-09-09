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
import qualified Data.Map as M


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

postContext :: Bool -> Tags -> Context String
postContext draft tags =
  mconcat [ tagsField "prettytags" tags
          , dateContext
          , dateField "date" "%d %b %Y"
          -- FIXME: set this properly
          , if draft then draftDisqusIdContext else disqusIdContext
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

disqusIdContext ::  Context String
disqusIdContext = disqusIdGen ""

draftDisqusIdContext :: Context String
draftDisqusIdContext = disqusIdGen "draft-"

disqusIdGen :: String -> Context String
disqusIdGen prefix = field "disqusId" $ \item -> do
  let path = toFilePath . itemIdentifier $ item
  metadata <- getMetadata (itemIdentifier item)
  return $ fromMaybe (take 80 . takeBaseName $ path) $ M.lookup "title" metadata
