{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (id)
import Control.Arrow ((>>>), (***), arr)
import Control.Category (id)
import Data.Monoid (mempty, mconcat)
import Text.Pandoc (writerHTMLMathMethod, HTMLMathMethod(..))
import Hakyll

-- friends
import Lambdalog.Util

lambdalogPageCompiler = pageCompilerWithPandoc defaultHakyllParserState opts id
  where
    opts = defaultHakyllWriterOptions {
      writerHTMLMathMethod = MathJax "http://cdn.mathjax.org/mathjax/latest/MathJax.js" }


renderPostsGen :: Bool -> Pattern a -> RulesM (Pattern (Page String))
renderPostsGen isDraft glob = do
    -- Render posts
    match glob $ do
        route   $ setExtension ".html"
        compile $ lambdalogPageCompiler
            >>> arr (renderDateFields ("%e", "%b", "%Y") ("1", "Jan", "2001"))
            >>> renderTagsField "prettytags" (fromCapture "tags/*")
            >>> arr (if isDraft then setDraftDisqusId else setDisqusId)
            >>> arr (copyBodyToField "renderedPost")
            >>> applyTemplateCompiler "templates/post.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

renderPosts :: RulesM (Pattern (Page String))
renderPosts = renderPostsGen False "posts/*"

renderDrafts :: RulesM (Pattern (Page String)) 
renderDrafts = renderPostsGen True "drafts/*"


renderPostsList :: Pattern (Page String) -> String -> String -> RulesM (Identifier (Page String))
renderPostsList glob pageName title = do
    match (parseGlob pageName) $ route idRoute
    create (parseIdentifier pageName) $ constA mempty
        >>> arr (setField "title" title)
        >>> requireAllA glob addPostList
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

main :: IO ()
main = hakyll $ do
    -- Compress CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    renderPosts
    renderDrafts

    -- Render posts list
    renderPostsList "posts/*" "posts.html" "All posts"
    renderPostsList "drafts/*" "drafts.html" "All drafts"

    -- Index
    match "index.html" $ route idRoute
    create "index.html" $ constA mempty
        >>> arr (setField "title" "Home")
        >>> requireA "tags" (setFieldA "tagcloud" (renderTagCloud'))
        >>> requireAllA "posts/*" (id *** arr (take 3 . reverse . sortByBaseName) >>> addPostList)
        >>> applyTemplateCompiler "templates/index.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    -- Tags
    create "tags" $
        requireAll "posts/*" (\_ ps -> readTags ps :: Tags String)

    -- Add a tag list compiler for every tag
    match "tags/*" $ route $ setExtension ".html"
    metaCompile $ require_ "tags"
        >>> arr tagsMap
        >>> arr (map (\(t, p) -> (tagIdentifier t, makeTagList t p)))

    -- Render RSS feed
    match "rss.xml" $ route idRoute
    create "rss.xml" $
        requireAll_ "posts/*"
            >>> mapCompiler (arr $ copyField "renderedPost" "description")
            >>> renderRss feedConfiguration

    -- Read templates
    match "templates/*" $ compile templateCompiler

    -- Compile images
    match "static/images/*" $ do
      route idRoute
      compile copyFileCompiler

  where
    renderTagCloud' :: Compiler (Tags String) String
    renderTagCloud' = renderTagCloud tagIdentifier 100 120

    tagIdentifier :: String -> Identifier (Page String)
    tagIdentifier = fromCapture "tags/*"

-- | Auxiliary compiler: generate a post list from a list of given posts, and
-- add it to the current page under @$posts@
--
addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "posts" $
    arr (reverse . sortByBaseName)
        >>> require "templates/postbody.html" (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody

makeTagList :: String
            -> [Page String]
            -> Compiler () (Page String)
makeTagList tag posts =
    constA (mempty, posts)
        >>> addPostList
        >>> arr (setField "title" ("Posts tagged &#8216;" ++ tag ++ "&#8217;"))
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Lambdalog"
    , feedDescription = "RSS feed for Lambdalog"
    , feedAuthorName  = "Sean Seefried"
    , feedRoot        = "http://lambdalog.seanseefried.com"
    }
