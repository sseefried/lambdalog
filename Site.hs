{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (id)
import Control.Arrow ((>>>), (***), arr)
import Control.Category (id)
import Data.Monoid (mempty, mconcat)
import Text.Pandoc (writerHTMLMathMethod, HTMLMathMethod(..))
import Data.Monoid
import Hakyll

-- friends
import Lambdalog.Util

lambdalogItemCompiler = pandocCompilerWith defaultHakyllReaderOptions opts
  where
    opts = defaultHakyllWriterOptions {
      writerHTMLMathMethod = MathJax "http://cdn.mathjax.org/mathjax/latest/MathJax.js" }

renderPostsGen :: Bool -> Pattern -> Tags -> Rules ()
renderPostsGen isDraft glob tags = do
  -- Render posts
  match glob $ do
    route   $ setExtension ".html"
    compile $ lambdalogItemCompiler
      -- this snapshot is vital when we go to render the 3 latest posts in index.html
      -- see use of [loadAllSnapshots]
      >>= saveSnapshot "postBody"
      >>= loadAndApplyTemplate "templates/post.html"    (postContext tags)
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

renderPosts :: Tags -> Rules ()
renderPosts = renderPostsGen False "posts/*"

renderDrafts :: Tags -> Rules ()
renderDrafts = renderPostsGen True "drafts/*"

renderPostsList :: Tags -> Pattern -> String -> String -> Rules ()
renderPostsList tags glob page title = do
  create [fromFilePath page] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< (loadAllSnapshots "posts/*" "postBody" :: Compiler [Item String])
      let ctx = listField "posts" (postContext tags) (return posts)
      makeItem "" -- this means that [page] doesn't need to exist in the source repo.
        >>= loadAndApplyTemplate "templates/posts.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

renderTagsPages :: Tags -> Rules ()
renderTagsPages tags = do
  -- Create the tags pages. If you don't create these then the tags will not
  -- appear in each of the posts.
  tagsRules tags $ \tag pattern -> do
    let title = "Posts tagged " ++ tag
    -- Copied from posts, need to refactor
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pattern
      let ctx = constField "title" title <>
                  listField "posts" (postContext tags) (return posts) <>
                  constField "tag" tag <>
                  defaultContext
      makeItem ""
          >>= loadAndApplyTemplate "templates/posts-with-tag.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls

compileImages :: Pattern -> Rules ()
compileImages glob = do
  match glob $ do
    route idRoute
    compile copyFileCompiler


main :: IO ()
main = hakyll $ do
  -- Compress CSS
  match "css/*" $ do
      route   idRoute
      compile compressCssCompiler
  match "templates/*" $ compile templateCompiler
  -- Compile images
  compileImages "static/sharing-recovery/images/*"
  tags <- buildTags "posts/*" (fromCapture "tags/*.html")
  renderPosts tags
  renderDrafts tags
  renderTagsPages tags
  renderPostsList tags "posts/*"  "posts.html"  "All posts"
  renderPostsList tags "drafts/*" "drafts.html" "All drafts"
  -- Render RSS feed
  create ["rss.xml"] $ do
    route idRoute
    compile $ do
      loadAllSnapshots "posts/*" "postBody"
        >>= fmap (take 10) . recentFirst
        >>= renderRss feedConfiguration feedCtx
  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- fmap (take 3) . recentFirst =<< loadAllSnapshots "posts/*" "postBody"
      let indexContext = listField "posts" (postContext tags) (return posts)
      getResourceBody
        >>= applyAsTemplate indexContext
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Lambdalog"
    , feedDescription = "RSS feed for Lambdalog"
    , feedAuthorName  = "Sean Seefried"
    , feedAuthorEmail = "sean.seefried@gmail.com"
    , feedRoot        = "http://lambdalog.seanseefried.com"
    }


{-

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
    compileImages "static/sharing-recovery/images/*"
    -- Compile files
    match "static/files/*" (route idRoute >> compile copyFileCompiler)

  where
    renderTagCloud' :: Compiler (Tags String) String
    renderTagCloud' = renderTagCloud tagIdentifier 100 120

    tagIdentifier :: String -> Identifier (Item String)
    tagIdentifier = fromCapture "tags/*"

-- | Auxiliary compiler: generate a post list from a list of given posts, and
-- add it to the current page under @$posts@
--
addPostList :: Compiler (Item String, [Item String]) (Item String)
addPostList = setFieldA "posts" $
    arr (reverse . sortByBaseName)
        >>> require "templates/postbody.html" (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody

makeTagList :: String
            -> [Item String]
            -> Compiler () (Item String)
makeTagList tag posts =
    constA (mempty, posts)
        >>> addPostList
        >>> arr (setField "title" ("Posts tagged &#8216;" ++ tag ++ "&#8217;"))
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"

-}