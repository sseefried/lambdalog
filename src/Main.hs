{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.Pandoc (writerHTMLMathMethod, HTMLMathMethod(..))
import Hakyll

-- friends
import Lambdalog.Util

lambdalogItemCompiler :: Compiler (Item String)
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
      >>= loadAndApplyTemplate "templates/post.html"    (postContext isDraft tags)
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

renderPosts :: Tags -> Rules ()
renderPosts = renderPostsGen False "posts/*"

renderDrafts :: Tags -> Rules ()
renderDrafts = renderPostsGen True "drafts/*"

renderPostsList :: Bool -> Tags -> Pattern -> String -> Rules ()
renderPostsList isDraft tags glob page  = do
  create [fromFilePath page] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< (loadAllSnapshots glob "postBody" :: Compiler [Item String])
      let ctx = listField "posts" (postContext isDraft tags) (return posts)
      makeItem ("" :: String) -- this means that [page] doesn't need to exist in the source repo.
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
                  listField "posts" (postContext False tags) (return posts) <>
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
  renderPostsList False tags "posts/*"  "posts.html"
  renderPostsList True tags "drafts/*" "drafts.html"
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
      let indexContext = listField "posts" (postContext False tags) (return posts)
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