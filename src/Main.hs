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


postsGlob :: Pattern
postsGlob = "posts/*"

getCachedPosts :: Compiler [Item String]
getCachedPosts =
  recentFirst =<< (loadAllSnapshots (postsGlob .&&. hasVersion "cache") "postBody")

postsListField :: String -> Tags -> [Item String] -> Context String
postsListField name tags posts = listField name (postContext False tags) (return posts)

numRecent :: Int
numRecent = 10

cachePosts :: Rules ()
cachePosts = do
  -- Render posts
  match postsGlob $ version "cache" $ do
  --  let ctx = postContext False tags
    compile $ lambdalogItemCompiler
      -- this snapshot is vital when we go to render the 3 latest posts in index.html
      -- see use of [loadAllSnapshots]
      >>= saveSnapshot "postBody"

loadAndApplyDefault :: Tags -> [Item String] -> (Item String -> Compiler (Item String))
loadAndApplyDefault tags posts = do
  let newCtx = defaultContext <> listField "posts" (postContext False tags) (return $ take numRecent posts)
  loadAndApplyTemplate "templates/default.html" newCtx

renderPostsGen :: Bool -> Pattern -> Tags -> Rules ()
renderPostsGen isDraft glob tags = do
  -- Render posts
  match glob $ do
    route   $ setExtension ".html"
    let ctx = postContext isDraft tags
    compile $ do
      -- to avoid a circular dependency we only load snapshots that have version "cache"
      posts <- getCachedPosts
      lambdalogItemCompiler
        >>= loadAndApplyTemplate "templates/post.html"    ctx
        >>= loadAndApplyDefault tags posts
        >>= relativizeUrls

renderPosts :: Tags -> Rules ()
renderPosts = renderPostsGen False "posts/*"

renderDrafts :: Tags -> Rules ()
renderDrafts = renderPostsGen True "drafts/*"

renderPostsList :: Tags -> String -> Rules ()
renderPostsList tags page  = do
  create [fromFilePath page] $ do
    route idRoute
    compile $ do
      posts <- getCachedPosts
      let ctx = postsListField "posts" tags posts
      makeItem ("" :: String) -- this means that [page] doesn't need to exist in the source repo.
        >>= loadAndApplyTemplate "templates/posts.html" ctx
        >>= loadAndApplyDefault tags posts
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
      posts <- recentFirst =<< loadAll (pattern .&&. hasVersion "cache")
      let ctx = constField "title" title <>
                  postsListField "posts" tags posts <>
                  constField "tag" tag <>
                  defaultContext
      makeItem ""
          >>= loadAndApplyTemplate "templates/posts-with-tag.html" ctx
          >>= loadAndApplyDefault tags posts
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
  cachePosts
  renderPosts tags
  renderDrafts tags
  renderTagsPages tags
  renderPostsList tags "posts.html"
--  renderPostsList True tags "drafts/*" "drafts.html"
  -- Render RSS feed
  create ["rss.xml"] $ do
    route idRoute
    compile $ do
      loadAllSnapshots ("posts/*" .&&. hasVersion "cache") "postBody"
        >>= fmap (take 10) . recentFirst
        >>= renderRss feedConfiguration feedCtx
  create [ "index.html" ] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAllSnapshots ("posts/*" .&&. hasVersion "cache") "postBody"
      let last3Posts = take 3 posts
      let indexContext = listField "last3Posts" (postContext False tags) (return last3Posts)
      makeItem ("" :: String)
        >>= loadAndApplyTemplate "templates/index.html"   indexContext
        >>= loadAndApplyDefault tags posts
        >>= relativizeUrls

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Lambdalog"
    , feedDescription = "RSS feed for Lambdalog"
    , feedAuthorName  = "Sean Seefried"
    , feedAuthorEmail = "sean.seefried@gmail.com"
    , feedRoot        = "http://lambdalog.seanseefried.com"
    }