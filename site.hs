{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (filterM)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Hakyll

import DJSet

itemIsDraft :: MonadMetadata m => Item a -> m Bool
itemIsDraft item = do
  md <- getMetadata (itemIdentifier item)
  return . isDraft $ md

isDraft :: Metadata -> Bool
isDraft md =
  let draft = fromMaybe "false" (lookupString "draft" md)
  in map toLower draft == "true"

main :: IO ()
main = hakyll $ do
    match "robots.txt" $ do
        route   idRoute
        compile copyFileCompiler

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "static/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "dj-sets/*.csv" $ do
        route $ setExtension "html"
        compile $ do
          set <- rawDjSet
          let total = totalDuration set
              songs = listField "songs" songContext (djSet set)
              total' = constField "totalduration" total
              ctx = songs <> total' <> defaultContext
          makeItem ""
            >>= loadAndApplyTemplate "templates/song-list.html" ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

    create ["dj-sets/index.html"] $ do
        route idRoute
        compile $ do
            sets <- reverse <$> loadAll "dj-sets/*.csv"
            let archiveCtx =
                    constField "title" "DJ Setlists" <>
                    listField "sets" defaultContext (return sets) <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/dj-set-archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    match "posts/*" $ do
        route . metadataRoute $ \md ->
          if isDraft md
          then gsubRoute "posts/" (const "drafts/") `composeRoutes` setExtension "html"
          else setExtension "html"
        compile $ do
            let safetitle = field "safetitle" $ \item -> do
                    metadata <- getMetadata (itemIdentifier item)
                    let title = fromMaybe "No title" (lookupString "title" metadata)
                    return $ concatMap (\x -> if x == '\'' then "\\'" else [x]) title
            pandocCompilerWith defaultHakyllReaderOptions defaultHakyllWriterOptions
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/post.html"    (postCtx tags <> safetitle)
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    match "talks/*" $ do
        route $ setExtension "html"
        compile $ pandocCompilerWith defaultHakyllReaderOptions defaultHakyllWriterOptions
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/talk.html"    defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    create ["talk-archive.html"] $ do
        route idRoute
        compile $ do
            talks <- recentFirst =<< loadAll "talks/*"
            let archiveCtx =
                    listField "talks" defaultContext (return talks) <>
                    constField "title" "Talks" <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/talk-archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- loadAll "posts/*" >>= \posts -> do
              recent <- recentFirst posts
              filterM (\x -> not <$> itemIsDraft x) recent
            let archiveCtx =
                    listField "posts" (postCtx tags) (return posts) <>
                    constField "title" "Archives" <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "index.html" $ do
        route $ setExtension "html"
        compile $ do
            talks <- recentFirst =<< loadAll "talks/*"
            posts <- loadAll "posts/*" >>= \posts -> do
              recent <- recentFirst posts
              filterM (\x -> not <$> itemIsDraft x) recent
            let indexCtx =
                    listField "posts" (postCtx tags) (return (take 10 posts)) <>
                    listField "talks" defaultContext (return (take 10 talks)) <>
                    constField "title" "Home" <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "*.html" $ do
        route $ setExtension "html"
        compile $ do
            talks <- recentFirst =<< loadAll "talks/*"
            posts <- loadAll "posts/*" >>= \posts -> do
              recent <- recentFirst posts
              filterM (\x -> not <$> itemIsDraft x) recent
            let indexCtx =
                    listField "posts" (postCtx tags) (return (take 10 posts)) <>
                    listField "talks" defaultContext (return (take 10 talks)) <>
                    constField "title" "Home" <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged " ++ tag

        route idRoute
        compile $ do
            posts <- loadAll pattern >>= \posts -> do
              recent <- recentFirst posts
              filterM (\x -> not <$> itemIsDraft x) recent
            let ctx =
                    listField "posts" (postCtx tags) (return posts) <>
                    constField "title" title <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

        version "rss" $ do
            route   $ setExtension "xml"
            compile $ loadAllSnapshots pattern "content"
                >>= fmap (take 10) . recentFirst
                >>= renderRss (feedConfiguration title) feedCtx

    create ["rss.xml"] $ do
        route idRoute
        compile $
            loadAllSnapshots "articles/*.md" "content"
                >>= fmap (take 10) . recentFirst
                >>= renderRss (feedConfiguration "All posts") feedCtx

    match "templates/*" $ compile templateCompiler


postCtx :: Tags -> Context String
postCtx tags = mconcat
    [ modificationTimeField "mtime" "%U"
    , dateField "date" "%B %e, %Y"
    , tagsField "tags" tags
    , defaultContext
    ]

feedConfiguration :: String -> FeedConfiguration
feedConfiguration title = FeedConfiguration
    { feedTitle       = "Rick Elrod's Blog - " ++ title
    , feedDescription = "My life, FOSS, Fedora, (Functional) Programming, Music, and other random things."
    , feedAuthorName  = "Rick Elrod"
    , feedAuthorEmail = "rick.website@elrod.me"
    , feedRoot        = "https://elrod.me"
    }

feedCtx :: Context String
feedCtx = mconcat
    [ bodyField "description"
    , defaultContext
    ]
