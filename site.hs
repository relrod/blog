{-# LANGUAGE OverloadedStrings #-}
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Hakyll
import Text.Pandoc.Options (readerSmart)

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "static/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
            let safetitle = field "safetitle" $ \item -> do
                    metadata <- getMetadata (itemIdentifier item)
                    let title = fromMaybe "No title" (lookupString "title" metadata)
                    return $ concatMap (\x -> if x == '\'' then "\\'" else [x]) title
            pandocCompilerWith defaultHakyllReaderOptions {readerSmart = False} defaultHakyllWriterOptions
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/post.html"    (postCtx tags <> safetitle)
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    match "talks/*" $ do
        route $ setExtension "html"
        compile $ pandocCompilerWith defaultHakyllReaderOptions {readerSmart = False} defaultHakyllWriterOptions
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
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" (postCtx tags) (return posts) <>
                    constField "title" "Archives" <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "*.html" $ do
        route $ setExtension "html"
        compile $ do
            talks <- recentFirst =<< loadAll "talks/*"
            posts <- recentFirst =<< loadAll "posts/*"
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
            posts <- recentFirst =<< loadAll pattern
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
                >>= renderAtom (feedConfiguration title) feedCtx

    create ["rss.xml"] $ do
        route idRoute
        compile $
            loadAllSnapshots "articles/*.md" "content"
                >>= fmap (take 10) . recentFirst
                >>= renderAtom (feedConfiguration "All posts") feedCtx

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
    { feedTitle       = "Ricky Elrod's Blog - " ++ title
    , feedDescription = "FOSS, Fedora, (Functional) Programming"
    , feedAuthorName  = "Ricky Elrod"
    , feedAuthorEmail = "ricky@elrod.me"
    , feedRoot        = "https://elrod.me"
    }

feedCtx :: Context String
feedCtx = mconcat
    [ bodyField "description"
    , defaultContext
    ]
