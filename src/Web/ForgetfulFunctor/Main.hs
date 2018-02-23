{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Web.ForgetfulFunctor.Main
-- Copyright    : (C) 2014 - 2018 Dom De Re
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Web.ForgetfulFunctor.Main (siteGenerator) where

import Web.ForgetfulFunctor.Contexts.General (
    forgetfulFunctorContext
  )
import Web.ForgetfulFunctor.Contexts.Post (
    postCtx
  , postCtxWithTags
  )
import Web.ForgetfulFunctor.Data.String (extractTeaser)
import Web.ForgetfulFunctor.Details.Social (
    domdereGithub
  , domdereTwitter
  , socialContext
  )

import Ultra.Data.List (ordNub)

import Hakyll (
    Context
  , FeedConfiguration(..)
  , Item
  , Pattern
  , Tags(..)
  , bodyField
  , buildCategories
  , buildTags
  , compile
  , composeRoutes
  , constField
  , copyFileCompiler
  , create
  , fromCapture
  , fromFilePath
  , fromGlob
  , gsubRoute
  , hakyll
  , idRoute
  , listField
  , loadAll
  , loadAllSnapshots
  , loadAndApplyTemplate
  , makeItem
  , match
  , pandocCompiler
  , recentFirst
  , renderAtom
  , renderTagCloud
  , relativizeUrls
  , route
  , saveSnapshot
  , setExtension
  , tagsRules
  , templateCompiler
  )

import Data.Monoid ((<>))

import Preamble hiding ((<>))

siteGenerator :: IO ()
siteGenerator = hakyll $
  let
    idPatterns :: [Pattern]
    idPatterns = ["images/**", "fonts/*", "css/*", "js/*", "CNAME"]
  in do
    forM_ idPatterns $ \pat ->
      match pat $ do
        route idRoute
        compile copyFileCompiler

    match "templates/*" $
      compile templateCompiler

    tags <- buildTags "posts/**.md" (fromCapture "tags/*.html")

    cats <- buildCategories "posts/**.md" (fromCapture "categories/*.html")

    let postCtx' = postCtxWithTags cats tags

    forM_ [
        (tags, "tag", "templates/tag-posts.html")
      , (cats, "cat", "templates/cat-posts.html")
      ] $ \(items, key, template) ->
        tagsRules items $ \tag ptn ->
          let
            title :: String
            title = fold [
                "Posts tagged '"
              , tag
              , "'"
              ]

            ctx :: [Item String] -> Context String
            ctx posts' =
                  constField "title" title
              <>  constField key tag
              <>  listField "posts" postCtx (pure posts')
              <>  forgetfulFunctorContext
          in do
            route idRoute
            compile $ do
              posts <- recentFirst =<< loadAll ptn
              makeItem ""
                >>= loadAndApplyTemplate template (ctx posts)
                >>= loadAndApplyTemplate "templates/default.html" (ctx posts)
                >>= relativizeUrls

    match "posts/**.md" $ do
      route $ setExtension "html"
      compile $ do
        rawHtml <- pandocCompiler
        fullArticle <- loadAndApplyTemplate "templates/post.html" postCtx' rawHtml
          >>= saveSnapshot "content"
        void $ loadAndApplyTemplate "templates/post-teaser.html" postCtx' (extractTeaser <$> rawHtml)
          >>= saveSnapshot "teaser"
        loadAndApplyTemplate "templates/post-default.html" postCtx' fullArticle
          >>= relativizeUrls

    -- The Atom feed
    create ["atom.feed"] $
      let
        feedCtx :: Context String
        feedCtx = bodyField "description" <> postCtx'

        feedCfg :: FeedConfiguration
        feedCfg = FeedConfiguration
          "Forgetful Functor"
          "Thoughts on Indie Game Dev, Software Engineering, Tech Culture, etc..."
          "Dom De Re"
          ""
          "http://blog.forgetfulfunctor.com"
      in do
        route idRoute
        compile $ loadAllSnapshots "posts/**.md" "content"
          >>= (fmap (take 10) . recentFirst)
          >>= renderAtom feedCfg feedCtx

    tagsRules cats $ \tag _ ->
      create [fromFilePath $ "feeds/" <> tag <> ".feed"] $
        let
          feedCtx :: Context String
          feedCtx = bodyField "description" <> postCtx'

          feedCfg :: FeedConfiguration
          feedCfg = FeedConfiguration
            "Forgetful Functor"
            ("The sub feed for just the " <> tag <> " category")
            "Dom De Re"
            ""
            "http://blog.forgetfulfunctor.com"
        in do
          route idRoute
          compile $ loadAllSnapshots (fromGlob $ "posts/" <> tag <> "/**.md") "content"
            >>= (fmap (take 10) . recentFirst)
            >>= renderAtom feedCfg feedCtx


    create [".nojekyll"] $ do
      route idRoute
      compile $ makeItem ("" :: String)

    create ["contact.html"] $ do
      route idRoute
      compile $
        let
          ctx :: Context String
          ctx = constField "title" "Contact"
            <>  constField "contact" "Yes"
            <>  listField "categories" (ordNub . fmap fst . tagsMap $ cats)
            <>  socialContext "domdere-twitter" domdereTwitter
            <>  socialContext "domdere-github" domdereGithub
            <>  forgetfulFunctorContext
        in makeItem ""
          >>= loadAndApplyTemplate "templates/contact.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls

    create ["posts.html"] $ do
      route idRoute
      compile $
        let
          archiveCtx :: String -> String -> [Item String] -> Context String
          archiveCtx catCloud tagCloud posts' = fold [
              listField "posts" postCtx (pure posts')
            , constField "title" "Posts"
            , constField "archives" "Yes"
            , constField "cats" catCloud
            , constField "tags" tagCloud
            , forgetfulFunctorContext
            ]
        in do
          posts <- recentFirst =<< loadAll "posts/**.md"
          tagCloud <- renderTagCloud 100.0 1000.0 tags
          catCloud <- renderTagCloud 100.0 1000.0 cats
          makeItem ""
            >>= loadAndApplyTemplate "templates/posts.html" (archiveCtx catCloud tagCloud posts)
            >>= loadAndApplyTemplate "templates/default.html" (archiveCtx catCloud tagCloud posts)
            >>= relativizeUrls

    let simplePages = ["about.md"]
    forM_ simplePages $ \str ->
      match (fromGlob ("pages/" <> str)) $
        let
          ctx :: Context String
          ctx = forgetfulFunctorContext
        in do
          route $
            composeRoutes
              (gsubRoute "pages/" (const ""))
              (setExtension "html")
          compile $
            pandocCompiler
              >>= loadAndApplyTemplate "templates/default.html" ctx
              >>= relativizeUrls


    match "pages/index.md" $ do
      route $
        composeRoutes
          (gsubRoute "pages/" (const ""))
          (setExtension "html")
      compile $
        let
          indexCtx :: [Item String] -> Context String
          indexCtx posts =
                constField "title" "Forgetful Functor"
            <>  listField "postList" postCtx (pure (take 10 posts))
            <>  forgetfulFunctorContext
        in do
          posts <- recentFirst =<< loadAllSnapshots "posts/**.md" "teaser"
          pandocCompiler
            >>= loadAndApplyTemplate "templates/home.html" (indexCtx posts)
            >>= loadAndApplyTemplate "templates/default.html" (indexCtx posts)
            >>= relativizeUrls
