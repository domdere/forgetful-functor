{-# LANGUAGE OverloadedStrings #-}

-------------------------------------------------------------------
-- |
-- Module       : Web.ForgetFunctor.Context.Post
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- Contexts for Posts.
-------------------------------------------------------------------
module Web.ForgetfulFunctor.Context.Post (
    -- * Extracts the contexts from Posts
        postContext
    ,   postListContext
    ,   homePagePostCtx
    ) where

import Data.Monoid
import Data.Foldable ( fold )
import Hakyll
    (   Context
    ,   Item
    ,   bodyField
    ,   constField
    ,   dateField
    ,   defaultContext
    ,   listField
    )

-- | Extracts the context for a single post
--
postContext :: Context String
postContext = fold
    [   dateField "date" "%B %e, %Y"
    ,   constField "posts" "Yes"
    ,   defaultContext
    ]

-- | Extracts the context for all the talks
--
postListContext :: [Item String] -> Context String
postListContext ps = fold
    [   listField "postList" postContext (return ps)
    ,   constField "posts" "Yes"
    ,   defaultContext
    ]

-- | On the home page, I just want to show the most recent post..
--
homePagePostCtx :: [Item String] -> Context String
homePagePostCtx ps = fold
    [   listField "postList" postContext (return (take 1 ps))
    ,   constField "posts" "Yes"
    ,   defaultContext
    ]
