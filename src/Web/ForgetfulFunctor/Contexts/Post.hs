{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Web.ForgetfulFunctor.Contexts.Post
-- Copyright    : (C) 2014 - 2018 Dom De Re
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Web.ForgetfulFunctor.Contexts.Post (
  -- * Functions
    postCtxWithTags
  -- * Values
  , postCtx
  ) where

import Web.ForgetfulFunctor.Contexts.General (forgetfulFunctorContext)

import Hakyll (
    Context
  , Tags
  , categoryField
  , constField
  , dateField
  , tagsField
  )

import Preamble

postCtx :: Context String
postCtx = fold [
    constField "posts" "Yes"
  , dateField "date" "%B %e, %Y"
  , dateField "datetime" "%Y-%m-%d"
  , forgetfulFunctorContext
  ]

postCtxWithTags :: Tags -> Tags -> Context String
postCtxWithTags cats tags = fold [
    categoryField "cats" cats
  , tagsField "tags" tags
  , postCtx
  ]


