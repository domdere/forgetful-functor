{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Web.ForgetfulFunctor.Contexts.General
-- Copyright    : (C) 2014 - 2018 Dom De Re
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Web.ForgetfulFunctor.Contexts.General (
    forgetfulFunctorContext
  ) where

import Web.ForgetfulFunctor.Details.Social (
    domdereGithub
  , domdereRssFeed
  , domdereTwitter
  , socialContext
  )

import Data.Monoid ((<>))

import Hakyll (
    Context
  , defaultContext
  )

import Preamble hiding ((<>))

forgetfulFunctorContext :: Context String
forgetfulFunctorContext =
      socialContext "domdere-github" domdereGithub
  <>  socialContext "domdere-twitter" domdereTwitter
  <>  socialContext "domdere-rss" domdereRssFeed
  <>  defaultContext

