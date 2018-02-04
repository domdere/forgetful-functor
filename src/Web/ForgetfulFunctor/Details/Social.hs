{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Web.ForgetfulFunctor.Details.Social
-- Copyright    : (C) 2014 - 2018 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Web.ForgetfulFunctor.Details.Social (
  -- * Types
    SocialDetails(..)
  -- * Functions
  , socialContext
  -- * Values
  , domdereGithub
  , domdereRssFeed
  , domdereTwitter
  ) where

import qualified Ultra.Data.Text as T

import Data.Monoid ((<>))

import Hakyll (
    Context
  , constField
  )

import Preamble hiding ((<>))

data SocialDetails = SocialDetails {
    socialHandle :: !T.Text
  , socialUrl    :: !T.Text
  } deriving (Show, Eq)

domdereRssFeed :: SocialDetails
domdereRssFeed = SocialDetails
  "RSS Feed"
  "http://blog.forgetfulfunctor.com/atom.feed"

domdereTwitter :: SocialDetails
domdereTwitter = SocialDetails
  "@dom_dere"
  "https://twitter.com/dom_dere"

domdereGithub :: SocialDetails
domdereGithub = SocialDetails
  "@domdere"
  "https://github.com/domdere"

socialContext :: T.Text -> SocialDetails -> Context String
socialContext name (SocialDetails h url) =
      constField (T.unpack name <> "-handle") (T.unpack h)
  <>  constField (T.unpack name <> "-url") (T.unpack url)
