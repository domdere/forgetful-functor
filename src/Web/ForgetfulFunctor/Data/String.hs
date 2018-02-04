{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Web.ForgetfulFunctor.Data.String
-- Copyright    : (C) 2014 - 2018 Dom De Re
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Web.ForgetfulFunctor.Data.String (
  -- * Functions
    extractTeaser
  ) where

import Data.String (lines, unlines)

import Preamble

extractTeaser :: String -> String
extractTeaser = unlines . takeWhile (/= "<!-- more -->") . lines

