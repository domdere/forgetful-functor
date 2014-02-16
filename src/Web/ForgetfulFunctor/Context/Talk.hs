{-# LANGUAGE OverloadedStrings #-}

-------------------------------------------------------------------
-- |
-- Module       : Web.ForgetFunctor.Context.Talk
-- Copyright    : (C) 2014 Dom De Re
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- Contexts for Talks.
-------------------------------------------------------------------
module Web.ForgetfulFunctor.Context.Talk (
    -- * Extracts the contexts from Talks
        talkCtx
    ,   talkListContext
    ) where

import Data.Monoid
import Data.Foldable ( fold )
import Hakyll
    (   Context
    ,   Item
    ,   constField
    ,   dateField
    ,   defaultContext
    ,   listField
    )

-- | Extracts the context for a single talk
--
talkCtx :: Context String
talkCtx = fold
    [   dateField "date" "%B %e, %Y"
    ,   constField "talks" "Yes"
    ,   defaultContext
    ]

-- | Extracts the context for all the talks
--
talkListContext :: [Item String] -> Context String
talkListContext ts = fold
    [   listField "talkList" talkCtx (return ts)
    ,   constField "talks" "Yes"
    ,   defaultContext
    ]
