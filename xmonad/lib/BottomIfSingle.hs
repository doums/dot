{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

-- ⚠ This file is a modified version of XMonad.Layout.CenteredIfSingle
-- https://hackage.haskell.org/package/xmonad-contrib-0.17.1/docs/XMonad-Layout-CenteredIfSingle.html

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.CenteredIfSingle
-- Description :  If only a single window is shown, center it on screen
-- Copyright   :  (c) 2021 Leon Kowarschick
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  Leon Kowarschick. <TheElkOfWar@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout modifier that, if there is only a single window on screen, places
-- that window in the center of the screen.
-- This is especially useful on wide screen setups, where the window would otherwise
-- be unnecessarily far away from the center of your field of vision.
--
-----------------------------------------------------------------------------

module BottomIfSingle
  ( -- * Usage
    -- $usage
    bottomIfSingle, BottomIfSingle
  ) where

import XMonad
import XMonad.Layout.LayoutModifier
import XMonad.Prelude (fi)

-- $usage
-- You can use this module by including  the following in your @~\/.xmonad/xmonad.hs@:
--
-- > import XMonad.Layout.BottomIfSingle
--
-- and adding the 'bottomIfSingle' layoutmodifier to your layouts.
--
-- > myLayoutHook = bottomIfSingle 0.7 0.8 Grid ||| ...
--
-- For more information on configuring your layouts see "XMonad.Doc.Extending".


-- | Layout Modifier that places a window in the center of the screen,
-- leaving room on the left and right if there is only a single window.
-- The first argument is the horizontal and the second one the vertical
-- ratio of the screen the centered window should take up.  Both numbers
-- should be between 0.0 and 1.0.
data BottomIfSingle a = BottomIfSingle !Double !Double
  deriving (Show, Read)

instance LayoutModifier BottomIfSingle Window where
  pureModifier (BottomIfSingle ratioX ratioY) r _ [(onlyWindow, _)] = ([(onlyWindow, rectangleCenterPiece ratioX ratioY r)], Nothing)
  pureModifier _ _ _ winRects = (winRects, Nothing)

-- | Layout Modifier that places a window in the center of the screen,
-- leaving room on all sides if there is only a single window
bottomIfSingle :: Double -- ^ Horizontal ratio of the screen the centered window should take up; should be a value between 0.0 and 1.0
                 -> Double -- ^ Vertical ratio; should also be a value between 0.0 and 1.0
                 -> l a    -- ^ The layout that will be used if more than one window is open
                 -> ModifiedLayout BottomIfSingle l a
bottomIfSingle ratioX ratioY = ModifiedLayout (BottomIfSingle ratioX ratioY)

-- | Calculate the center piece of a rectangle given the percentage of the outer rectangle it should occupy.
rectangleCenterPiece :: Double -> Double -> Rectangle -> Rectangle
rectangleCenterPiece ratioX ratioY (Rectangle rx ry rw rh) = Rectangle startX startY width height
  where
    startX = rx + left
    startY = ry + top

    -- NOTE if one wants to use the layout with the window pushed
    -- to the edges of the screen, not centered, you should use
    -- `newSizeH` function for Both width and height calculations.
    width  = newSizeW rw left
    height = newSizeH rh top

    left = rw `scaleBy` ratioX
    top  = rh `scaleBy` ratioY

newSizeW :: Dimension -> Position -> Dimension
newSizeW dim pos = fi $ fi dim - pos * 2

newSizeH :: Dimension -> Position -> Dimension
newSizeH dim pos = fi $ fi dim - pos

scaleBy :: Dimension -> Double -> Position
scaleBy dim ratio = floor $ fi dim * (1.0 - ratio) / 2
