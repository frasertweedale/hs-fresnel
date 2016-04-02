-- This file is part of fresnel
-- Copyright (C) 2015  Fraser Tweedale
--
-- fresnel is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Data.Fresnel.Char
  (
    digit
  , integral
  ) where

import Prelude hiding (print)
import Control.Lens
import Data.Char (isDigit)

import Data.Fresnel

-- $setup
-- >>> import Numeric.Natural
-- >>> import Data.Fresnel

digit :: (Cons s s Char Char) => Grammar s Char
digit = satisfy isDigit

-- |
-- >>> parse integral "01." :: Maybe Integer
-- Just 1
-- >>> parse integral "-1" :: Maybe Natural
-- Nothing
-- >>> print integral 42 :: String
-- "42"
-- >>> print integral (-42) :: String
-- "-42"
--
integral
  :: (Cons s s Char Char, Integral a, Read a, Show a)
  => Grammar s a
integral =
  iso (\(c,s) -> maybe s (:s) c) (Nothing,) . _Show
  <<$>> (opt (symbol '-')) <<*>> (many digit)
