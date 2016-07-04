{-# LANGUAGE FlexibleContexts #-}

import Prelude as P

import Data.Fresnel as F
import Data.Fresnel.Char as F
import qualified Data.Text.Lazy as TL


main :: IO ()
main = P.print $ TL.last $ F.print g ['a'..]
  where
  g :: Cons s s Char Char => Grammar s String
  g = digit `sepBy` literal ','
