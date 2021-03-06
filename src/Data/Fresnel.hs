-- This file is part of fresnel
-- Copyright (C) 2015, 2016  Fraser Tweedale
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

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Data.Fresnel
  (
  -- | Grammars
    Grammar
  , parse
  , print
  , toPrism

  -- | Grammar constructors and combinators
  , element
  , satisfy
  , symbol
  , success
  , failure
  , many
  , many1
  , (<<*)
  , (*>>)
  , between
  , sepBy
  , sepBy1
  , literal
  , match
  , def
  , opt
  , eof
  , (<<*>>)
  , (<<+>>)
  , replicate
  , bind
  , (<<$>>)

  -- | 'Iso's
  , _NonEmpty

  , module Data.Fresnel.TH

  -- | Re-exports
  , Cons
  ) where

import Prelude hiding (print, replicate)
import Control.Applicative ((<|>))
import Control.Monad ((>=>))
import Data.Bifunctor (first)
import Numeric.Natural (Natural)

import Control.Lens hiding (element)
import Data.List.NonEmpty (NonEmpty(..))

import Data.Fresnel.TH

-- $setup
-- >>> import Data.Char
-- >>> import Data.Fresnel.Char


type Grammar s a = Prism' s (a, s)

element :: Cons s s a a => Grammar s a
element = _Cons

-- | Check element matches a predicate.
--
-- Note: This is /not/ a legal 'Prism', unless you are very careful
-- not to review an element that fails the predicate.
--
-- >>> parse (satisfy isAlpha) "a1"
-- Just 'a'
-- >>> parse (satisfy isAlpha) "1a"
-- Nothing
--
satisfy :: Cons s s a a => (a -> Bool) -> Grammar s a
satisfy f = element . filtered (f . fst)

-- | Check element for equality.
--
-- Note: This is /not/ a legal 'Prism', unless you are very careful
-- to only review values that are equal to the given element.
--
-- >>> parse (symbol 'a') "a1"
-- Just 'a'
-- >>> parse (symbol 'a') "1a"
-- Nothing
--
symbol :: (Cons s s a a, Eq a) => a -> Grammar s a
symbol a = satisfy (== a)

-- | Adapt the 'Grammar' with a 'Prism' or 'Iso'
--
-- >>> let g = reversed <<$>> many (satisfy isAlpha)
-- >>> parse g "live!"
-- Just "evil"
-- >>> print g "evil" :: String
-- "live"
--
-- >>> let shouting s = if all (not . isLower) s then Right (fmap toLower s) else Left s
-- >>> let g = prism (fmap toUpper) shouting <<$>> many element
-- >>> parse g "WOW!!!"
-- Just "wow!!!"
-- >>> parse g "meh"
-- Nothing
-- >>> print g "hello world" :: String
-- "HELLO WORLD"
--
(<<$>>) :: Prism' a b -> Grammar s a -> Grammar s b
p <<$>> g = g . swapped . aside p . swapped
infixr 4 <<$>>

-- | Sequence two grammars and combine their results as a tuple
--
-- >>> let g = integral <<*>> many (satisfy isAlpha)
-- >>> parse g "-10abc"
-- Just (-10,"abc")
-- >>> print g (42, "xyz") :: String
-- "42xyz"
--
-- >>> data Chars3 = Chars3 Char Char Char deriving (Show)
-- >>> let c3iso = iso (\((x,y),z) -> Chars3 x y z) (\(Chars3 x y z) -> ((x,y),z))
-- >>> let g = c3iso <<$>> digit <<*>> digit <<*>> digit
-- >>> parse g "007"
-- Just (Chars3 '0' '0' '7')
--
(<<*>>) :: Grammar s a -> Grammar s b -> Grammar s (a, b)
p1 <<*>> p2 = p1 . aside p2
  . iso (\(a, (b, s)) -> ((a, b), s)) (\((a, b), s) -> (a, (b, s)))
infixl 6 <<*>>


-- | Choice between two grammars
--
-- >>> let g = integral <<+>> satisfy isAlpha
-- >>> parse g "-10!"
-- Just (Left (-10))
-- >>> parse g "abc!"
-- Just (Right 'a')
-- >>> parse g "???"
-- Nothing
-- >>> print g (Left 42) :: String
-- "42"
-- >>> print g (Right 'x') :: String
-- "x"
--
(<<+>>) :: Grammar s a -> Grammar s b -> Grammar s (Either a b)
p1 <<+>> p2 = prism'
  (\(x, s) -> either (review p1 . (,s)) (review p2 . (,s)) x)
  (\x -> first Left <$> preview p1 x  <|>  first Right <$> preview p2 x)
infixr 5 <<+>>

-- | Run the grammar as many times as possible on the input,
-- returning or consuming a list.
--
-- >>> let g = many (satisfy isAlpha)
-- >>> parse g ""
-- Just ""
-- >>> parse g "abc!"
-- Just "abc"
-- >>> print g "xyz" :: String
-- "xyz"
--
many :: Grammar s a -> Grammar s [a]
many p = isoList <<$>> (p <<*>> many p) <<+>> success ()

isoList :: Iso' (Either (a, [a]) ()) [a]
isoList = iso f g where
  f = either (uncurry (:)) (const [])
  g (x:xs) = Left (x, xs)
  g [] = Right ()

failure :: Grammar s ()
failure = prism' snd (const Nothing)

success :: a -> Grammar s a
success a = prism' snd (Just . (a,))


-- | Run the grammar as many times as possible and at least once.
--
-- >>> let g = many1 (satisfy isDigit)
-- >>> parse g ""
-- Nothing
-- >>> parse g "42"
-- Just ('4' :| "2")
-- >>> print g ('1' :| "23") :: String
-- "123"
--
many1 :: Grammar s a -> Grammar s (NonEmpty a)
many1 g = _NonEmpty <<$>> g <<*>> many g where

_NonEmpty :: Iso' (a, [a]) (NonEmpty a)
_NonEmpty = iso (uncurry (:|)) (\(h :| t) -> (h, t))

-- | Sequence two grammars, ignoring the second value.
--
-- >>> let g = integral <<* literal '~'
-- >>> parse g "123~"
-- Just 123
-- >>> parse g "123!"
-- Nothing
-- >>> print g 123 :: String
-- "123~"
--
(<<*) :: Grammar s a -> Grammar s () -> Grammar s a
p1 <<* p2 = iso (\(a, ()) -> a) (\a -> (a, ())) <<$>> p1 <<*>> p2
infixl 6 <<*

-- | Sequence two grammars, ignoring the first value.
--
-- >>> let g = literal '~' *>> integral
-- >>> parse g "~123"
-- Just 123
-- >>> parse g "123"
-- Nothing
-- >>> print g 123 :: String
-- "~123"
--
(*>>) :: Grammar s () -> Grammar s a -> Grammar s a
p1 *>> p2 = iso (\((), a) -> a) (\a -> ((), a)) <<$>> p1 <<*>> p2
infixl 6 *>>

-- | Replicate a grammar N times
--
-- >>> let g = replicate 3 (satisfy isAlpha)
-- >>> parse g "ab3"
-- Nothing
-- >>> parse g "abc"
-- Just "abc"
-- >>> print g "abcd" :: String  -- note there are FOUR dots
-- "abc"
-- >>> print g "ab" :: String  -- can't do much about this
-- "ab"
--
replicate :: Natural -> Grammar s a -> Grammar s [a]
replicate 0 _ = success []
replicate n g = isoList <<$>> (g <<*>> replicate (n - 1) g) <<+>> failure

-- | Sequence a grammar based on functions that return the next
-- grammar and yield a determinant.
--
-- >>> let g = bind integral (\n -> replicate n (satisfy isAlpha)) (fromIntegral . length)
-- >>> parse g "3abc2de?"
-- Just "abc"
-- >>> parse g "3ab2de?"
-- Nothing
-- >>> parse (many g) "3abc2de1f?"
-- Just ["abc","de","f"]
-- >>> print (many g) ["hello", "world"] :: String
-- "5hello5world"
--
bind :: Grammar s a -> (a -> Grammar s b) -> (b -> a) -> Grammar s b
bind p f g = prism'
  (\(b, s) -> review p (g b, review (f (g b)) (b, s)))
  (preview p >=> \(a, s') -> preview (f a) s')

-- | Given left and right "surrounding" grammars and an interior
-- grammar sequence all three, discarding the surrounds.
--
-- >>> let g = between (literal '<') (literal '>') integral
-- >>> parse g "<-123>"
-- Just (-123)
-- >>> print g 42 :: String
-- "<42>"
--
between :: Grammar s () -> Grammar s () -> Grammar s a -> Grammar s a
between l r a = l *>> a <<* r

-- | Given grammars for element and separator, construct grammar for
-- elements separated by the separator.
--
-- >>> let g = satisfy isDigit `sepBy` literal '.'
-- >>> parse g "1.2.3."
-- Just "123"
-- >>> parse g "."
-- Just ""
-- >>> print g "123" :: String
-- "1.2.3"
--
sepBy :: Grammar s a -> Grammar s () -> Grammar s [a]
sepBy g sep = isoList <<$>> (g <<*>> many (sep *>> g)) <<+>> success ()

-- | Given grammars for element and separator, construct grammar for
-- elements separated by the separator, with at least one element.
--
-- >>> let g = satisfy isDigit `sepBy1` literal '.'
-- >>> parse g "1."
-- Just ('1' :| "")
-- >>> parse g "1.2.3."
-- Just ('1' :| "23")
-- >>> parse g "."
-- Nothing
-- >>> print g ('1' :| "23")  :: String
-- "1.2.3"
--
sepBy1 :: Grammar s a -> Grammar s () -> Grammar s (NonEmpty a)
sepBy1 g sep = _NonEmpty <<$>> g <<*>> many (sep *>> g)

-- | Consumes or produces a literal character (mapped to '()').
--
-- >>> let g = literal '$'
-- >>> parse g "$~"
-- Just ()
-- >>> print g () :: String
-- "$"
--
literal :: (Cons s s a a, Eq a) => a -> Grammar s ()
literal a = match (symbol a) a

-- | Match grammar and discard result.  Print "canonical" value.
--
-- >>> let g = match (many1 (symbol '>') <<*>> many1 space) ('>' :| ">>", ' ' :| "")
-- >>> parse g ">>> foo"
-- Just ()
-- >>> parse g ">>>>foo"
-- Nothing
-- >>> print g () :: String
-- ">>> "
--
match :: Grammar s a -> a -> Grammar s ()
match g a = iso (const ()) (const a) <<$>> g

-- | Give a default value for a grammar.
--
-- A defaulted grammar can always be viewed.  If a reviewed value
-- is equal to the default nothing is written.
--
-- >>> let g = def 0 integral
-- >>> parse g "1~"
-- Just 1
-- >>> parse g "~"
-- Just 0
-- >>> print g 1 :: String
-- "1"
-- >>> print g 0 :: String
-- ""
--
def :: Eq a => a -> Grammar s a -> Grammar s a
def a' p = iso f g <<$>> p <<+>> success a' where
  f = either id id
  g a = if a == a' then Right a else Left a

-- | Make a grammar optional; a failed view yields 'Nothing' and
-- a review of 'Nothing' writes nothing.
--
-- >>> let g = opt integral
-- >>> parse g "1~"
-- Just (Just 1)
-- >>> parse g "~"
-- Just Nothing
-- >>> print g (Just 1) :: String
-- "1"
-- >>> print g Nothing :: String
-- ""
--
opt :: Grammar s a -> Grammar s (Maybe a)
opt p = iso f g <<$>> p <<+>> success () where
  f = either Just (const Nothing)
  g = maybe (Right ()) Left

-- | Matches at end of input; writes nothing
--
-- >>> parse eof ""
-- Just ()
-- >>> parse (eof *>> eof) ""
-- Just ()
-- >>> parse eof "~"
-- Nothing
-- >>> print eof () :: String
-- ""
--
eof :: Cons s s a a => Grammar s ()
eof = prism' snd (\s -> maybe (Just ((), s)) (const Nothing) (uncons s))

-- | Parse with a grammar, discarding any remaining input.
--
-- If remaining input is an error, apply '(<<* eof)'
-- to your grammar first.
--
parse :: Grammar s b -> s -> Maybe b
parse g s = fst <$> preview g s

-- | Print with a grammar
--
print :: Monoid s => Grammar s b -> b -> s
print g b = review g (b, mempty)

-- | Turn a Grammar into a prism, implicitly adding 'eof'
--
toPrism :: (Cons s s a a, Monoid s) => Grammar s b -> Prism' s b
toPrism g = prism' (print g) (parse (g <<* eof))
