Prism-based unified parsers and pretty printers
===============================================

Synopsis
--------

Defining a grammar:

.. code:: haskell

  {-# LANGUAGE TemplateHaskell #-}

  import Data.Fresnel
  import Data.Fresnel.Char
  import Data.Fresnel.TH

  -- | A contrived and poorly-constrained type for phone numbers
  --
  data PhoneNumber = PhoneNumber
    { phoneAreaCode :: String
    , phoneNumber :: String
    }
  makeIso ''PhoneNumber

  phoneNumberG :: Cons s s Char Char => Grammar s PhoneNumber
  phoneNumberG = _PhoneNumber <<$>>
    literal '(' *>> replicate 2 digit <<* literal ')'
    <<*   match (many space) " "
    <<*>> replicate 8 (match (many space) "" *>> digit)

Using a grammar::

  λ> parse phoneNumberG ("(07)3456  78  9  0" :: String)
  Just (PhoneNumber "07" "34567890")

  λ> print phoneNumberG (PhoneNumber "07" "34567890") :: String
  "(07) 34567890"


Library overview
----------------

*fresnel* is a combinator library for building first class unified
parser/printers out of prisms and isos.  A parser/printer is a
``Prism``, but most functions reference the ``Grammar`` type alias::

  type Grammar s a = Prism' s (a, s)


The prism can be reviewed to parse, and previewed to print.  The
``parse`` and ``print`` functions are provided for convenience::

  parse :: Grammar s b -> s -> Maybe b
  parse g s = fst <$> preview g s

  print :: Monoid s => Grammar s b -> b -> s
  print g b = review g (b, mempty)


Many familiar combinators are provided.  Some have the
``Control.Lens.Cons.Cons`` type class constraint which provides for
generic parser/printer that work with many types that can be
(de)constructed "piecewise".

::

  satisfy :: Cons s s a a => (a -> Bool) -> Grammar s a

  symbol :: (Cons s s a a, Eq a) => a -> Grammar s a

  eof :: Cons s s a a => Grammar s ()

  between :: Grammar s () -> Grammar s () -> Grammar s a -> Grammar s a

  many :: Grammar s a -> Grammar s [a]

  many1 :: Grammar s a -> Grammar s (NonEmpty a)

  replicate :: Natural -> Grammar s a -> Grammar s [a]


The analogue of ``fmap``/``(<$>)`` is ``(<<$>>)`` which composes an
``Iso`` into the grammar.

::

  (<<$>>) :: Iso' a b -> Grammar s a -> Grammar s b


Product types (records) and sum types (choices) can be handled with
special combinators in conjunction with isos.  *Template Haskell*
code is provided for automatically generating appropriate ``Iso``
values for your types.

::

  (<<*>>) :: Grammar s a -> Grammar s b -> Grammar s (a, b)

  (<<+>>) :: Grammar s a -> Grammar s b -> Grammar s (Either a b)


Combinators for sequencing and an analogue of ``(>>=)`` are also
provided::

  (<<*) :: Grammar s a -> Grammar s () -> Grammar s a

  (*>>) :: Grammar s () -> Grammar s a -> Grammar s a

  bind :: Grammar s a -> (a -> Grammar s b) -> (b -> a) -> Grammar s b


By the way, I lied about the type of ``(<<$>>)``.  It can also be
used with any old ``Prism'``.  A failed ``preview`` is a parse
failure.  Isos are just prisms that never fail!  Here's the real
type::

  (<<$>>) :: Prism' a b -> Grammar s a -> Grammar s b


You can consume and print a literal value, or match an arbitrary
grammar while printing a "canonical" value on review::

  literal :: (Cons s s a a, Eq a) => a -> Grammar s ()

  match :: Grammar s a -> a -> Grammar s ()


Mapping to/from custom data types
---------------------------------

You can automatically generate ``Iso`` values for custom data types
via the ``makeIso`` Template Haskell function::

  {-# LANGUAGE TemplateHaskell #-}

  import Data.Fresnel.TH (makeIso)

  data Foo = A Int Char | B Bool
  makeIso ''Foo

This will create the ``Iso``::

  _Foo :: Iso' (Either (Int, Char) Bool) Foo
