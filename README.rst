Prism-based unified parsers and pretty printers
===============================================

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

  replicateG :: Natural -> Grammar s a -> Grammar s [a]


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

  bindG :: Grammar s a -> (a -> Grammar s b) -> (b -> a) -> Grammar s b


By the way, I lied about the type of ``(<<$>>)``.  It can also be
used with any old ``Prism'``.  A failed ``preview`` is a parse
failure.  Isos are just prisms that never fail!  Here's the real
type::

  (<<$>>) :: Prism' a b -> Grammar s a -> Grammar s b
