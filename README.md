
# iTeML : Inline (Unit) Tests for OCaml

iTeML is an inline test extraction utility, originally
developed internally for [Batteries][bat] under the
name "qTest".


## qTest2

qTest version 2.1 is the current implementation, and it considered stable and
feature-complete. It is available under the _qtest/_ directory of this repository.
It has extensive (but not quite up-to-date) [documentation][doc],
as well as good syntax highlighting for Kate (KatePart: KWrite, KDevelop, Konqueror,...)
and basic support for Emacs.

For a sanity check of the system and a raw demonstration of available syntaxes,
see the _qtest/tests/_ subdirectory.

qTest2 relies on [oUnit](ounit.forge.ocamlcore.org/) to provide the underlying testing framework.

Backwards-incompatible changes are unlikely to be considered.

### Building, installing, un-installing

Execute one of:

    ocaml do.ml qtest build   [prefix]
    ocaml do.ml qtest install [prefix]
    ocaml do.ml qtest remove  [prefix]

OASIS will be used as backend.

## Future works

There are ideas floating around on how to improve qTest2, generally revolving
around going from a test "extraction" to an "annotation" model. The new version
will be called _iteml_, and can be thought of as _qtest3_.
No timetable is set yet, as all parties involved are busy bees.

All ideas, included (and especially!) crazy ones, are welcome at this stage,
and backwards compatibility with qtest2 is not a consideration.


## History of the project

(or at least, what I (VH) can unearth of it thanks to git logs)

 * 2007--2008  : Ilmari Heikkinen writes _make_suite.rb_ for his Prelude.ml.
 * Jan 17, 2011: _make_suite.rb_ is copied into Batteries. (=qtest0)
 * Jan 27, 2011: Kaustuv Chaudhuri writes from scratch
   an equivalent _make_suite.mll_ to replace the Ruby script. (=qtest1)
 * Jan 19, 2012: Vincent Hugot writes from scratch a new version, with a lot
   of new features. Syntax changes a bit.  (=qtest2)
 * Oct 21, 2012: qtest2 moves to its own repository.

Over time, the various versions of qtest have received additional contributions by:
Eric Norige, Gabriel Scherer, Cedric Cellier, Valentin Gatien-Baron, Max Mouratov
and Simon Cruanes

## Contact

The issues system of github seems the sanest way for now.

Current developer & maintainer: Vincent Hugot.


[bat]: http://batteries.forge.ocamlcore.org/ "Batteries"
[doc]: qtest/doc/qtest.adoc "qTest Documentation"
