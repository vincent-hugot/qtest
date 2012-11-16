
# iTeML : Inline (Unit) Tests for OCaml

iTeML is an inline test extraction utility, originally
developed internally for [Batteries][bat] under the
name "qTest".


## qTest2 

qTest version 2 is the current implementation, and it considered stable and
feature-complete. It is available under the _qtest/_ directory of this repository.
It has extensive (but not quite up-to-date) [documentation][doc],
as well as good syntax highlighting for Kate (KatePart: KWrite, KDevelop, Konqueror,...)
and basic support for Emacs.

For a sanity check of the system and a raw demonstration of available syntaxes,
see the _qtest/tests/_ subdirectory.

qTest2 relies on oUnit to provide the underlying testing framework.

Backwards-incompatible changes are unlikely to be considered.

## Future Works

There are ideas floating around on how to improve qTest2, generally revolving
around going from a test "extraction" to an "annotation" model. The new version
will be called _iteml_, and can be thought of as _qtest3_.
No timetable is set yet, as all parties involved are busy bees.

All ideas, included (and especially!) crazy ones, are welcome at this stage,
and backwards compatibility with qtest2 is not a consideration.


## Contact

The issues system of github seems the sanest way for now.

Current developer & maintainer: Vincent Hugot.


[bat]: http://batteries.forge.ocamlcore.org/ "Batteries"
[doc]: http://batteries.vhugot.com/qtest/ "qTest Documentation"