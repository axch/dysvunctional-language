DVL
===

A prototype compiler for running the code you want to write as fast as
the code you have to write (as long as it's numerical).

By which I mean that DVL is a language and compiler for writing the
numerical kernel of your system in, instead of Fortran or C.  The DVL
input language is much nicer and higher level, a dialect of Lisp; but
more important, *high level DVL constructs compile to efficient code*.

Why yet another compiler?  Isn't it already possible to get within
3-5x of Fortran with a little annotation of Common Lisp or Haskell?
Yes, but only by writing Fortran in parentheses (or in monads).  I
know of no compiler where modularity is costless (except research
systems along the lines of DVL).

It's not costless in DVL either, but in DVL you pay for it with
resource-intensive compilations rather than slow runtimes.  In the
numeric context, all the modularity constructs you know and love: data
structures, closures, higher-order functions, abstraction boundaries
of every stripe -- even automatic differentiation -- are just
scaffolding for carrying numbers hither and yon to be added and
multiplied.  DVL solves the scaffolding at compile time and emits
efficient code to do the arithmetic at runtime.

Disclaimer: DVL is a *prototype*.  The language is minimal (but you
don't need much to predict the positions of the planets); the error
messages are inscrutable; and the commandline interface is inflexible.
DVL is not curretnly polished enough to be really used by someone who
does not have time to understand its internals.  You have been warned.

- [So where do I get it?](#installation)
- [So how do I use it?](#usage)
- [What's with the name?](#history)
- [Where did this come from?](#history)
- [I can haz legal?](#license)

Installation
============

DVL is written primarily in MIT Scheme and distributed in source form,
so you will need to install that first.  If you want to use any other
backend besides compiling through MIT Scheme, you will need to install
that system.  In particular, the Haskell backend, in addition to
generating Haskell, is written in Haskell, so you will need GHC twice
for that.

When ready, just `git clone` this repository and then `git submodule
init` and `git submodule update` to pull in code dependencies.  Then

```
dysvunctional-language/dvl/dvl <dvl-program-file>
```

and you're off to the races.  Or fire up your MIT Scheme and

```scheme
(load "dysvunctional-language/dvl/load")
```

to use DVL as a library.

Usage
=====

*DVL is a prototype*.  The purpose of making it public is
experimentation, collaboration, and elucidation.  You are not expected
to be able to use it without considerable source diving -- but the
source is intended to be legible.

That said, read `dvl/README` to get started, or the `dvl` shell script
to see the entry points.

The code tree is organized as follows:

Directories

- `fol/`         Implementation of FOL (intermediate target for VL and DVL)
- `vl/`          Implementation of VL (uses fol/)
- `dvl/`         Implementation of DVL (some refers to vl/; uses fol/)
- `support/`     General libraries
- `support/pattern-case/`   Schemely pattern-matching macro (submodule)
- `support/rules/`   Rule-based term-rewriting system (submodule)
- `testing/`     Unit test framework (submodule)
- `*/test/`      Unit tests for *

Important Files

- `*/load.scm`   Scheme entry point for *
- `./README`     This document
- `*/README`     Entry point for * documentation
- `fol/fol`      Commandline entry point for FOL
- `vl/vl`        Commandline entry point for VL
- `dvl/dvl`      Commandline entry point for DVL

Technical Summary
=================

DVL consists of three main pieces:

1. DVL proper,
2. VL, a more expository and less capable compiler sharing a lot
   of code with DVL, and
3. FOL, which serves as a compilation target for DVL and VL, and in
   turn compiles to (low-level, efficient) Scheme, Lisp, or Haskell.

In reverse order,

3) FOL is the intermediate language for VL and DVL.  Instead of being
entirely internal, FOL is a standalone compiler for a statically typed
first-order language with Scheme syntax (which makes it easy to
generate programmatically).  The FOL compiler is dominated by a FOL to
FOL optimizer, which performs inlining, scalar replacement of
aggregates, dead code elimination, common subexpression elimination,
and some algebraic simplification.  Optimized FOL can then be compiled
to linkable libraries or executables via any of several backends.

2) VL is a compiler implementing union-free polyvariant flow analysis
on a (almost) functional source language.  It generates FOL, and
therefore does not itself contain any post-analysis optimization.

1) DVL is a variation on VL extended to be able to reason about
gensyms (unique values); DVL the language adds the primitives GENSYM
and GENSYM= to VL.  Gensyms are sufficient to implement forward mode
AD correctly by operator overloading; the flow analysis suffices to
migrate all the boxing, tagging, dispatching, and unboxing that this
normally entails to compile time.  DVL also adds a rudimentary foreign
interface.

History
=======

DVL is based on
[Stalingrad](http://www.bcl.hamilton.ie/~qobi/stalingrad/), which was
developed by Professors [Jeffrey Mark
Siskind](https://engineering.purdue.edu/~qobi/) at Purdue University
and [Barak A. Pearlmutter](http://www.bcl.hamilton.ie/~barak/) at the
Hamilton Institute, with work beginning at least as early as 2004.

Stalingrad is an explicitly
[automatic-differentiation](http://en.wikipedia.org/wiki/Automatic_differentiation)-aware
compiler for scientific computing, building on Professor Siskind's
earlier work on the Stalin compiler for Scheme.  The source language
Stalingrad compiles is called VLAD, for Vunctional Language with
Automatic Differentiation -- a language that, true to its name, is
purely functional (except for a minimal i/o facility), and provides
built-in operators for invoking AD.

I had the privilege of having a post-doctoral appointment with
Professor Pearlmutter between the fall of 2010 and the summer of 2012,
working on compilation for numerical computing and automatic
differentiation.  During that time, I rewrote the non-AD portion of
VLAD in a system called, appropriately enough, VL (included in the
`vl/` directory) as a more expository implementation of the
compilation technique at the heart of Stalingrad.

After VL, I found myself taking this same compilation method in a
slightly different direction than VLAD and Stalingrad.  I noticed that
it essentially still worked even in the presence of the slight
language impurity of being able to generate unique objects -- which is
sufficient for an implementation of AD in user space, without having
to increase the (already considerable) complexity of the compiler with
it.  Thus was born the DysVunctional Language.

I have been the primary author of DVL to date, but my coworkers at the
Hamilton Institute cannot go unmentioned.  I am indebted to Professor
Pearlmutter himself, to David Rush, and to Oleksandr Manzyuk.  In
particular, the GHC and Javascript backends are due to him.  The
project also would not have been possible without access to Professor
Siskind, both for inspiration from the Stalingrad sources, and for
numerous technical conversations.  The early and, so far, majority of
the development of DVL was supported by Science Foundation Ireland
Principal Investigator grant 09/IN.1/I2637.

--Alexey Radul, Boston, 2013
