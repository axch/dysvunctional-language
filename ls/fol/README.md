FOL (First-Order Language)
==========================

FOL is a simple-minded, typed, first-order language, which serves as
the compilation target for VL and DVL.  FOL has several design
objectives:

- FOL must be a convenient target for code generation after flow analysis
- FOL must be compilable to efficient machine code
- FOL should not be unduly illegible

In pursuit of these design objectives, FOL is a first-oder subset of
MIT Scheme, augmented with type declarations, supporting a limited
range of constructs.

The present code collection includes a FOL type checker, an in-memory
optimizing compiler from FOL to itself, a runtime library for
executing FOL as MIT Scheme, and compilers from FOL to native code via
MIT Scheme and SBCL.

The remainder of this document consists of five sections,
corresponding to the reasons why you might wish to peruse it:

- [Reading FOL](#reading-fol) If you just want to understand the final
  output of the VL and DVL compilers.
- [Optimizing FOL](#optimizing-fol) If you want to know what
  optimizations FOL provides to VL and DVL and what they do.
- [Writing FOL](#writing-fol) If you want to know the constraints FOL
  imposes on the VL and DVL compilers and how they enable the FOL
  optimizations.
- [Implementing FOL](#implementing-fol) If you want to know how the
  FOL optimizer itself works.
- [Future of FOL](#future-of-fol) If you want to know the evolution
  plans.


Reading FOL
===========

FOL is a subset of Scheme, requiring minimal additional runtime
support.  You can run any FOL program right at your Scheme REPL:

- `(fol-eval <program>)`

  Evaluates a FOL program in the ambient MIT Scheme.  This has the
  same effect as typing a printout of that data structure into the
  REPL (provided the FOL runtime support is loaded).

You can also read FOL as though it were Scheme.  The only things you
might not recognize are multiple value returns, type declarations, and
some of the primitives.

The multiple value returns are actually standard Scheme, augmented
with the `let-values` macro from SRFI 11.  What they do should be
obvious.  The type declarations serve to aid the FOL optimizer, and
while they can be used to type-check a FOL program, you can also just
ignore them.  In fact, the FOL runtime defines `argument-types` as a
macro that expands into nothing.  As to the primitives, FOL comes
with the following procedures predefined:

```
CONS, CAR, CDR, VECTOR, VECTOR-REF, ABS, EXP, LOG, SIN, COS, TAN,
ASIN, ACOS, SQRT, +, -, *, /, ATAN, EXPT, <, <=, >, >=, =, ZERO?,
POSITIVE?, NEGATIVE?, READ-REAL, WRITE-REAL, REAL, GENSYM, GENSYM=,
and GENSYM<.
```

Those with Scheme equivalents have the same semantics.  `real` is the
identity function, but instructs the compiler to treat its argument as
an unknown real number.  `read-real` and `write-real` do i/o; `write-real`
returns the number written.  `gensym` synthesizes a fresh object that is
guaranteed to be distinct from all other objects.  `gensym=` tests
whether two objects were created by the same dynamic call to `gensym`.
`gensym<` tests whether its first argument was created by a dynamically
strictly earlier call to `gensym` than its second.

FOL is specified to be tail-recursive and memory-managed.  Order of
evaluation of procedure arguments is unspecified.  As of the present
writing, the compilation may fail to preserve the order (or even
presence!) of side-effects.  FOL is notionally strict, but the
compilation may rearrange the order of evaluation of various
subexpressions, even across procedure boundaries.


Optimizing FOL
==============

The FOL optimizer cleans up the mess that the VL and DVL compilers
emit, so that it has some hope of being read by a person, and doesn't
reach too far outside the expectations of any subsequent compiler to
whose input language FOL may be translated.

Specifically, VL and DVL produce huge piles of tiny procedures that
pass each other huge piles of data structures (mostly closure records)
that contain each other, with many useless layers and redundant paths.
To clean this up, the FOL optimizer aggressively inlines procedures,
aggressively replaces aggregates with corresponding collections of
scalars, and aggressively eliminates dead and redundant code.

The standard use case of `compile-to-fol` emits FOL with all these
optimizations already performed.

- `(compile-to-fol <vl-or-dvl>)`
- `(compile-to-fol <vl-or-dvl> <adverb> ...)`

  Performs flow analysis on the given VL or DVL program (both VL and
  DVL define `compile-to-fol` routines), then generates and optimizes
  FOL that does the same job, and returns the result.  Any adverbs
  passed modify how each step is performed.

You can modify the compilation process by passing `compile-to-fol` an
adverb for how to do the job.  For example, calling

```scheme
(compile-to-fol (dvl-source "examples/celestial.dvl") visibly)
```

will do the same compilation process as plain `compile-to-fol`, but also
print out a two-line status report for every stage of compilation,
noting the size of the input it was given and the amount of time it
took to complete.  The available adverbs are

- `visibly`

  Print short progress reports, with stage name, input size, and time
  to completion.

- `volubly`

  Print longer progress reports, with stage name, size and several
  statistics about the input, and time to completion.

- `watching-memory`

  Produce reports about system memory use.

- `measuring-memory`

  Produce reports about total size of arguments and system memory use.
  When intermediate structures are large, this can take time and space
  comparable to the compilation itself.

- `type-safely`

  Check that each intermediate state that is supposed to be well typed
  actually is.  Print the type of the entry point if so, and signal an
  error if not.

All the subsequent procedures in this section accept adverb arguments,
except the ones that do trivial jobs, such as checking whether a
particular property holds of the program or not (namely,
`unique-names?`, `alpha-rename?`, `approximate-anf?`, `strip-argument-types`,
`let->let*`, and `let*->let`).

You can also get the output from the code generator, examine it, and
optimize it separately.

- `(analyze-and-generate <vl-or-dvl>)`
- `(compile-to-raw-fol <vl-or-dvl>)`

  Performs flow analysis on the given VL or DVL program (both VL and
  DVL define `analyze-and-generate` routines (`compile-to-raw-fol` is an
  alias)) and generates unoptimized FOL that does the same job.

- `(check-fol-types <program>)`

  Verifies that the given FOL program is syntactically correct and
  well typed.  If so, returns the type returned by the entry point; if
  not, signals an appropriate error.

- `(fol-optimize <program>)`

  Completely optimizes the given FOL program and returns the result.

FOL optimization proceeds as a cascade of stages, which you can also
run one by one to see their effect.  The stages are all
self-contained, in that you can run them in any order.  Some stages
rely on the program being normalized in one way or another; they will
preprocess it into the needed form before proceeding (if you are
watching with `visibly` or `volubly`, this may cause you to see multiple
stages execute even though you only asked for one).  The stages try to
be smart about knowing which stages preserve which properties and thus
not repeating normalization work, but may sometimes do more than is
strictly necessary.  If you are curious about this, see `optimize.scm`.

These are the individual FOL stages:

- `(inline <program>)`

  Inlines as many procedures in the given FOL program as possible and
  returns the result.  A procedure may not be inlined if it calls
  itself, or if inlining it would lead to too large of an increase in
  the size of the program.

- `(intraprocedural-cse <program>)`

  Eliminates common subexpressions inside procedure bodies and returns
  the result (this automatically also propagates constants and
  copies).  As suggested by its name, this does not do full
  interprocedural CSE, and does not detect circumstances where some
  formal parameters to some procedure are reliably always given the
  same value.

  The definition of "common subexpression" includes simple
  equivalences of the form

  ```scheme
    (car (cons x y)) -> x
    (+ 0 x) -> x
    (* 1 x) -> x
    (* 0 x) -> 0
  ```

  The first of these eliminates a great many of the structure accesses
  that VL and DVL leave around, permitting dead code elimination to
  get rid of a great deal of consing.  The last of these also has
  significant effects on the code size and speed, because it chains
  and may allow elimination of `x`.  It is not, however, floating-safe,
  because `x` might have been a NaN or an infinity.

- `(eliminate-intraprocedural-dead-code <program>)`

  Eliminates dead code inside procedure bodies and returns the result.
  As suggested by the name, this does not detect circumstances where
  some variable is passed around from one user-defined procedure to
  another but then never used.

- `(scalar-replace-aggregates <program>)`

  Performs scalar replacement of aggregates and returns the resulting
  program.  All possible data structure allocations (namely, instances
  of `cons`, `vector`, and user-defined constructors) are replaced by
  appropriate use of `values` and `let-values`.  This is the stage that
  first introduces them.  If the input program is union-free, this
  will eliminate all consing except for the construction of the final
  answer that is returned from the entry point, should that be a
  structure.

- `(eliminate-interprocedural-dead-code <program>)`

  Detects circumstances when some value is passed around from one
  procedure to another but never used, and flushes the computation and
  storage of those values.  Includes elimination of intraprocedural
  dead code.

- `(eliminate-dead-types <program>)`

  Removes unused type definitions from the given program and returns
  the result.

- `(reverse-anf <program>)`

  Inlines the definitions of variables that are used only once.  This
  is useless to the compiler, but clarifies the output.

In addition to running the stages proper, you can also run the
normalization procedures that the stages use.  The checking procedures
listed here are correct and expensive; this is not the mechanism by
which the FOL stages know whether to run a given normalization or not.

- `(alpha-rename <program>)`

  Renames variables in the given program so that no local variable
  binding binds the same name as any other, and returns the result.
  This is useful all over the place because it allows the system not
  to worry about scoping rules.

- `(unique-names? <program>)`

  Returns `#t` if all local variable names in the program are unique,
  and `#f` otherwise.

- `(alpha-rename? <program> <program>)`

  Checks whether the two given programs are equal up to names of bound
  variables.

- `(approximate-anf <program>)`

  Converts the given program into approximate A-normal form, where all
  procedure calls operate on variables or constants, and returns the
  result.  This is useful for CSE and SRA because it gives names to
  intermediate results, and information can be associated with those
  names.

- `(approximate-anf? <program>)`

  Returns `#t` if the given program is already in approximate A-normal
  form, and `#f` otherwise.

- `(lift-lets <program>)`

  Expands the scopes of variables as much as possible without changing
  whether and when something is computed.  This is useful for CSE
  because computed values remain in scope longer.

- `(lets-lifted? <program>)`

  Returns `#t` if the given program already has all its lets lifted, and
  `#f` otherwise.


In addition to the optimizer and its individual stages, FOL also
provides some legibility aids.  There is a utility to remove the
argument type declarations if desired.  The result will no longer type
check (obviously), and will cease to be acceptable as input to several
of the optimization stages, but will still execute in MIT Scheme, and
may be easier to read.

- `(strip-argument-types <program>)`

  Removes the argument type declarations from a FOL program and
  returns the result (which can no longer be properly called a FOL
  program).

There is also a utility to convert user structure definitions to
vectors (constructors become `vector` and accesses become `vector-ref`).

- `(structure-definitions->vectors <program>)`

  Replaces Scheme record types with Scheme vectors in a FOL program
  and returns the result.  Said result has no `define-type` forms.

Finally, you can also compress staircases of `let`s into `let*` forms,
if that helps you read them.

- `(let->let* <program>)`

  Changes all strings of one-binding `let`s into `let*` blocks and returns
  the result (which can no longer be properly called a FOL program).

- `(let*->let <program>)`

  Changes all `let*` blocks into strings of one-binding `let`s and returns
  the result (which can again be properly called a FOL program).


Machine code from FOL
---------------------

FOL includes a compiler to machine code via the MIT Scheme compiler,
and experimental backeds via the Stalin compiler and via Steel Bank
Common Lisp.

- `(fol->mit-scheme <program> [<basename>])`

  Dumps the given FOL program into `<basename>.fol-scm` and compiles
  that with MIT Scheme.  The result can be executed by loading it into
  an environment that already contains the FOL runtime.  The basename
  defaults to `frobnozzle`, local to the current directory.

- `(fol->floating-mit-scheme <program> [<basename>])`

  Like `fol->mit-scheme`, but replaces all of MIT Scheme's generic
  numeric procedures with specialized floating point versions.  This
  is faster, but only safe if you did not intend to do any exact
  arithmetic in your program.  (Vector indexing is not broken).

- `(run-mit-scheme [<basename>])`

  Executes a program previously compiled with `fol->mit-scheme` or
  `fol->floating-mit-scheme`.

- `(fol->standalone-mit-scheme <program> [<basename>])`

  Akin to `fol->mit-scheme`, but includes the FOL runtime in the
  dumped file.  The resulting `<basename>.com` can be executed by
  loading it into a fresh MIT Scheme.  Do not use `run-mit-scheme` on
  the output of this; the redefinition of the runtime will cause
  problems.

- `(fol->stalin <program> [<basename>])`

  Dumps the given FOL program into `<basename>.sc` and compiles that
  with Stalin.  The result is a standalone executable.

- `(fol->common-lisp <program> [<basename>])`

  Dumps the given FOL program into `basename.lisp` and compiles that
  with SBCL.  The result is a fasl file that can be loaded into SBCL.

- `(run-common-lisp [<basename>])`

  Call out to SBCL from the Scheme REPL to run a fasl file generated
  by `fol->common-lisp`.


Writing FOL
===========

A FOL program is a Scheme data structure.  This may have been parsed
from a file or constructed directly in memory, as appropriate for the
application.

FOL follows the following grammar:

```
program    = <expression>
           | (begin <definition> ... <expression>)

definition = <def-type>
           | <def-proc>

def-type   = (define-type <type-var> <named-type>)

def-proc   = (define (<proc-var> <data-var> ...)
               (argument-types <shape> ... <return-shape>)
               <expression>)

named-type = (structure (<field-name> <shape>) ...)

return-shape = <shape> | (values <shape> ...)

shape      = real | bool | gensym | () | <type-var>
           | (cons <shape> <shape>) | (vector <shape> ...)
           | escaping-function ; for escaping functions only

expression = <data-var>
           | <number> | <boolean> | ()
           | (if <expression> <expression> <expression>)
           | (let ((<data-var> <expression>) ...) <expression>)
           | (let-values (((<data-var> <data-var> <data-var> ...) <expression>))
               <expression>)
           | (lambda (<data-var>) <expression>)  ; for escape only
           | <access>
           | <construction>
           | (values <expression> <expression> <expression> ...)
           | (<proc-var> <expression> ...)

access = (car <expression>)
       | (cdr <expression>)
       | (vector-ref <expression> <integer>)
       ;; Also accessor procedures implied by DEFINE-TYPE
       | (<proc-var> <expression>)

construction = (cons <expression> <expression>)
             | (vector <expression> ...)
             ;; Also constructor procedures implied by DEFINE-TYPE
             | (<proc-var> <expression> ...)
```

FOL has three namespaces for variables: one for types, one for
procedures, and one for data; all variables are represented as Scheme
symbols.  The type variables and the procedure variables have global
scope and must be globally unique.  The type variables may only be
bound by `define-type` forms and may not shadow primitive types or type
constructors.  The procedure variables may only be bound by `define`
forms, and may not shadow primitive procedures or procedures implied
by type declarations.  The data variables are lexically scoped with
shadowing, and may be bound by `let` forms, `lambda` forms, and the formal
parameter positions of `define` forms.  Types and procedures may only be
defined at the top level.  The `lambda` form is provided only for
wrapping FOL procedures for export across the foreign interface.
Applications may only apply procedures directly by name.  This serves
to make FOL a first-order language.

FOL being first-order was the point.  The restrictions on FOL make the
targets of all control transfers (except procedure returns) statically
apparent.  The purpose of hairy flow analysis is to transform a
computation concisely expressed in a higher-order language, possibly
with AD operators, into an equivalent computation whose control
structure is as clear as possible.  The job of the FOL optimizer is to
give the flow analysis freedom to generate an arbitrarily ugly FOL
program; on the other hand the FOL optimizer can use the restrictions
on FOL to optimize it using standard, well-understood techniques.

FOL procedure definitions must be annotated with the types of the
arguments expected and the type of the result.  The annotation is
achieved via the `argument-types` form in the grammar.  The shapes given
in the form are taken to be parallel to the formal parameters, except
that the last one refers to the type returned by the procedure.  The
type declarations are used to perform scalar replacement of
aggregates, and also serve as a useful sanity check when debugging the
FOL optimizer or the code generator that gives it input.

The FOL type system does not at present admit union types or recursive
types.  This will be changed when FOL is extended to accept the
results of compiliing non-union-free VL or DVL programs.

FOL supports multiple value returns from procedures and expressions
via the standard Scheme `values` construct, and multiple value binding
via the SRFI-11 `let-values` construct.  All `values` expressions must be
in tail position with respect to a matching `let-values` expression.
You will note that because multiple value returns via the `values`
construct cannot be stored in data structures or bound as variables
without destructuring them, they cannot be copied or aliased, and are
therefore much easier for the optimizer to reason about than general
aggregates.  They also do not need to be represented in the runtime as
heap-allocated objects, but can instead be implemented by just placing
the values to be returned on the stack.  The VL and DVL code generator
does not emit `values` or `let-values` expressions; the FOL optimizer
generates them as replacements for returning general aggregates where
possible, as determined by the scalar replacement of aggregates
optimization.

For implementation convenience, `let-values` is restricted to binding
from a single expression; no parallel form is provided.  Also, nullary
and unary multiple value returns are not permitted in FOL because they
are easy to get rid of and would not interact as well as one would
like with MIT Scheme's expression return semantics.

FOL reserves the tokens that appear in the grammar as names to which
procedures and formal parameters may not be bound.  The set of FOL
reserved words is:

```
BEGIN, DEFINE-TYPE, DEFINE, IF, LET, LET-VALUES, VALUES, CONS, CAR,
CDR, VECTOR, and VECTOR-REF.
```

These all behave as in Scheme, except `define-type`, which is a typed
variant of Scheme's `define-structure`.  In addition, FOL predefines the
following procedures:

```
ABS, EXP, LOG, SIN, COS, TAN, ASIN, ACOS, SQRT, +, -, *, /, ATAN,
EXPT, <, <=, >, >=, =, ZERO?, POSITIVE?, NEGATIVE?, READ-REAL,
WRITE-REAL, REAL, GENSYM, GENSYM=, and GENSYM<.
```

whose semantics are given in [Reading FOL](#reading-fol).

FOL is specified to be tail-recursive and memory-managed.  Order of
evaluation of procedure arguments is unspecified.  As of the present
writing, the compilation may fail to preserve the order (or even
presence!) of side-effects.  FOL is notionally strict, but the
compilation may rearrange the order of evaluation of various
subexpressions, even across procedure boundaries.


Implementing FOL
================

The implementation of FOL is, of course, the matter at hand in the
rest of this program.  Besides the present `README`, there are some more
specific documents in the `doc/` directory, and, of course, the
extensively commented source.  Here is the table of contents:

Basic documents

- `architecture.txt`    Architecture overview
- `cost-model.txt`      About the assumed cost model of FOL execution
- `intermediate-representation.txt`  FOL compared to other representations

Top level

- `optimize.scm`        Toplevel
- `stages.scm`          Stage management system
  - `stages.txt`        Architecture and user documentation thereof

Main stages

- `inline.scm`          Inliner
  - `feedback-vertex-set.scm`  Graph algorithm supporting inlining
  - `inlinees.scm`      Other graph algorithm supporting inlining
- `cse.scm`             Common subexpression elimination
  - `simplification.txt`  Issues with simplifying algebra in a compiler
- `dead-code.scm`       Dead variable elimination
- `dead-types.scm`      Dead type elimination
- `sra.scm`             Scalar replacement of aggregates

Supporting stages

- `type-check.scm`      Type checker
- `structs.scm`         structure-definitions->vectors
- `alpha-remaing.scm`   Uniquifying bound names
- `a-normal-form.scm`   Conversion to and from A-normal form
- `lift-lets.scm`       Lifting lets

Other support

- `nomenclature.scm`    Representation of FOL names
- `syntax.scm`          Low-level support; FOL syntax
- `srfi-11.scm`         Reference implementation of SRFI 11
- `runtime.scm`         FOL runtime system
- `primitives.scm`      Primitives available to FOL programs
- `mit-scheme.scm`      Compilation to machine code with MIT Scheme
- `stalin.scm`          Compilation to machine code with Stalin
- `common-lisp.scm`     Compilation with Steel Bank Common Lisp
- `load.scm`            Entry point; namespace management

The FOL compiler makes extensive use of a pattern matching and
replacement system.  It's pretty intuitive, but a brief additional
description can be found in `doc/rule-system.txt`.


Future of FOL
=============

There are four major improvements needed for the extant FOL system:

1.  FOL will need to accept non-union-free programs.  At this moment,
    neither VL nor DVL compile non-union-free programs either, but in time
    this defect will need to be corrected; and FOL will need to go first.
    Doing this will involve a reworking of the FOL type system and an
    updating of the SRA algorithm, but should not require any other major
    changes.

2.  FOL will need to preserve the presence and order of side-effects,
    without compromising the optimizations available to pure code.
    Perhaps adding the IO monad to the type system?

3.  FOL will need a foreign interface, at least to the host language
    (which then has a foreign interface to the rest of the world).
    This is how VL and DVL can be given foreign interfaces.  There already
    is a rudimentary one in the form of returning a value (which may be a
    `lambda` object, representing a procedure that accepts one real number
    argument), but this will need to be expanded upon.

4.  FOL will need to run on different platforms.  Even MIT Scheme
    native code at the moment boxes its floating point numbers, which
    seems to cost about 5x or 10x; also, MIT Scheme is not exactly a
    paragon of interoperation with other things.  Candidate platforms
    include:
    - Integrating FOL with Racket.
    - Treating FOL as Common Lisp and compiling with, e.g., SBCL.
    - Compiling to and running on the JVM or on .NET.
    - Compiling FOL to/with LLVM.
    - Translating FOL to C (or C--) and compiling with, e.g., gcc.

    See also `doc/backends.txt`.  This process will be undertaken as VL,
    DVL, and FOL scale to examples that are too slow to interpret, or too
    big to implement directly in the VL or DVL source languages without
    foreign libraries.

Finally, the current state of FOL is also open to some improvements
that don't affect its interface, for instance implementing
interprocedural common subexpression elimination.
