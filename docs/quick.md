
# Quick Introduction to Sibilant

Sibilant is a LISP that compiles to Python bytecode.

A sibilant module is a Python module. Sibilant can import and call
Python functions, and vise verse.

Sibilant provides LISP data structure types, like symbol, keyword,
pair, and nil. These are used to represent the sibilant syntax, and
are also useful at runtime.


## Parse, Compile, Eval

The loading of a Sibilant module is performed incrementally. Before
loading can begin, an empty module object must be prepared to hold the
state and the eventual final bindings for the module. This module is
provided with a collection of functions, values, special forms, and
macros. Some are provided by Sibilant, others are directly from
Python. A parser, compiler, and evaluator are assigned to the module,
then the loading can begin.

Each top-level expression goes through parse-time, compile-time, and
finally run-time. When all expressions in the module source have been
processed, the loading is complete, and the module is ready to be
used.


### Parse

During parse-time, the module's parser will read a single expression
from the module's source. The parser is extensible, so behavior can be
added or adjusted from one expression to the next.


### Compile

The resulting expression is then compiled, using the compiler instance
specific to that module. The compiler will attempt to produce a code
object based on the expression and its sub-expressions. The compiler
will look for specials and macros in the module itself, and in the
module's builtins. This means that it is possible for a module to
define or import a macro in one top-level expression, and then have
that macro be available for subsequent expressions as they are
compiled.

Sibilant specials are expanded at compile time. When used as a
run-time function, they will raise an exception. A special takes a
source expression and injects bytecode operands into the compiler.

Sibilant operators are expanded at compile time, but also have a
run-time implementation. This is to take advantage of the numerous
opcodes that Python has, sparing unnecessary function lookups and
invocations.

Sibilant macros are expanded at compile time. When used as a run-time
function, they will raise an exception. A macro takes a source
expression and returns the expression which should be compiled in its
place. This expansion happens at compile time, but has full access to
the module contents that have been evaluated prior.

Sibilant aliases are a form of macro which is invoked from a single
symbol, instead of a pair expression. Aliases do not accept arguments
when they are expanded, but they have full access to the module
contents that have been evaluated prior.

Specials, operators, macros, and aliases may all be defined at
runtime. They are all importable and can be bound to variables inside
of a module scope. The compiler finds these forms by the name that
they are bound to in the module.

Finally, the expression is evaluated. This allows it to define values
inside of the module (such as functions, or type definitions).


### Loaded

Once all expressions have been read, compiled, and evaluated, the
module is completely loaded. If loading had been invoked via the
importlib extension, then the module will now be available for other
Sibilant or Python modules to reference.


## Variable Bindings

Sibilant has significantly less restrictions in variable naming
conventions. For example, it's perfectly acceptable for a Sibilant
predicate testing whether a value was `nil` to be named `nil?`
