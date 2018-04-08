
# Builtin Bindings

## Operators

Operators work as both compile-time specials and run-time functions.
When the compiler sees the appropriate symbol at the head of an
invocation form, it will compile the operation to the appropriate
bytecode. When passed as a parameter, the operator becomes a callable
with similar behavior.

For example:

This will compile to the bytecode addition op
```
(if (== action 'up)
  then: (+ value 5)
  else: (- value 5))
```

Where-as this will result in a function call
```
((if (== action 'up) + -) value 5)
```

### List of Operators

```
% modulo
& bitwise-and
* multiply
** power
+ add
- subtract
/ divide
// floor-divide
@ matrix-multiply
^ bitwise-xor
| bitwise-or
~
<< shift-left
>> shift-right
or
and
not
raise
item
set-item
del-item
iter
```


## Comparators

Comparators are a type of operator, and behave in much the same way,
but are always binary (taking two arguments).

As with the rest of the operators, they can be used either as a
compile-time or run-time feature.

### List of Comparators

```
< lt
<= le
== eq
!= not-eq
> gt
>= gt
is
is-not
in
not-in
```


## Sibilant Types

```
cons
pair?
proper?
copy-pair
car
cdr
set-car
set-cdr
nil
nil?
symbol
symbol?
keyword
keyword?
build-unpack-pair
build-proper
join-pairs
setf
values
```


## Parse-Time Features

```
set-atom-pattern
set-atom-regex
set-event-macro
set-macro-character
temp-event-macro
temp-macro-character
get-atom-pattern
get-event-macro
clear-atom-pattern
clear-event-macro
```


## Compile-Time Features

```
defmacro
defmacrolet
gv-define-setter
gv-define-setter-fn
gv-define-simple-setter
macroexpand-1
special
special?
macro
macro?
operator
operator?
macrolet
macrolet?
trampoline
tailcall
tco-disable
```


## Python Types and Functions

```
True
False
None
...
none?
bool
bool?
int
int?
float
float?
fraction
fraction?
complex
complex?
str
str?
list
list?
tuple
tuple?
set
set?
dict
dict?
range
range?
partial
open
import
object
print
isinstance
build-dict
build-list
build-set
build-tuple
locals
globals
slice
slice?
repr
getattr
setattr
format
type
type?
dir
doc
help
next
exit

```


## Enhanced Python Functions

```
callable?
iterable?
to-list
to-set
to-tuple
len
enumerate
map
filter
reduce
zip

```

## Utility

```
apply
count
incr
decr
last

```

## Defining Vars

```
def
define var
defimport
defimportfrom
define-global
defun
defclass

```

## Special Forms

```
begin
cond
try
lambda
let
function
quote
quasiquote
while
with
setq
attr
set-attr
define

```


## Flow Control Macros

```
if
unless
when

```


## Exceptions

```
ArithmeticError
AssertionError
AttributeError
BaseException
BlockingIOError
BrokenPipeError
BufferError
BytesWarning
ChildProcessError
ConnectionAbortedError
ConnectionError
ConnectionRefusedError
ConnectionResetError
DeprecationWarning
EOFError
EnvironmentError
Exception
FileExistsError
FileNotFoundError
FloatingPointError
FutureWarning
GeneratorExit
IOError
ImportError
ImportWarning
IndentationError
IndexError
InterruptedError
IsADirectoryError
KeyError
KeyboardInterrupt
LookupError
MemoryError
NameError
NotADirectoryError
NotImplementedError
OSError
OverflowError
PendingDeprecationWarning
PermissionError
ProcessLookupError
RecursionError
ReferenceError
ResourceWarning
RuntimeError
RuntimeWarning
StopAsyncIteration
StopIteration
SyntaxError
SyntaxWarning
SystemError
SystemExit
TabError
TimeoutError
TypeError
UnboundLocalError
UnicodeDecodeError
UnicodeEncodeError
UnicodeError
UnicodeTranslateError
UnicodeWarning
UserWarning
ValueError
Warning
ZeroDivisionError
```


## Unsorted


```

__all__
__builtins__
__cached__
__doc__
__file__
__loader__
__name__
__package__
__spec__
_compiler
_converters
_fractions
_functools
_op
_operator
_operators
_sibilant
_specials
_sys
_tco
_val


class
eighth
fifth
first
fourth
function?
global
key
local
method
ninth
read
second
seventh
sixth
tenth
third
val
```
