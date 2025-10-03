# Lisp primitives

 This document outlines the family groups of related low-level Lisp functions in
 ChrysaLisp, providing descriptions for each.

 These are the built-in VP Lisp primitives. Refer to the `root.inc` file for
 further Lisp level functions built upon these.

## Core Lisp Primitives

These are the fundamental building blocks of the Lisp language and evaluation
model, defined via FFI calls to the core Lisp class.

*   **`ffi`**: The Foreign Function Interface, used to bind a Lisp symbol to an
    underlying VP machine code implementation. This is the foundation for
    creating all other primitive functions.

*   **`lambda`**: Creates an anonymous function.

*   **`macro`**: Creates a macro, a function that transforms code at compile
    time.

*   **`quote`**: Prevents the evaluation of a form, returning the form itself.

*   **`quasi-quote`**: Similar to `quote`, but allows for selective evaluation
    of sub-expressions within the form using `unquote` (`,`) and
    `unquote-splicing` (`~`).

## Conditional and Control Flow Functions

This group of functions and macros provides the fundamental tools for
controlling the flow of execution based on conditions.

*   **`if` / `ifn`**: The basic conditionals.

    * `(if tst form [else_form])`: Evaluates `tst`. If the result is not `:nil`,
      it evaluates and returns `form`. Otherwise, it evaluates and returns
      `else_form` or `:nil`.

    * `(ifn tst form [else_form])`: The inverse of `if`. It evaluates `form` if
      `tst` is `:nil`.

*   **`cond` / `condn`**: Handling multiple conditions, similar to a switch-case
    statement.

    * `(cond [(tst body)] ...)`: Evaluates a series of clauses. For the first
      clause where `tst` evaluates to non `:nil`, it executes the corresponding
      `body` and returns the result.

    * `(condn [(tst body)] ...)`: The inverse of `cond`. It executes the body
      for the first clause where `tst` evaluates to `:nil`.

*   **`when` / `unless`**: Shorthand for simple `if` statements with a single
    consequent form.

    * `(when tst body)`: If `tst` is not `:nil`, executes the `body`.

    * `(unless tst body)`: If `tst` is `:nil`, executes the `body`.

*   **`while` / `until`**: Looping constructs.

    * `(while tst [body])`: Executes the `body` repeatedly as long as `tst`
      evaluates to a non `:nil` value.

    * `(until tst [body])`: Executes the `body` repeatedly until `tst` evaluates
      to a non `:nil` value.

*   **`case`**: A multi-way branching macro that compares a key expression
    against several literal values.

    * `(case key [(val body)] ...)`: Evaluates `key` and compares it to the
      `val` in each clause. It executes the `body` of the first matching clause.

## Sequence Manipulation and Slicing Functions

These functions provide the core utilities for accessing, slicing, and combining
sequences like lists, arrays, and strings.

*   **`length`**: Returns the number of elements in a sequence.

    * `(length seq) -> num`

*   **`elem-get`**: Retrieves the element at a specific zero-based index.

    * `(elem-get seq idx) -> elem`

*   **`first`**, **`second`**, **`third`**, **`last`**: Access specific elements
    of a sequence.

    * `(first seq) -> :nil | elem`: Returns the first element.

    * `(second seq) -> :nil | elem`: Returns the second element.

    * `(third seq) -> :nil | elem`: Returns the third element.

    * `(last seq) -> :nil | elem`: Returns the last element.

*   **`rest`**, **`most`**: Return a new sequence with elements removed from the
    beginning or end.

    * `(rest seq) -> empty | seq`: Returns the sequence without its first
      element.

    * `(most seq) -> empty | seq`: Returns the sequence without its last
      element.

*   **`slice`**: Extracts a subsequence.

    * `(slice seq start end) -> seq`: Returns a new sequence containing elements
      from `start` up to (but not including) `end`.

*   **`partition`**: Divides a sequence into a list of smaller sequences.

    * `(partition seq [cnt]) -> (seq ...)`: Groups elements of `seq` into
        sub-sequences of size `cnt`.

*   **`cat`**: Concatenates multiple sequences into one.

    * `(cat seq ...) -> seq`

## Sequence Searching and Matching Functions

These functions are used to find elements or subsequences within a sequence, with specialized functions for character classes.

*   **`find` / `rfind`**: Perform a linear search for an element in a sequence.

    * `(find elem seq [idx]) -> :nil | idx`: Searches forward from the beginning
        (or an optional start `idx`) and returns the index of the first match.

    * `(rfind elem seq [idx]) -> :nil | idx`: Searches backward from the end (or
        an optional start `idx`).

*   **`bfind`**: Performs a fast binary search to check if a character exists
    within a sorted character class string.

    * `(bfind char cls) -> :nil | idx`: Returns the index if the character is
      found, otherwise `:nil`.

*   **`bskip`**: Moves an index forward in a string as long as the characters
    are present in the given character class.

    * `(bskip cls str idx) -> idx`: Returns the new index after skipping all
      matching characters.

*   **`bskipn`**: Moves an index forward in a string as long as the characters
    are *not* present in the given character class.

    * `(bskipn cls str idx) -> idx`: The inverse of `bskip`.

## Sequence Operation Functions (Iterators)

ChrysaLisp's design heavily favors iteration over recursion. The `...!`
functions are the core iterative primitives for operating on sequences like
lists and arrays.

*   **`each!`**: Iterates over one or more sequences, applying a lambda function
    to the elements at each index.

    * `(each! lambda seqs [start end])`: Applies the `lambda` to corresponding
      elements from each sequence in `seqs`.

*   **`some!`**: Similar to `each!`, but stops and returns the first non `:nil`
    value returned by the lambda.

    * `(some! lambda seqs [mode start end]) -> :nil | val`: The `mode` argument
      can alter the termination condition.

*   **`map!`**: Applies a lambda to elements from sequences and collects the
    results into a new list.

    * `(map! lambda seqs [out start end]) -> out | (...)`: If an `out` list is
      provided, results are pushed into it; otherwise, a new list is created.

*   **`reduce!`**: Accumulates a single value by repeatedly applying a lambda to
    an accumulator and elements from sequences.

    * `(reduce! lambda seqs init [start end]) -> val`: The `init` value is the
      starting point for the reduction.

*   **`filter!`**: Creates a new list containing only the elements from a
    sequence for which the lambda returns a non `:nil` value.

    * `(filter! lambda seq [out start end]) -> out | (...)`

*   **`lines!`**: Iterates over each line in a stream, applying a lambda.

    * `(lines! lambda stream) -> :nil`

*   **`!`**: A special anaphoric symbol available inside `...!` loops that
    returns the current iteration index.

    * `(each! (# (print (!))) my-list)`

## Array and List Specific Functions

These functions are specialized for mutable `array` and `list` data structures.

*   **`cap`**: Sets the initial capacity of an array or list.

    * `(cap len array ...) -> array`

*   **`clear`**: Removes all elements from an array or list.

    * `(clear array ...) -> array`

*   **`push`**: Adds one or more elements to the end of an array or list.

    * `(push array elem ...) -> array`

*   **`pop`**: Removes and returns the last element from an array or list.

    * `(pop array) -> elem | :nil` Returns `:nil` if empty.

*   **`elem-set`**: Replaces the element at a specific index.

    * `(elem-set array idx elem) -> array`

*   **`merge-obj`**: Merges the elements of a source list into a destination
    list, ensuring uniqueness.

    * `(merge-obj dlist slist) -> dlist`

*   **`pivot`**: Partitions a list around a pivot element for sorting
    algorithms.

    * `(pivot lambda list start end)`

*   **`lmatch?`**: Compares two lists for structural and content equality.

    * `(lmatch? list list) -> :nil | :t`

*   **`copy`**: Creates a deep copy of a form.

    * `(copy form) -> 'form`

## Numeric and Arithmetic Functions

This family contains functions for comparison, arithmetic, bitwise operations,
and type conversion.

### Comparison Operators

*   `=` / `/=` / `<` / `>` / `<=` / `>=`: Standard numeric comparison operators.

    * `(= num num ...) -> :t | :nil`

### Arithmetic Operators

*   **`+` / `-` / `*` / `/` / `%`**: Standard arithmetic operations.

    * `(+ num num ...) -> num`

*   **`min` / `max`**: Return the minimum or maximum value from a sequence of
    numbers.

    * `(min num num ...) -> num`

*   **`neg` / `abs`**: Negate a number or return its absolute value.

    * `(neg num) -> num`

    * `(abs num) -> num`

### Bitwise and Logical Operators

*   **`>>` / `>>>` / `<<`**: Bitwise right shift, arithmetic right shift, and
    left shift.

    * `(>> num cnt) -> num`

*   **`logand` / `logior` / `logxor`**: Bitwise AND, OR, and XOR operations.

    * `(logand [num] ...) -> num`

### Type Conversion

*   **`n2i` / `n2f` / `n2r`**: Convert a generic number to an integer,
    fixed-point, or real number.

    * `(n2i num) -> num`

    * `(n2f num) -> fixed`

    * `(n2r num) -> real`

### Other Numeric Functions

*   **`random`**: Generates a random number within a specified range.

    * `(random num) -> num`

*   **`num-intern`**: Interns a number, ensuring that identical numerical values
    share the same object representation.

    * `(num-intern num) -> num`

## String and Character Functions

*   **`str`**: Converts any form into its string representation.

    * `(str form) -> str`

*   **`str-to-num`**: Parses a string and converts it to a number.

    * `(str-to-num str) -> num`

*   **`char`**: Creates a string from one or more numeric character codes.

    * `(char num [width]) -> str`

*   **`code`**: Returns the numeric code of a character in a string.

    * `(code str [width index]) -> num`

*   **`expand`**: Replaces tab characters in a string with spaces.

    * `(expand str tab_width) -> str`

*   **`cmp`**: Compares two strings lexicographically.

    * `(cmp str str) -> + | 0 | -`

*   **`save` / `load`**: Save a string to a file or load a file's content into a
    string.

    * `(save str path) -> str`

    * `(load path) -> str`

*   **`str-alloc`**: Allocates an empty string of a specified size. Mainly used
    for message creation.

    * `(str-alloc size) -> str`

## Symbol Functions

*   **`sym`**: Interns a string, returning the corresponding unique symbol.

    * `(sym str) -> sym`

*   **`gensym`**: Generates a new, unique interned symbol.

    * `(gensym) -> sym`

## Environment and Variable Binding Functions

These functions manage variables and their values within lexical scopes
(environments).

*   **`defq` / `setq`**: Define or set variables in the *current* environment.

    * `(defq sym val [sym val] ...)`: Binds one or more symbols (`sym`) to their
      corresponding values (`val`) in the current lexical scope.

    * `(setq sym val [sym val] ...)`: Re-assigns new values to existing symbols
      in the current or parent scopes.

*   **`env`**: Creates a new, empty environment or returns the current one.

    * `(env [num]) -> env`: If `num` (an integer) is provided, it creates a new,
      empty environment (an `hmap`), typically with `num` hash buckets for
      performance tuning. If no argument is given, it returns the current
      lexical environment.

*   **`def` / `set`**: Define or set variables in a *specified* environment.

    * `(def env sym val [sym val] ...)`: Binds symbols in the specified
      environment `env`.

    * `(set env sym val [sym val] ...)`: Re-assigns values in the specified
      environment `env`.

*   **`bind`**: Destructures a sequence, binding its elements to a list of
    symbols.

    * `(bind (sym ...) seq)`

*   **`let` / `let*`**: Create a new lexical scope and bind variables within it.

    * `(let ([(sym val) ...]) body)`: Binds all `val` expressions first, then
      creates the new scope with the `sym` bindings.

    * `(let* ([(sym val) ...]) body)`: Binds each `(sym val)` pair sequentially,
      allowing later bindings to refer to earlier ones.

*   **`get` / `def?`**: Look up a symbol's value in the environment chain.

    * `(get sym [env]) -> :nil | val`: Searches for `sym` starting from the
      specified `env` (or the current one) and traversing up the parent chain.

    * `(def? sym [env]) -> :nil | val`: Similar to `get`, but specifically
      checks for a definition within the `hmap` itself, without traversing.

*   **`undef`**: Removes a symbol binding from a specified environment.

    * `(undef env sym [sym] ...)`

*   **`tolist`**: Converts an environment into a list of key-value pairs.

    * `(tolist env) -> ((sym val) ...)`

## Environment Tree (Scoping) Functions

These functions manage the hierarchy of environments that form ChrysaLisp's
lexical scopes.

*   **`env-push`**: Creates a new, empty environment whose parent is the current
    environment.

    * `(env-push [env]) -> 'env`: Returns the new child environment.

*   **`env-pop`**: Discards the current environment and returns to its parent.

    * `(env-pop [env]) -> 'env`: Returns the parent environment.

*   **`penv`**: Retrieves the parent of the current or a specified environment.

    * `(penv [env]) -> :nil | env`

*   **`env-resize`**: A performance-tuning function to change the number of
    buckets in an environment's underlying hash map.

    * `(env-resize num [env]) -> env`

## Data Structure Constructors

*   **`array`**: Creates an array.

    * `(array [num ...]) -> array`

*   **`list`**: Creates a list.

    * `(list [elem ...]) -> list`

*   **`nums`**: Creates a specialized vector of numbers.

    * `(nums [num ...]) -> nums`

*   **`fixeds`**: Creates a specialized vector of fixed-point numbers.

    * `(fixeds [fixed ...]) -> fixeds`

*   **`reals`**: Creates a specialized vector of real numbers.

    * `(reals [real ...]) -> reals`

*   **`path`**: Creates a path object, which is a sequence of fixed-point
    coordinates.

    * `(path [fixed ...]) -> path`

*   **`dim`**: Creates a multi-dimensional array.

    * `(dim nums array) -> dim`

*   **`dim-get` / `dim-set`**: Access or modify elements in a multi-dimensional
    array.

    * `(dim-get dim nums) -> elem`

    * `(dim-set dim nums elem) -> array`

## Stream I/O Functions

Streams provide a unified interface for handling input and output from files,
strings, or standard I/O channels.

*   **`io-stream`**: Gets a handle to a standard I/O stream.

    * `(io-stream io)`: `io` can be `"stdin"`, `"stdout"`, or `"stderr"`.

*   **`string-stream`**: Creates a stream that reads from or writes to a string.

    * `(string-stream str) -> stream`

*   **`file-stream`**: Opens a file and returns a stream for it.

    * `(file-stream path [mode]) -> :nil | stream`: `mode` can be
        `+file_open_read`, `+file_open_write`, etc.

*   **`read-char`**: Reads a single character (as a num) from a stream.

    * `(read-char stream [width]) ->  :nil | num`

*   **`read-line`**: Reads a line of text from a stream.

    * `(read-line stream) -> :nil | str`

*   **`read-avail`**: Returns all available buffered data as a string.

    * `(read-avail stream) -> :nil | str`

*   **`write-char`**: Writes a single character or a list of character to a
    stream.

    * `(write-char stream list|num [width]) -> bytes`

*   **`write`**: Writes a string to a stream.

    * `(write stream str) -> bytes`

*   **`stream-flush`**: Flushes any buffered output for a stream.

    * `(stream-flush stream) -> stream`

*   **`prin`**: Prints the string representation of its arguments to standard
    output without a trailing newline.

    * `(prin [form] ...)`

*   **`print`**: Prints the string representation of its arguments to standard
    output, followed by a newline character.

    * `(print [form] ...)`

## Evaluation and Metaprogramming Functions

These are the core tools for defining new functions, macros, and controlling
evaluation.

*   **`defun` / `redefun`**: Defines a new function.

    * `(defun name ([arg ...]) body)`: Binds `name` to a new lambda function.

    * `(redefun name ([arg ...]) body)`: Re-defines an existing function.

*   **`defmacro` / `redefmacro`**: Defines a new macro.

    * `(defmacro name ([arg ...]) body)`: Binds `name` to a new macro.

    * `(redefmacro name ([arg ...]) body)`: Re-defines an existing macro.

*   **`eval`**: Evaluates a Lisp form.

    * `(eval form [env]) -> 'form`: Executes the form within the specified (or
        current) environment.

*   **`eval-list`**: Evaluates each element in a list.

    * `(eval-list list [env]) -> list`

*   **`apply`**: Calls a function with a list of arguments.

    * `(apply lambda seq) -> form`

*   **`macroexpand`**: Expands a macro call, showing what code it generates
    without executing it.

    * `(macroexpand form) -> 'form`

*   **`prebind`**: A performance optimization that resolves symbol lookups in a
    form at compile-time rather than runtime.

    * `(prebind form) -> form`

*   **`read`**: Reads a single S-expression from a stream.

    * `(read stream [last_char]) ->  :nil | (form next_char)`

*   **`repl`**: Starts a Read-Eval-Print Loop on a given stream.

    * `(repl stream name)`

*   **`repl-info`**: Returns the name and current line number of the file being
    processed by the REPL.

    * `(repl-info) -> (name line)`

*   **`progn`**: Evaluates a sequence of forms and returns the value of the last
    one.

    * `(progn [body]) -> 'form`

*   **`catch` / `throw`**: A non-local exit mechanism.

    * `(catch form eform)`: Evaluates `form`. If a `throw` occurs during its
        execution, `eform` is evaluated.

    * `(throw str form)`: Unwinds the stack until a matching `catch` is found.

## Object and Class System Functions

*   **`.`**: The primary mechanism for method dispatch.

    * `(. env sym [...]) -> form`: Calls the method `sym` on the object `env`
      with optional arguments.

*   **`type-of`**: Returns the inheritance chain of an object as a list of
    symbols.

    * `(type-of form) -> (... :obj)`

*   **`hash`**: Computes the hash value of a form.

    * `(hash form) -> num`

*   **`eql`**: Checks if two forms are identical.

    * `(eql form form) -> :nil | :t`

*   **`identity`**: Returns its argument unchanged.

    * `(identity [form]) -> :nil | form`

## System and Platform Interface (PII) Functions

These functions interact directly with the underlying host operating system.

*   **`pii-dirlist`**: Lists the contents of a directory.

    * `(pii-dirlist path) -> info`

*   **`pii-fstat`**: Retrieves file status information (modification time,
    size).

    * `(pii-fstat path) -> info`

*   **`pii-read-char` / `pii-write-char`**: Read or write a single character
    from/to a file descriptor.

    * `(pii-read-char fd) -> char`

    * `(pii-write-char fd char) -> char`

*   **`pii-remove`**: Deletes a file.

    * `(pii-remove path)`

*   **`pii-time`**: Returns the current system time in nanoseconds.

    * `(pii-time) -> ns`

## Low-Level Object Manipulation

These functions provide direct, unsafe access to the underlying memory
representation of objects.

*   **`get-field`**: Reads a value from an object at a specific memory offset.

    * `(get-field obj field size|0) -> val`

*   **`set-field`**: Writes a value to an object at a specific memory offset.

    * `(set-field obj field size|0 val) -> val`

*   **`weak-ref`**: Returns the memory address of a Lisp object as a number.

    * `(weak-ref form) -> num`

*   **`obj-ref`**: Converts a numeric memory address back into a Lisp object
    reference.

    * `(obj-ref num) -> form`
