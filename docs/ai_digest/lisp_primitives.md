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

*   **`inc`**: Increments a number by 1.

    * `(inc num) -> num`

*   **`dec`**: Decrements a number by 1.

    * `(dec num) -> num`

*   **`++`**: Increments a number by a given value (default 1).

    * `(++ num [num]) -> num`

*   **`--`**: Decrements a number by a given value (default 1).

    * `(-- num [num]) -> num`

*   **`not`**: Returns `:t` if the form is `:nil`, otherwise `:nil`.

    * `(not form) -> :t | :nil`

*   **`or`**: Evaluates forms from left to right, returning the first non-`:nil` value.

    * `(or [tst] ...) -> :nil | tst`

*   **`and`**: Evaluates forms from left to right, returning the last value if all are non-`:nil`.

    * `(and [tst] ...) -> :t | :nil | tst`

*   **`times`**: Executes a body of code a specified number of times.

    * `(times num body)`

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

*   **`sort`**: Sorts a list.

    * `(sort list [fcmp start end]) -> list`

*   **`swap`**: Swaps two elements in a list.

    * `(swap list idx idx)`

*   **`shuffle`**: Shuffles a list.

    * `(shuffle list [start end]) -> list`

*   **`range`**: Creates a list of numbers within a specified range.

    * `(range start end [step]) -> list`

*   **`each-mergeable`**: Applies a lambda to each element of a sequence.

    * `(each-mergeable lambda seq) -> seq`

*   **`each`**: Applies a lambda to each element of one or more sequences.

    * `(each lambda seq ...)`

*   **`reach`**: Applies a lambda to each element of one or more sequences in reverse order.

    * `(reach lambda seq ...)`

*   **`map`**: Applies a lambda to each element of one or more sequences and returns a new list with the results.

    * `(map lambda seq ...) -> list`

*   **`rmap`**: Applies a lambda to each element of one or more sequences in reverse order and returns a new list with the results.

    * `(rmap lambda seq ...) -> list`

*   **`reduce`**: Reduces a sequence to a single value using a lambda.

    * `(reduce lambda seq [init]) -> form`

*   **`rreduce`**: Reduces a sequence to a single value using a lambda in reverse order.

    * `(rreduce lambda seq [init]) -> form`

*   **`filter`**: Filters a sequence using a lambda.

    * `(filter lambda seq) -> list`

*   **`reverse`**: Reverses a sequence.

    * `(reverse seq) -> seq`

*   **`lists`**: Creates a list of lists.

    * `(lists n) -> ((list0) ... (listn-1))`

*   **`starts-with`**: Checks if a string starts with a given prefix.

    * `(starts-with str str) -> :t | :nil`

*   **`ends-with`**: Checks if a string ends with a given suffix.

    * `(ends-with str str) -> :t | :nil`

*   **`erase`**: Erases a part of a sequence.

    * `(erase seq start end) -> seq`

*   **`insert`**: Inserts a sequence into another sequence.

    * `(insert seq pos seq) -> seq`

*   **`replace`**: Replaces a part of a sequence with another sequence.

    * `(replace seq start end seq) -> seq`

*   **`rotate`**: Rotates a part of a sequence.

    * `(rotate seq start mid end) -> seq`

*   **`join`**: Joins a sequence of sequences with a separator.

    * `(join seqs seq [mode]) -> seq`

*   **`unzip`**: Unzips a sequence into a sequence of sequences.

    * `(unzip seq cnt) -> seqs`

*   **`zip`**: Zips multiple sequences into a single sequence.

    * `(zip seq ...) -> seq`

*   **`unique`**: Removes duplicate elements from a sequence.

    * `(unique seq) -> seq`

*   **`flatten`**: Flattens a nested list.

    * `(flatten list) -> list`

*   **`max-length`**: Returns the maximum length of a list of lists.

    * `(max-length list) -> max`

*   **`min-length`**: Returns the minimum length of a list of lists.

    * `(min-length list) -> min`

## Predicates

*   **`lambda?`**: Checks if a form is a lambda.

    * `(lambda? form) -> :t | :nil`

*   **`macro?`**: Checks if a form is a macro.

    * `(macro? form) -> :t | :nil`

*   **`quote?`**: Checks if a form is a quote.

    * `(quote? form) -> :t | :nil`

*   **`quasi-quote?`**: Checks if a form is a quasi-quote.

    * `(quasi-quote? form) -> :t | :nil`

*   **`array?`**: Checks if a form is an array.

    * `(array? form) -> :t | :nil`

*   **`list?`**: Checks if a form is a list.

    * `(list? form) -> :t | :nil`

*   **`num?`**: Checks if a form is a number.

    * `(num? form) -> :t | :nil`

*   **`fixed?`**: Checks if a form is a fixed-point number.

    * `(fixed? form) -> :t | :nil`

*   **`real?`**: Checks if a form is a real number.

    * `(real? form) -> :t | :nil`

*   **`nums?`**: Checks if a form is a numeric vector.

    * `(nums? form) -> :t | :nil`

*   **`fixeds?`**: Checks if a form is a fixed-point vector.

    * `(fixeds? form) -> :t | :nil`

*   **`reals?`**: Checks if a form is a real vector.

    * `(reals? form) -> :t | :nil`

*   **`func?`**: Checks if a form is a function.

    * `(func? form) -> :t | :nil`

*   **`str?`**: Checks if a form is a string.

    * `(str? form) -> :t | :nil`

*   **`sym?`**: Checks if a form is a symbol.

    * `(sym? form) -> :t | :nil`

*   **`env?`**: Checks if a form is an environment.

    * `(env? form) -> :t | :nil`

*   **`seq?`**: Checks if a form is a sequence.

    * `(seq? form) -> :t | :nil`

*   **`lambda-func?`**: Checks if a form is a lambda function.

    * `(lambda-func? form) -> :t | :nil`

*   **`macro-func?`**: Checks if a form is a macro function.

    * `(macro-func? form) -> :t | :nil`

*   **`nil?`**: Checks if a form is `:nil`.

    * `(nil? o) -> :t | :nil`

*   **`atom?`**: Checks if a form is an atom.

    * `(atom? o) -> :t | :nil`

*   **`empty?`**: Checks if a sequence is empty.

    * `(empty? form) -> :t | :nil`

*   **`nempty?`**: Checks if a sequence is not empty.

    * `(nempty? form) -> :t | :nil`

*   **`lisp-node?`**: Checks if a node is a Lisp node.

    * `(lisp_node? node) -> :t | :nil`

*   **`cpp-node?`**: Checks if a node is a C++ node.

    * `(cpp_node? node) -> :t | :nil`

*   **`neg?`**: Checks if a number is negative.

    * `(neg? num) -> :t | :nil`

*   **`pos?`**: Checks if a number is positive.

    * `(pos? num) -> :t | :nil`

*   **`odd?`**: Checks if a number is odd.

    * `(odd? num) -> :t | :nil`

*   **`even?`**: Checks if a number is even.

    * `(even? num) -> :t | :nil`

## Math Functions

*   **`lognot`**: Calculates the bitwise NOT of a number.

    * `(lognot num) -> num`

*   **`log2`**: Calculates the base-2 logarithm of a number.

    * `(log2 num) -> num`

*   **`pow`**: Calculates the power of a number.

    * `(pow base exponent) -> integer`

*   **`ntz`**: Counts the number of trailing zeros in a number.

    * `(ntz num) -> num`

*   **`nto`**: Counts the number of trailing ones in a number.

    * `(nto num) -> num`

*   **`nlz`**: Counts the number of leading zeros in a number.

    * `(nlz num) -> num`

*   **`nlo`**: Counts the number of leading ones in a number.

    * `(nlo num) -> num`

*   **`sqrt`**: Calculates the square root of a number.

    * `(sqrt num) -> num`

*   **`sign`**: Returns the sign of a number.

    * `(sign num) -> -1 | 0 | 1`

## Macros

*   **`macrobind`**: Prebinds and macroexpands a form.

    * `(macrobind form) -> (prebind (macroexpand form))`

*   **`exec`**: Evaluates a macrobinded form.

    * `(exec form)`

*   **`const`**: Evaluates a form at compile time.

    * `(const form)`

*   **`static-q`**: Statically quotes a form.

    * `(static-q form) -> 'form`

*   **`static-qq`**: Statically quasi-quotes a form.

    * `(static-qq form) -> `form`

*   **`static-qqp`**: Statically quasi-quotes a form, prebind only.

    * `(static-qqp form) -> `form`

*   **`callback`**: Creates a callback.

    * `(callback lambda env arg ...)`

*   **`debug-brk`**: Creates a debug breakpoint.

    * `(debug-brk brk_id [condition])`

*   **`profile-report`**: Generates a profile report.

    * `(profile-report name [reset])`

*   **`setd`**: Sets default values for symbols.

    * `(setd sym val [sym val] ...)`

*   **`#`**: A reader macro for creating lambdas.

    * `(# (< %9 %0 %3) ...)`

*   **`some`**: Applies a lambda to each element of a sequence, returning the first non-nil result.

    * `(some lambda seq ...)`

*   **`rsome`**: Applies a lambda to each element of a sequence in reverse, returning the first non-nil result.

    * `(rsome lambda seq ...)`

*   **`every`**: Applies a lambda to each element of a sequence, returning true if all results are non-nil.

    * `(every lambda seq ...)`

*   **`notany`**: Applies a lambda to each element of a sequence, returning true if all results are nil.

    * `(notany lambda seq ...)`

*   **`notevery`**: Applies a lambda to each element of a sequence, returning true if at least one result is nil.

    * `(notevery lambda seq ...)`

## Utilities

*   **`usort`**: Sorts a list and removes duplicate elements.

    * `(usort list [fcmp start end]) -> list`

*   **`export`**: Exports symbols to an environment.

    * `(export env symbols)`

*   **`export-symbols`**: Exports symbols to the parent environment.

    * `(export-symbols symbols)`

*   **`export-classes`**: Exports classes to the parent environment.

    * `(export-classes classes)`

*   **`ascii-code`**: Returns the ASCII code of a character.

    * `(ascii-code char) -> num`

*   **`ascii-char`**: Returns the character for an ASCII code.

    * `(ascii-char num) -> char`

*   **`ascii-upper`**: Converts a character to uppercase.

    * `(ascii-upper num) -> num`

*   **`ascii-lower`**: Converts a character to lowercase.

    * `(ascii-lower num) -> num`

*   **`to-upper`**: Converts a string to uppercase.

    * `(to-upper str) -> str`

*   **`to-lower`**: Converts a string to lowercase.

    * `(to-lower str) -> str`

*   **`align`**: Aligns a number to a specified boundary.

    * `(align num div) -> num`

*   **`str-as-num`**: Converts a string to a number.

    * `(str-as-num str) -> num`

*   **`num-to-utf8`**: Converts a number to a UTF-8 string.

    * `(num-to-utf8 num) -> str`

*   **`byte-to-hex-str`**: Converts a byte to a hex string.

    * `(byte-to-hex-str num) -> str`

*   **`short-to-hex-str`**: Converts a short to a hex string.

    * `(short-to-hex-str num) -> str`

*   **`int-to-hex-str`**: Converts an integer to a hex string.

    * `(int-to-hex-str num) -> str`

*   **`long-to-hex-str`**: Converts a long to a hex string.

    * `(long-to-hex-str num) -> str`

*   **`trim-start`**: Trims whitespace from the beginning of a string.

    * `(trim-start str [cls]) -> str`

*   **`trim-end`**: Trims whitespace from the end of a string.

    * `(trim-end str [cls]) -> str`

*   **`trim`**: Trims whitespace from the beginning and end of a string.

    * `(trim str [cls]) -> str`

*   **`split`**: Splits a string into a list of strings.

    * `(split str [cls]) -> strs`

*   **`pad`**: Pads a string to a specified length.

    * `(pad form width [str]) -> str`

*   **`get-ubyte`**: Gets an unsigned byte from a string.

    * `(get-ubyte str idx) -> num`

*   **`get-ushort`**: Gets an unsigned short from a string.

    * `(get-ushort str idx) -> num`

*   **`get-uint`**: Gets an unsigned integer from a string.

    * `(get-uint str idx) -> num`

*   **`get-long`**: Gets a long from a string.

    * `(get-long str idx) -> num`

*   **`get-byte`**: Gets a byte from a string.

    * `(get-byte str idx) -> num`

*   **`get-short`**: Gets a short from a string.

    * `(get-short str idx) -> num`

*   **`get-int`**: Gets an integer from a string.

    * `(get-int str idx) -> num`

*   **`get-nodeid`**: Gets a node ID from a string.

    * `(get-nodeid str idx) -> nodeid`

*   **`get-netid`**: Gets a net ID from a string.

    * `(get-netid str idx) -> netid`

*   **`get-cstr`**: Gets a C-style string from a string.

    * `(get-cstr str idx) -> str`

*   **`type-to-size`**: Returns the size of a type.

    * `(type-to-size sym) -> num`

*   **`time-in-seconds`**: Converts a time in nanoseconds to a string in seconds.

    * `(time-in-seconds time) -> str`

*   **`lisp-nodes`**: Returns a list of Lisp nodes.

    * `(lisp-nodes) -> nodes`

*   **`age`**: Returns the age of a file.

    * `(age path) -> 0 | time ns`

*   **`load-stream`**: Loads a file into a stream.

    * `(load-stream path) -> :nil | stream`

*   **`abs-path`**: Converts a relative path to an absolute path.

    * `(abs-path path [current]) -> path`

*   **`import`**: Imports a file.

    * `(import path [env]) -> env`

*   **`import-from`**: Imports symbols and classes from a file.

    * `(import-from [symbols classes])`

*   **`read-long`**: Reads a long from a stream.

    * `(read-long stream) -> num`

*   **`read-int`**: Reads an integer from a stream.

    * `(read-int stream) -> num`

*   **`read-short`**: Reads a short from a stream.

    * `(read-short stream) -> num`

*   **`write-long`**: Writes a long to a stream.

    * `(write-long stream list|num) -> bytes`

*   **`write-int`**: Writes an integer to a stream.

    * `(write-int stream list|num) -> bytes`

*   **`write-short`**: Writes a short to a stream.

    * `(write-short stream list|num) -> bytes`

*   **`read-data`**: Reads data from a stream.

    * `(read-data stream bytes) -> str`

*   **`os`**: Returns the operating system.

    * `(os) -> sym`

*   **`cpu`**: Returns the CPU architecture.

    * `(cpu) -> sym`

*   **`abi`**: Returns the ABI.

    * `(abi) -> sym`

*   **`within-compile-env`**: Executes a lambda within a compilation environment.

    * `(within-compile-env lambda)`

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

*   **`merge`**: Merges the elements of a source list into a destination
    list, ensuring uniqueness.

    * `(merge dlist slist) -> dlist`

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

### Fixed Point Math Functions

*   **`sin`**: Calculates the sine of a fixed-point number.

    * `(sin fixed) -> fixed`

*   **`cos`**: Calculates the cosine of a fixed-point number.

    * `(cos fixed) -> fixed`

*   **`frac`**: Returns the fractional part of a fixed-point number.

    * `(frac fixed) -> fixed`

*   **`floor`**: Returns the largest integer less than or equal to the fixed-point number.

    * `(floor fixed) -> fixed`

*   **`recip`**: Calculates the reciprocal of a fixed-point number.

    * `(recip fixed) -> fixed`

### Numeric Vector Functions

*   **`nums-abs`**: Calculates the absolute value of each element in a numeric vector.

    * `(nums-abs nums [nums]) -> nums`

*   **`nums-scale`**: Scales each element in a numeric vector by a scalar value.

    * `(nums-scale nums scale [nums]) -> nums`

*   **`nums-add`**: Adds two numeric vectors.

    * `(nums-add nums nums [nums]) -> nums`

*   **`nums-div`**: Divides two numeric vectors.

    * `(nums-div nums nums [nums]) -> nums`

*   **`nums-mod`**: Calculates the modulus of two numeric vectors.

    * `(nums-mod nums nums [nums]) -> nums`

*   **`nums-mul`**: Multiplies two numeric vectors.

    * `(nums-mul nums nums [nums]) -> nums`

*   **`nums-sub`**: Subtracts two numeric vectors.

    * `(nums-sub nums nums [nums]) -> nums`

*   **`nums-min`**: Calculates the minimum of two numeric vectors.

    * `(nums-min nums nums [nums]) -> nums`

*   **`nums-max`**: Calculates the maximum of two numeric vectors.

    * `(nums-max nums nums [nums]) -> nums`

*   **`nums-sum`**: Calculates the sum of all elements in a numeric vector.

    * `(nums-sum nums) -> num`

*   **`nums-dot`**: Calculates the dot product of two numeric vectors.

    * `(nums-dot nums nums) -> num`

## String and Character Functions

*   **`str`**: Converts any form into its string representation.

    * `(str form) -> str`

*   **`str-to-num`**: Parses a string and converts it to a number.

    * `(str-to-num str) -> num`

*   **`char`**: Creates a string from one or more numeric character codes.

    * `(char num [width]) -> str`

*   **`code`**: Returns the numeric code of a character in a string.

    * `(code str [width idx]) -> num`

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

*   **`read-blk`**: Reads a block from a stream.

    * `(read-blk stream bytes) -> :nil | str`

*   **`read-line`**: Reads a line of text from a stream.

    * `(read-line stream) -> :nil | str`

*   **`read-avail`**: Returns all available buffered data as a string.

    * `(read-avail stream) -> :nil | str`

*   **`write-char`**: Writes a single character or a list of character to a
    stream.

    * `(write-char stream list|num [width]) -> bytes`

*   **`write-blk`**: Writes a block to a stream.

    * `(write-blk stream str) -> bytes`

*   **`write-line`**: Writes a line to a stream.

    * `(write-line stream str) -> bytes`

*   **`stream-flush`**: Flushes any buffered output for a stream.

    * `(stream-flush stream) -> stream`

*   **`prin`**: Prints the string representation of its arguments to standard
    output without a trailing newline.

    * `(prin [form] ...)`

*   **`print`**: Prints the string representation of its arguments to standard
    output, followed by a newline character.

    * `(print [form] ...)`

*   **`in-stream`**: Creates an input stream.

    * `(in-stream) -> in_stream`

*   **`in-next-msg`**: Retrieves the next message from an input stream.

    * `(in-next-msg in_stream) -> msg`

*   **`in-mbox`**: Gets the mailbox associated with an input stream.

    * `(in-mbox in) -> mbox`

*   **`in-get-state`**: Gets the state of an input stream.

    * `(in-get-state in) -> num`

*   **`in-set-state`**: Sets the state of an input stream.

    * `(in-set-state in num) -> in`

*   **`out-stream`**: Creates an output stream.

    * `(out-stream mbox) -> out_stream`

*   **`create-stdio`**: Creates a standard I/O stream object.

    * `(create-stdio) -> stdio`

*   **`stdio-get-args`**: Gets the command line arguments from a stdio object.

    * `(stdio-get-args stdio) -> cmd_line`

*   **`stream-avail`**: Returns the number of available bytes in a stream.

    * `(stream-avail stream) -> num`

*   **`stream-seek`**: Seeks to a position in a stream.

    * `(stream-seek stream offset whence) -> stream`

*   **`read-bits`**: Reads a specified number of bits from a stream.

    * `(read-bits stream (array bit_pool bit_pool_size) num_bits) -> (data|-1)`

*   **`write-bits`**: Writes a specified number of bits to a stream.

    * `(write-bits stream (array bit_pool bit_pool_size) data num_bits) -> stream`

*   **`memory-stream`**: Creates a memory stream.

    * `(memory-stream) -> stream`

*   **`flush-bits`**: Flushes any remaining bits in the bit pool to the stream.

    * `(flush-bits stream bit_pool_state)`

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

    * `(type-of obj) -> (... :obj)`

*   **`hash`**: Computes the hash value of a form.

    * `(hash obj) -> num`

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

    * `(obj-ref num) -> obj`
