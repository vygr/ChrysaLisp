# Lisp Primitives

This document outlines the complete family groups of low-level Lisp primitives
and built-in functions in ChrysaLisp.

These components encompass both foreign function interfaces (FFI) to the Virtual
Processor (VP) core and foundational macros defined in `root.inc`.

## Core Evaluation and Special Forms

These are the fundamental evaluation primitives and special forms recognized by
the interpreter and macro expander.

*	**`ffi`**: Foreign Function Interface. Binds a Lisp symbol to an
	underlying Virtual Processor function implementation.

	*	`(ffi path [sym flags]) -> func`

*	**`lambda`**: Creates an anonymous function template executed in the
	caller's lexical environment.

	*	`(lambda (param ...) body ...)`

*	**`macro`**: Creates a compile-time syntactic transformation macro.

	*	`(macro (param ...) body ...)`

*	**`quote`**: Inhibits evaluation of a form, returning the unevaluated
	expression.

	*	`(quote form)` or `'form`

*	**`quasi-quote`**: Quotes a template while permitting selective evaluation
	via unquote (`,`) and unquote-splicing (`~`).

	*	`(quasi-quote form)` or `` `form ``

*	**`progn`**: Evaluates expressions sequentially from left to right and
	returns the result of the final expression.

	*	`(progn [body ...]) -> 'form`

*	**`catch`**: Evaluates a form within an exception handler block. If an
	error or throw occurs, the fallback expression `eform` is evaluated with the
	error payload bound to `_`.

	*	`(catch form eform) -> 'form`

*	**`throw`**: Non-local exit that unwinds the call stack to the nearest
	enclosing `catch`.

	*	`(throw str form)`

*	**`bind`**: Destructures sequence elements into local symbol bindings.

	*	`(bind (sym ...) seq) -> val`

*	**`identity`**: Returns its argument unchanged.

	*	`(identity [form]) -> :nil | form`

## Evaluation, Compilation, and Metaprogramming

Tools for runtime code evaluation, compilation lifecycle control, and macro
expansion.

*	**`eval`**: Evaluates a form within a specified (or current) lexical
	environment.

	*	`(eval form [env]) -> 'form`

*	**`eval-list`**: Evaluates every element of a list sequentially.

	*	`(eval-list list [env]) -> list`

*	**`apply`**: Invokes a function or lambda template with arguments provided
	as a sequence.

	*	`(apply lambda seq) -> form`

*	**`macroexpand`**: Expands macro calls recursively without evaluating the
	resulting form.

	*	`(macroexpand form) -> 'form`

*	**`prebind`**: Resolves symbol bindings and function call sites at compile
	time for O(1) performance.

	*	`(prebind form) -> form`

*	**`macrobind`**: Expands macros and prebinds symbols in a single pass.

	*	`(macrobind form) -> (prebind (macroexpand form))`

*	**`exec`**: Evaluates a macrobound form.

	*	`(exec form)`

*	**`const`**: Forces evaluation of an expression at compile time.

	*	`(const form)`

*	**`static-q`**: Produces a statically quoted, macrobound form.

	*	`(static-q form) -> 'form`

*	**`static-qq`**: Produces a statically quasi-quoted, macrobound form.

	*	`(static-qq form) -> `form`

*	**`static-qqp`**: Statically quasi-quotes a form with prebinding only.

	*	`(static-qqp form) -> `form`

*	**`read`**: Parses a single S-expression from an input stream.

	*	`(read stream [last_char]) -> :nil | (form next_char)`

*	**`repl`**: Initiates a Read-Eval-Print Loop on a stream.

	*	`(repl stream name) -> form`

*	**`repl-info`**: Returns the source file path and current line number of the
	active REPL stream.

	*	`(repl-info) -> (name line)`

*	**`defun` / `redefun`**: Binds a symbol to a function definition. `defun`
	guards against accidental overrides, while `redefun` allows re-binding.

	*	`(defun name ([arg ...]) body)`

	*	`(redefun name ([arg ...]) body)`

*	**`defmacro` / `redefmacro`**: Defines or redefines a macro.

	*	`(defmacro name ([arg ...]) body)`

	*	`(redefmacro name ([arg ...]) body)`

*	**`callback`**: Evaluates an application callback in a target environment.

	*	`(callback lambda env arg ...)`

*	**`setd`**: Assigns default fallback values to variables if they evaluate to
	`:nil`.

	*	`(setd sym val [sym val] ...)`

*	**`#`**: Anaphoric macro generating an inline lambda using positional
	placeholders `%0` through `%9`.

	*	`(# (< %0 %1))`

## Conditional and Control Flow Functions

Conditionals, iterative loops, and branch constructs.

*	**`if` / `ifn`**: Evaluates a test condition and branches accordingly.

	*	`(if tst form [else_form]) -> 'form`

	*	`(ifn tst form [else_form]) -> 'form`

*	**`cond` / `condn`**: Multi-way conditional branching.

	*	`(cond [(tst body)] ...) -> 'form`

	*	`(condn [(tst body)] ...) -> 'form`

*	**`when` / `unless`**: Single-branch conditionals executing bodies on
	truthy or falsy tests.

	*	`(when tst body ...)`

	*	`(unless tst body ...)`

*	**`while` / `until`**: Loop constructs checking conditions before each
	iteration.

	*	`(while tst [body ...]) -> :nil`

	*	`(until tst [body ...]) -> tst`

*	**`for`**: Iterates an index over an integer range.

	*	`(for start end [body ...])`

*	**`times`**: Executes a code block a fixed number of iterations.

	*	`(times num [body ...])`

*	**`case` / `pcase`**: Matches a key against values or property sets.

	*	`(case key [(match body)] ...)`

	*	`(pcase key symbols [(match body)] ...)`

*	**`inc` / `dec`**: Returns a number incremented or decremented by 1.

	*	`(inc num) -> num`

	*	`(dec num) -> num`

*	**`++` / `--`**: In-place variable mutation by an increment or decrement.

	*	`(++ var [step]) -> num`

	*	`(-- var [step]) -> num`

*	**`not`**: Logical boolean inversion.

	*	`(not form) -> :t | :nil`

*	**`or`**: Short-circuit logical OR evaluating left-to-right.

	*	`(or [tst] ...) -> :nil | tst`

*	**`and`**: Short-circuit logical AND evaluating left-to-right.

	*	`(and [tst] ...) -> :t | :nil | tst`

## Sequence Manipulation and Slicing

Unified sequence operations for lists, arrays, paths, and strings.

*	**`length`**: Returns the element count or byte length of a sequence.

	*	`(length seq) -> num`

*	**`elem-get`**: Reads an element at a zero-based index. Negative indices
	index from the end.

	*	`(elem-get seq idx) -> elem`

*	**`first` / `second` / `third` / `last`**: Fast positional element access.

	*	`(first seq) -> :nil | elem`

	*	`(second seq) -> :nil | elem`

	*	`(third seq) -> :nil | elem`

	*	`(last seq) -> :nil | elem`

*	**`rest` / `most`**: Subsequence extraction stripping extremities.

	*	`(rest seq) -> empty | seq`

	*	`(most seq) -> empty | seq`

*	**`slice`**: Extracts a sub-vector between start and end indices.

	*	`(slice seq start end) -> seq`

*	**`splice`**: Slices and merges two sequences according to an index vector.

	*	`(splice seq1 seq2 idxs) -> seq`

*	**`partition`**: Chunks a sequence into equal-sized sub-sequences.

	*	`(partition seq [cnt]) -> (seq ...)`

*	**`cat`**: Concatenates multiple sequences into a single instance.

	*	`(cat seq ...) -> seq`

*	**`slices`**: Groups contiguous integer indices into `(start end)` ranges.

	*	`(slices list) -> ((s0 e0) (s1 e1) ...)`

*	**`join`**: Joins a list of sequences with an interleaved separator.

	*	`(join seqs sep [mode]) -> seq`

*	**`unzip`**: De-interleaves a sequence into `cnt` separate lists.

	*	`(unzip seq cnt) -> seqs`

*	**`zip`**: Interleaves multiple sequences into a single sequence.

	*	`(zip seq ...) -> seq`

*	**`unique`**: Filters adjacent duplicate items from a sorted sequence.

	*	`(unique seq) -> seq`

*	**`flatten`**: Recursively unwraps nested list structures into a flat list.

	*	`(flatten list) -> list`

*	**`max-length` / `min-length`**: Finds maximum or minimum sequence lengths.

	*	`(max-length list) -> max`

	*	`(min-length list) -> min`

*	**`erase`**: Removes a range from a sequence.

	*	`(erase seq start end) -> seq`

*	**`insert`**: Inserts a sequence into another at a specified index.

	*	`(insert seq pos seq) -> seq`

*	**`replace`**: Overwrites a subsequence range with replacement elements.

	*	`(replace seq start end seq) -> seq`

*	**`rotate`**: Rotates a subsequence internally around a midpoint index.

	*	`(rotate seq start mid end) -> seq`

*	**`reverse`**: Inverts the order of elements in a sequence.

	*	`(reverse seq) -> seq`

## Sequence Searching and Matching

Searching algorithms and character-class predicates.

*	**`find` / `rfind`**: Linear element search forward or backward.

	*	`(find elem seq [idx]) -> :nil | idx`

	*	`(rfind elem seq [idx]) -> :nil | idx`

*	**`bfind`**: Fast binary search for a byte within a sorted character-class.

	*	`(bfind char cls) -> :nil | idx`

*	**`bskip` / `bskipn`**: Scans forward skipping matching or non-matching
	characters.

	*	`(bskip cls str idx) -> idx`

	*	`(bskipn cls str idx) -> idx`

*	**`rbskip` / `rbskipn`**: Scans backward skipping matching or non-matching
	characters.

	*	`(rbskip cls str idx) -> idx`

	*	`(rbskipn cls str idx) -> idx`

## Sequence Iteration and Functional Primitives

Iterative sequence processors utilizing small, non-recursive stack allocations.

*	**`each!`**: Iterates through sequences in parallel, calling a lambda.

	*	`(each! lambda seqs [start end])`

*	**`some!`**: Iterates until the callback returns a non-`:nil` value.

	*	`(some! lambda seqs [mode start end]) -> :nil | val`

*	**`map!`**: Transforms sequence items into an output list.

	*	`(map! lambda seqs [start end out]) -> out | (...)`

*	**`reduce!`**: Accumulates values across sequence items using a reducer.

	*	`(reduce! lambda seqs init [start end]) -> val`

*	**`filter!`**: Filters sequence items based on predicate truthiness.

	*	`(filter! lambda seq [start end out]) -> out | (...)`

*	**`lines!`**: Line-by-line iterator over an input stream.

	*	`(lines! lambda stream [start end]) -> :nil`

*	**`!`**: Anaphoric variable representing the current iteration index in
	`...!` forms.

	*	`(!)`

*	**`each` / `reach`**: Iterates forward or backward across sequences.

	*	`(each lambda seq ...)`

	*	`(reach lambda seq ...)`

*	**`map` / `rmap`**: Maps a lambda forward or backward over sequences.

	*	`(map lambda seq ...) -> list`

	*	`(rmap lambda seq ...) -> list`

*	**`reduce` / `rreduce`**: Accumulates a sequence forward or backward.

	*	`(reduce lambda seq [init]) -> form`

	*	`(rreduce lambda seq [init]) -> form`

*	**`filter`**: Filters elements of a sequence into a list.

	*	`(filter lambda seq) -> list`

*	**`some` / `rsome`**: Finds the first truthy value forward or backward.

	*	`(some lambda seq ...) -> :nil | form`

	*	`(rsome lambda seq ...) -> :nil | form`

*	**`every`**: Tests if all items evaluate truthy.

	*	`(every lambda seq ...) -> :t | :nil`

*	**`notany`**: Tests if all items evaluate falsy.

	*	`(notany lambda seq ...) -> :t | :nil`

*	**`notevery`**: Tests if at least one item evaluates falsy.

	*	`(notevery lambda seq ...) -> :t | :nil`

*	**`each-mergeable`**: Iterates over a sequence that can expand dynamically.

	*	`(each-mergeable lambda seq) -> seq`

## Array and List Operations

Primitives targeting mutable `:array` and `:list` data structures.

*	**`cap`**: Sets the preallocated storage capacity of an array or list.

	*	`(cap len array ...) -> array`

*	**`clear`**: Erases all elements and resets length to zero.

	*	`(clear array ...) -> array`

*	**`push`**: Appends one or more items to the end of an array or list.

	*	`(push array elem ...) -> array`

*	**`pop`**: Removes and returns the final element.

	*	`(pop array) -> elem | :nil`

*	**`elem-set`**: Modifies an element at a given index.

	*	`(elem-set array idx elem) -> array`

*	**`merge`**: Merges items into a destination list ensuring set uniqueness.

	*	`(merge dlist slist) -> dlist`

*	**`pivot`**: Quick-sort array partitioning primitive around a pivot index.

	*	`(pivot lambda list start end)`

*	**`lmatch?`**: Checks structural equality between lists.

	*	`(lmatch? list list) -> :nil | :t`

*	**`copy`**: Creates a deep copy of a list or form.

	*	`(copy form) -> 'form`

*	**`sort` / `usort`**: In-place list sorting and unique-sorting.

	*	`(sort list [fcmp start end]) -> list`

	*	`(usort list [fcmp start end]) -> list`

*	**`swap`**: Exchanges the elements at two indices.

	*	`(swap list idx1 idx2)`

*	**`shuffle`**: Pseudo-randomly shuffles list elements.

	*	`(shuffle list [start end]) -> list`

*	**`range`**: Generates a list of integers over an interval.

	*	`(range start end [step]) -> list`

*	**`lists`**: Allocates `n` independent, mutable empty lists.

	*	`(lists n) -> ((list0) ... (listn-1))`

## Property Sets and Property Maps (`pset`, `pmap`)

Flat array-backed property maps and sets optimized for symbol keys.

*	**`pset`**: Constructs a new property set from keys.

	*	`(pset [key ...]) -> pset`

*	**`pmap`**: Constructs a new property map from key-value pairs.

	*	`(pmap [key val ...]) -> pmap`

*	**`pinsert`**: Inserts a key or key-value pair into a property collection.

	*	`(pinsert props key [val]) -> props`

*	**`perase`**: Erases a key from a property collection.

	*	`(perase props key) -> props`

*	**`pfind`**: Looks up a key, returning the value (for maps) or key (for sets).

	*	`(pfind props key) -> val | key | :nil`

*	**`pfindi`**: Looks up a key, returning the raw element index.

	*	`(pfindi props key) -> idx | :nil`

## Predicates and Type Checking

Type inspection and structural verification predicates.

*	**`lambda?` / `macro?`**: Checks if an object is a function or macro.

	*	`(lambda? form) -> :t | :nil`

	*	`(macro? form) -> :t | :nil`

*	**`quote?` / `quasi-quote?`**: Checks if an object is a quote form.

	*	`(quote? form) -> :t | :nil`

	*	`(quasi-quote? form) -> :t | :nil`

*	**`array?` / `list?` / `list??`**: Type hierarchy tests for collections.

	*	`(array? form) -> :t | :nil`

	*	`(list? form) -> :t | :nil`

	*	`(list?? form) -> :t | :nil`

*	**`pset?` / `pmap?`**: Checks for property set or map instances.

	*	`(pset? form) -> :t | :nil`

	*	`(pmap? form) -> :t | :nil`

*	**`num?` / `fixed?` / `real?`**: Numerical scalar type checks.

	*	`(num? form) -> :t | :nil`

	*	`(fixed? form) -> :t | :nil`

	*	`(real? form) -> :t | :nil`

*	**`nums?` / `fixeds?` / `reals?`**: Numeric vector type checks.

	*	`(nums? form) -> :t | :nil`

	*	`(fixeds? form) -> :t | :nil`

	*	`(reals? form) -> :t | :nil`

*	**`func?` / `str?` / `sym?` / `env?` / `seq?`**: Core object type checks.

	*	`(func? form) -> :t | :nil`

	*	`(str? form) -> :t | :nil`

	*	`(sym? form) -> :t | :nil`

	*	`(env? form) -> :t | :nil`

	*	`(seq? form) -> :t | :nil`

*	**`lambda-func?` / `macro-func?`**: Checks if a list is a lambda/macro.

	*	`(lambda-func? form) -> :t | :nil`

	*	`(macro-func? form) -> :t | :nil`

*	**`nil?` / `atom?` / `empty?` / `nempty?`**: Structural predicates.

	*	`(nil? o) -> :t | :nil`

	*	`(atom? o) -> :t | :nil`

	*	`(empty? form) -> :t | :nil`

	*	`(nempty? form) -> :t | :nil`

*	**`msafe?`**: Checks if a form can be evaluated repeatedly in macros
	without side-effect hazards.

	*	`(msafe? o) -> :t | :nil`

*	**`lisp-node?` / `cpp-node?`**: Distinguishes Lisp node identities.

	*	`(lisp-node? node) -> :t | :nil`

	*	`(cpp-node? node) -> :t | :nil`

*	**`neg?` / `pos?` / `odd?` / `even?`**: Numerical properties.

	*	`(neg? num) -> :t | :nil`

	*	`(pos? num) -> :t | :nil`

	*	`(odd? num) -> :t | :nil`

	*	`(even? num) -> :t | :nil`

## Numeric, Math, and Vector Operations

Arithmetic, bitwise math, trigonometry, and SIMD-like numerical vector operations.

### Comparison and Arithmetic Operators

*	**`=` / `/=` / `<` / `>` / `<=` / `>=`**: Scalar numeric comparisons.

	*	`(= num num ...) -> :t | :nil`

*	**`+` / `-` / `*` / `/` / `%`**: Arithmetic addition, subtraction,
	multiplication, division, and modulo.

	*	`(+ num num ...) -> num`

*	**`min` / `max`**: Minimum or maximum of numbers.

	*	`(min num num ...) -> num`

	*	`(max num num ...) -> num`

*	**`neg` / `abs` / `sign` / `sqrt`**: Numeric transformations.

	*	`(neg num) -> num`

	*	`(abs num) -> num`

	*	`(sign num) -> -1 | 0 | 1`

	*	`(sqrt num) -> num`

### Bitwise and Bitfield Operators

*	**`>>` / `>>>` / `<<`**: Logical right shift, arithmetic right shift, and
	left shift.

	*	`(>> num cnt) -> num`

	*	`(>>> num cnt) -> num`

	*	`(<< num cnt) -> num`

*	**`logand` / `logior` / `logxor` / `lognot`**: Bitwise boolean operations.

	*	`(logand [num] ...) -> num`

	*	`(logior [num] ...) -> num`

	*	`(logxor [num] ...) -> num`

	*	`(lognot num) -> num`

*	**`bitcnt` / `ntz` / `nto` / `nlz` / `nlo`**: Bit inspection operations.

	*	`(bitcnt n) -> num`

	*	`(ntz num) -> num`

	*	`(nto num) -> num`

	*	`(nlz num) -> num`

	*	`(nlo num) -> num`

### Numerical Types and Conversion

*	**`n2i` / `n2f` / `n2r`**: Inter-type numerical conversion between integer,
	fixed-point (16.16), and 64-bit double precision real.

	*	`(n2i num) -> num`

	*	`(n2f num) -> fixed`

	*	`(n2r num) -> real`

*	**`num`**: Interns a 64-bit integer.

	*	`(num num) -> num`

*	**`random`**: Returns a pseudo-random integer within `[0, num)`.

	*	`(random num) -> num`

*	**`log2` / `pow` / `align`**: Mathematical powers and alignment.

	*	`(log2 num) -> num`

	*	`(pow base exponent) -> integer`

	*	`(align num div) -> num`

### Fixed-Point Trigonometry and Real Operations

*	**`sin` / `cos`**: Fixed-point sine and cosine operations.

	*	`(sin fixed) -> fixed`

	*	`(cos fixed) -> fixed`

*	**`frac` / `floor` / `ceil` / `recip`**: Fractional part, floor, ceiling,
	and reciprocal calculation.

	*	`(frac fixed) -> fixed`

	*	`(floor fixed) -> fixed`

	*	`(ceil fixed) -> fixed`

	*	`(recip fixed) -> fixed`

*	**`quant`**: Quantizes a real number to a specified tolerance.

	*	`(quant real tol) -> real`

### Numeric Vector Operations (`nums`, `fixeds`, `reals`)

High-performance vector operations executed across packed numeric vectors.

*	**`nums-add` / `nums-sub` / `nums-mul` / `nums-div` / `nums-mod`**: Element-wise
	vector arithmetic.

	*	`(nums-add nums nums [nums]) -> nums`

	*	`(nums-sub nums nums [nums]) -> nums`

	*	`(nums-mul nums nums [nums]) -> nums`

	*	`(nums-div nums nums [nums]) -> nums`

	*	`(nums-mod nums nums [nums]) -> nums`

*	**`nums-abs` / `nums-scale`**: Vector absolute value and scaling.

	*	`(nums-abs nums [nums]) -> nums`

	*	`(nums-scale nums scale [nums]) -> nums`

*	**`nums-min` / `nums-max`**: Element-wise minimum and maximum.

	*	`(nums-min nums nums [nums]) -> nums`

	*	`(nums-max nums nums [nums]) -> nums`

*	**`nums-sum` / `nums-dot`**: Vector reduction sum and dot-product.

	*	`(nums-sum nums) -> num`

	*	`(nums-dot nums nums) -> num`

*	**`fixeds-frac` / `fixeds-floor` / `fixeds-ceil`**: Vector fixed-point
	rounding.

	*	`(fixeds-frac fixeds [fixeds]) -> fixeds`

	*	`(fixeds-floor fixeds [fixeds]) -> fixeds`

	*	`(fixeds-ceil fixeds [fixeds]) -> fixeds`

*	**`reals-quant`**: Quantizes an entire vector of real numbers.

	*	`(reals-quant reals tol [reals]) -> reals`

*	**`mat4x4-mul` / `mat4x4-inv`**: 4x4 matrix multiplication and inversion.

	*	`(mat4x4-mul reals reals [reals]) -> reals`

	*	`(mat4x4-inv reals [reals]) -> reals`

*	**`mat4x4-vec4-mul` / `mat4x4-vec3-mul`**: Matrix-vector transformation.

	*	`(mat4x4-vec4-mul reals reals [reals]) -> reals`

	*	`(mat4x4-vec3-mul reals reals [reals]) -> reals`

## String and Character Functions

*	**`str`**: Serializes any Lisp expression to a string.

	*	`(str form) -> str`

*	**`str-to-num` / `str-as-num` / `str-to-real`**: String numeric parsing.

	*	`(str-to-num str) -> num`

	*	`(str-as-num str) -> num`

	*	`(str-to-real str) -> real`

*	**`char` / `code`**: Character creation and character extraction.

	*	`(char num [width]) -> str`

	*	`(code str [width idx]) -> num`

*	**`expand` / `compress`**: Tab expansion and space compression.

	*	`(expand str tab_width idx) -> str`

	*	`(compress str tab_width idx) -> str`

*	**`hex-encode` / `hex-decode`**: Hexadecimal encoding and decoding.

	*	`(hex-encode str) -> str`

	*	`(hex-decode str) -> str`

*	**`cmp`**: Lexicographical comparison of two strings.

	*	`(cmp str str) -> + | 0 | -`

*	**`save` / `load`**: File string serialization.

	*	`(save str path) -> str`

	*	`(load path) -> str`

*	**`str-alloc`**: Preallocates an empty string buffer of given byte size.

	*	`(str-alloc size) -> str`

*	**`starts-with` / `ends-with`**: Prefix and suffix matching.

	*	`(starts-with str str) -> :t | :nil`

	*	`(ends-with str str) -> :t | :nil`

*	**`escape` / `unescape`**: Escape character processing.

	*	`(escape str) -> str`

	*	`(unescape str) -> str`

*	**`split`**: Splits a string by delimiter characters.

	*	`(split str [cls]) -> strs`

*	**`to-upper` / `to-lower`**: ASCII case conversion.

	*	`(to-upper str) -> str`

	*	`(to-lower str) -> str`

*	**`ascii-code` / `ascii-char` / `ascii-upper` / `ascii-lower`**: Character
	conversions.

	*	`(ascii-code char) -> num`

	*	`(ascii-char num) -> char`

	*	`(ascii-upper num) -> num`

	*	`(ascii-lower num) -> num`

*	**`num-to-utf8`**: Encodes a Unicode code point to a UTF-8 string.

	*	`(num-to-utf8 num) -> str`

*	**`byte-to-hex-str` / `short-to-hex-str` / `int-to-hex-str` / `long-to-hex-str`**:
	Hex string formatters.

	*	`(byte-to-hex-str num) -> str`

	*	`(short-to-hex-str num) -> str`

	*	`(int-to-hex-str num) -> str`

	*	`(long-to-hex-str num) -> str`

*	**`trim` / `trim-start` / `trim-end`**: Strips whitespace or character classes.

	*	`(trim str [cls]) -> str`

	*	`(trim-start str [cls]) -> str`

	*	`(trim-end str [cls]) -> str`

*	**`pad`**: Pads a string to a specified width.

	*	`(pad form width [str]) -> str`

*	**`get-ubyte` / `get-ushort` / `get-uint` / `get-long` / `get-real` / `get-byte` / `get-short` / `get-int`**:
	Typed memory getters from strings or objects.

	*	`(get-ubyte obj idx) -> num`

*	**`get-str` / `get-cstr`**: Substring and null-terminated string getters.

	*	`(get-str obj idx bytes) -> str`

	*	`(get-cstr obj idx) -> str`

*	**`set-byte` / `set-short` / `set-int` / `set-long` / `set-real` / `set-str`**:
	Typed memory setters for strings or objects.

	*	`(set-byte obj idx val) -> str`

## Symbol Functions

*	**`sym`**: Interns a string into a unique symbol.

	*	`(sym str) -> sym`

*	**`gensym`**: Generates a new, globally unique interned symbol.

	*	`(gensym) -> sym`

## Environment and Variable Binding

Lexical environment creation and variable manipulation.

*	**`defq` / `setq`**: Defines or mutates variables in the current lexical
	environment.

	*	`(defq sym val [sym val] ...) -> val`

	*	`(setq sym val [sym val] ...) -> val`

*	**`def` / `set`**: Defines or mutates variables in an explicit target
	environment.

	*	`(def env sym val [sym val] ...) -> val`

	*	`(set env sym val [sym val] ...) -> val`

*	**`get` / `def?`**: Resolves symbol values via scope traversal or local
	lookup.

	*	`(get sym [env]) -> :nil | val`

	*	`(def? sym [env]) -> :nil | val`

*	**`undef`**: Unbinds symbols from a specified environment.

	*	`(undef env sym [sym] ...) -> env`

*	**`env`**: Creates an environment or returns the active lexical scope.

	*	`(env [num]) -> env`

*	**`penv`**: Returns the parent environment of a scope.

	*	`(penv [env]) -> :nil | env`

*	**`env-push` / `env-pop`**: Pushes or pops a child scope in the environment
	tree.

	*	`(env-push [env]) -> 'env`

	*	`(env-pop [env]) -> 'env`

*	**`env-resize`**: Reallocates an environment hash map with new bucket counts.

	*	`(env-resize num [env]) -> env`

*	**`env-copy`**: Clones an environment.

	*	`(env-copy env num) -> env`

*	**`tolist`**: Converts an environment into a list of `(sym val)` bindings.

	*	`(tolist env) -> ((sym val) ...)`

*	**`let` / `let*`**: Parallel or sequential lexical variable binding scopes.

	*	`(let ([(sym val) ...]) body ...)`

	*	`(let* ([(sym val) ...]) body ...)`

*	**`export` / `export-symbols` / `export-classes`**: Exports bindings to
	enclosing environments.

	*	`(export env symbols)`

	*	`(export-symbols symbols)`

	*	`(export-classes classes)`

## Data Structure Constructors

*	**`array`**: Constructs an untyped 64-bit array.

	*	`(array [num ...]) -> array`

*	**`list`**: Constructs a general Lisp list.

	*	`(list [elem ...]) -> list`

*	**`nums`**: Constructs a 64-bit integer vector.

	*	`(nums [num ...]) -> nums`

*	**`fixeds`**: Constructs a 16.16 fixed-point vector.

	*	`(fixeds [fixed ...]) -> fixeds`

*	**`reals`**: Constructs a 64-bit double-precision real vector.

	*	`(reals [real ...]) -> reals`

*	**`path`**: Constructs a 2D vector graphic path.

	*	`(path [fixed ...]) -> path`

*	**`dim`**: Constructs an N-dimensional tensor wrapping a flat array.

	*	`(dim nums array) -> dim`

*	**`dim-get` / `dim-set`**: Reads or writes elements in an N-dimensional
	array.

	*	`(dim-get dim nums) -> elem`

	*	`(dim-set dim nums elem) -> array`

## Stream and I/O Functions

Stream interfaces for files, memory buffers, pipes, and standard I/O.

*	**`io-stream`**: Returns a handle to standard I/O streams (`"stdin"`,
	`"stdout"`, `"stderr"`).

	*	`(io-stream io) -> :nil | stream`

*	**`string-stream`**: Constructs a stream backed by an in-memory string.

	*	`(string-stream str) -> stream`

*	**`file-stream`**: Opens a file stream.

	*	`(file-stream path [mode]) -> :nil | stream`

*	**`memory-stream`**: Constructs a dynamically expanding memory stream.

	*	`(memory-stream) -> stream`

*	**`read-char` / `write-char`**: Character-level stream I/O.

	*	`(read-char stream [width]) -> :nil | num`

	*	`(write-char stream list|num [width]) -> bytes`

*	**`read-blk` / `write-blk`**: Block-level stream byte transfer.

	*	`(read-blk stream bytes) -> :nil | str`

	*	`(write-blk stream str) -> bytes`

*	**`read-line` / `write-line`**: Text line stream I/O.

	*	`(read-line stream) -> :nil | str`

	*	`(write-line stream str) -> bytes`

*	**`read-avail` / `stream-avail`**: Inspects available unread bytes in stream
	buffers.

	*	`(read-avail stream) -> :nil | num`

	*	`(stream-avail stream) -> num`

*	**`stream-flush`**: Flushes unwritten stream buffers to host sinks.

	*	`(stream-flush stream) -> stream`

*	**`stream-seek`**: Repositions the stream offset pointer.

	*	`(stream-seek stream offset whence) -> stream`

*	**`read-bits` / `write-bits`**: Bitfield stream I/O using a state tuple.

	*	`(read-bits stream (array bit_pool bit_pool_size) num_bits) -> (data|-1)`

	*	`(write-bits stream (array bit_pool bit_pool_size) data num_bits) -> stream`

*	**`fill-bits` / `copy-bits`**: Batch bitstream replication primitives.

	*	`(fill-bits stream (array bit_pool bit_pool_size) data num_bits cnt) -> stream`

	*	`(copy-bits wstream rstream (array wbit_pool wbit_pool_size) (array rbit_pool rbit_pool_size) num_bits cnt) -> wstream`

*	**`flush-bits`**: Flushes partial bytes remaining in a bit pool to a stream.

	*	`(flush-bits stream (array bit_pool bit_pool_size))`

*	**`load-stream`**: Reads a file directly into an active string-stream.

	*	`(load-stream path) -> :nil | stream`

*	**`read-ubyte` / `read-ushort` / `read-uint` / `read-byte` / `read-short` / `read-int` / `read-long`**:
	Typed binary stream getters.

	*	`(read-ubyte stream) -> num`

*	**`write-byte` / `write-short` / `write-int` / `write-long`**: Typed binary
	stream writers.

	*	`(write-long stream list|num) -> bytes`

*	**`in-stream` / `in-next-msg` / `in-mbox`**: Message-driven input stream
	endpoints.

	*	`(in-stream) -> in_stream`

	*	`(in-next-msg in_stream) -> msg`

	*	`(in-mbox in) -> mbox`

*	**`in-get-state` / `in-set-state`**: Stream lifecycle state inspection.

	*	`(in-get-state in) -> num`

	*	`(in-set-state in num) -> in`

*	**`out-stream` / `out-set-state`**: Message-driven output stream endpoints.

	*	`(out-stream mbox) -> out_stream`

	*	`(out-set-state out num) -> out`

*	**`create-stdio` / `stdio-get-args`**: Process stdio encapsulation.

	*	`(create-stdio) -> stdio`

	*	`(stdio-get-args stdio) -> cmd_line`

*	**`prin` / `print`**: Standard output printing with or without newlines.

	*	`(prin [form] ...) -> form`

	*	`(print [form] ...) -> form`

## Object and Class System

Object-oriented dispatch, class inheritance, and property modeling.

*	**`.`**: Direct O(1) method dispatch on class vtables.

	*	`(. env sym [...]) -> form`

*	**`defclass`**: Defines an object class inheriting from a base class.

	*	`(defclass Name ([arg ...]) (super ...)|:nil body ...)`

*	**`defmethod`**: Binds a concrete method implementation to a class vtable.

	*	`(defmethod name ([arg ...]) body ...)`

*	**`defabstractmethod`**: Declares an abstract method stub.

	*	`(defabstractmethod name ([arg ...]))`

*	**`deffimethod`**: Binds a method directly to an FFI entry point.

	*	`(deffimethod name ffi)`

*	**`defgetmethod` / `defsetmethod`**: Auto-generates property getters and
	setters.

	*	`(defgetmethod :field)`

	*	`(defsetmethod :field)`

*	**`defproxymethod`**: Auto-generates proxy delegators to child fields.

	*	`(defproxymethod name args field [ret_flag])`

*	**`.?`**: Tests if an object implements a given method.

	*	`(.? this method) -> :nil | lambda`

*	**`.super`**: Invokes a superclass method implementation explicitly.

	*	`(.super this :method [arg ...])`

*	**`.->`**: Macro for chaining method invocations on an object instance.

	*	`(.-> this form ...)`

*	**`raise` / `lower`**: Synchronizes object properties to/from local
	lexical variables.

	*	`(raise field | (sym val) ...)`

	*	`(lower field | (field sym) ...)`

*	**`type-of`**: Returns the type inheritance chain of an object.

	*	`(type-of obj) -> (... :obj)`

*	**`hash`**: Computes an object's numeric hash code.

	*	`(hash obj) -> num`

*	**`eql` / `nql`**: Equality and inequality predicates.

	*	`(eql obj obj) -> :nil | :t`

	*	`(nql obj obj) -> :nil | :t`

## Low-Level Object and Memory Layout

Low-level memory offsets, structures, bitfields, and address references.

*	**`obj-get`**: Reads raw typed memory at an offset within an object.

	*	`(obj-get obj offset type) -> val`

*	**`obj-set`**: Writes raw typed memory at an offset within an object.

	*	`(obj-set obj offset type val) -> obj`

*	**`weak-ref`**: Obtains the raw numeric memory address of an object.

	*	`(weak-ref obj) -> num`

*	**`obj-ref`**: Reconstructs an object reference from a memory address.

	*	`(obj-ref num) -> obj`

*	**`structure`**: Declares binary struct field offsets and types.

	*	`(structure name base [(type field ...)] ...)`

*	**`getf` / `setf`**: Accesses struct fields on objects by offset metadata.

	*	`(getf obj field [offset]) -> value`

	*	`(setf obj field value [offset]) -> obj`

*	**`getf->` / `setf->`**: Batch struct field readers and writers.

	*	`(getf-> obj field|(field offset) ...) -> (val ...)`

	*	`(setf-> obj (field val [offset]) ...) -> obj`

*	**`enums`**: Declares sequential enumeration constants.

	*	`(enums name base [(enum field ...)] ...)`

*	**`bits` / `bits?` / `bit-mask`**: Bitfield declarations and testing.

	*	`(bits name base [(bit field ...)] ...)`

	*	`(bits? val mask ...) -> :t | :nil`

	*	`(bit-mask mask ...) -> val`

## Mailbox, Networking, and Inter-Process Communication (IPC)

Location-transparent messaging using ephemeral `netid` addresses.

*	**`mail-mbox`**: Allocates an ephemeral mailbox `netid` on the local node.

	*	`(mail-mbox) -> netid`

*	**`mail-declare`**: Advertises a named service endpoint to the cluster.

	*	`(mail-declare mbox name info) -> key`

*	**`mail-nodes`**: Returns all active node IDs discovered on the network.

	*	`(mail-nodes) -> nodeids`

*	**`mail-enquire`**: Queries network nodes for services matching a prefix.

	*	`(mail-enquire prefix) -> netids`

*	**`mail-forget`**: Withdraws a service advertisement from the network.

	*	`(mail-forget key)`

*	**`mail-poll`**: Polls a list of mailboxes for pending messages without
	blocking.

	*	`(mail-poll mboxs) -> :nil | idx`

*	**`mail-validate`**: Checks whether a mailbox ID is currently valid and
	alive.

	*	`(mail-validate mbox) -> :t | :nil`

*	**`mail-read`**: Blocks until a message is delivered to a mailbox.

	*	`(mail-read mbox) -> :nil | msg`

*	**`mail-select`**: Blocks on multiple mailboxes, returning the index of the
	first ready mailbox.

	*	`(mail-select mboxs) -> idx`

*	**`mail-send`**: Dispatches a message to a destination `netid`.

	*	`(mail-send mbox obj)`

*	**`mail-timeout`**: Schedules a timeout signal delivered to a mailbox.

	*	`(mail-timeout mbox ns id) -> mbox`

## Task and Kernel Management

Task concurrency, cooperative scheduling, and process spawning.

*	**`kernel-stats`**: Returns kernel telemetry `(task_count mem_used mem_avail max_stack)`.

	*	`(kernel-stats) -> (task_count mem_used mem_avail max_stack)`

*	**`load-path`**: Returns the object binary directory path.

	*	`(load-path) -> path`

*	**`task-flags`**: Returns the flag bitmask of the active task.

	*	`(task-flags) -> flags`

*	**`task-mbox`**: Returns the primary default mailbox `netid` of the active
	task.

	*	`(task-mbox) -> netid`

*	**`task-count`**: Adjusts or queries the task load bias on the current node.

	*	`(task-count bias) -> count`

*	**`task-sleep`**: Cooperatively suspends the task for a duration in
	microseconds (`0` yields execution).

	*	`(task-sleep usec)`

*	**`task-slice`**: Cooperatively yields execution if the time quantum has
	elapsed.

	*	`(task-slice)`

*	**`task-mboxes`**: Allocates an array of `size` disposable mailboxes.

	*	`(task-mboxes size) -> ((task-mbox) [temp_mbox] ...)`

*	**`task-nodeid`**: Extracts the `node_id` component from a mailbox address.

	*	`(task-nodeid [mbox]) -> nodeid`

*	**`task-timeout`**: Calculates platform-scaled timeout values.

	*	`(task-timeout s) -> ns`

*	**`open-task`**: Dispatches a task launch request across the network.

	*	`(open-task task node mode key_num reply)`

*	**`open-child`**: Spawns a child task on the local node.

	*	`(open-child task mode) -> net_id`

*	**`open-remote`**: Spawns a task on a specific remote node.

	*	`(open-remote task node mode) -> net_id`

*	**`open-pipe`**: Spawns a pipeline of interconnected child tasks.

	*	`(open-pipe tasks [modes]) -> ([net_id | 0] ...)`

*	**`jit`**: Triggers JIT compilation of VP source files with locking.

	*	`(jit prefix file products)`

## Platform Implementation Interface (PII)

Direct host operating system primitives provided by the host engine.

*	**`pii-dirlist`**: Reads the contents of a directory on the host file
	system.

	*	`(pii-dirlist path) -> info`

*	**`pii-fstat`**: Retrieves modification time, file size, and mode of a host
	path.

	*	`(pii-fstat path) -> (mtime fsize mode) | :nil`

*	**`pii-read-char` / `pii-write-char`**: Unbuffered character I/O on host
	file descriptors.

	*	`(pii-read-char fd) -> char`

	*	`(pii-write-char fd char) -> char`

*	**`pii-remove`**: Deletes a file on the host file system.

	*	`(pii-remove path) -> num`

*	**`pii-time`**: Reads the host high-resolution monotonic timer in
	nanoseconds.

	*	`(pii-time) -> ns`

## System, Environment, and Utility Functions

File path management, environment introspection, and timing helpers.

*	**`time-it`**: Times the execution of a body of expressions.

	*	`(time-it heading body ...)`

*	**`age`**: Returns the modification timestamp of a file in nanoseconds.

	*	`(age path) -> 0 | time ns`

*	**`path-to-file`**: Returns the enclosing directory path of the active
	source file.

	*	`(path-to-file) -> path`

*	**`path-to-absolute`**: Resolves relative paths against the current file.

	*	`(path-to-absolute target [current]) -> path`

*	**`path-to-relative`**: Converts an absolute file path into a relative path.

	*	`(path-to-relative target [current]) -> path`

*	**`import`**: Loads and evaluates a module within an environment.

	*	`(import module [env])`

*	**`import-from`**: Selectively imports symbols or classes from a module.

	*	`(import-from module [symbols classes])`

*	**`type-to-size`**: Returns byte size for type specifiers.

	*	`(type-to-size sym) -> num`

*	**`time-in-seconds`**: Formats a nanosecond duration as a decimal seconds
	string.

	*	`(time-in-seconds time) -> str`

*	**`lisp-nodes`**: Returns a list of active Lisp cluster node IDs.

	*	`(lisp-nodes) -> nodes`

*	**`os` / `cpu` / `abi`**: Returns target host OS, CPU, and ABI symbols.

	*	`(os) -> sym`

	*	`(cpu) -> sym`

	*	`(abi) -> sym`

*	**`within-compile-env`**: Runs a compiler pass inside an isolated build
	environment.

	*	`(within-compile-env lambda)`