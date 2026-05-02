# The Regular Expression System: NFA-Based Pattern Matching

While ChrysaLisp emphasizes high-performance text parsing through simple binary
search primitives (`bfind`, `bskip`), it also provides a robust and efficient
Regular Expression engine. Implemented as the `Regexp` class, this system is
used for complex pattern matching, global search, and sophisticated text
transformations in tools like `grep`, `sed`, and the `Editor` application.

## Architecture: The NFA Engine

The ChrysaLisp Regexp engine is a Non-deterministic Finite Automaton (NFA)
implementation. It prioritizes correctness and predictable performance over the
complex, backtracking-heavy engines found in many other environments.

The process follows a classic three-stage pipeline:

1. **Tokenization:** The pattern is broken into literals, operators, and
   character classes.

2. **Postfix Conversion:** The tokens are rearranged into postfix notation
   (Reverse Polish Notation) to handle operator precedence correctly.

3. **Compilation:** The postfix expression is compiled into a list of NFA
   instructions (meta-data), which represents the state machine.

### NFA Instructions

The compiled "meta" object consists of instructions like:

* `(:lit char)`: Match a literal character.

* `(:wild .)`: Match any character.

* `(:class string)`: Match a character from an optimized `char-class`.

* `(:nil offset1 [offset2])`: A non-consuming split or jump instruction used to
  implement loops, branches, and quantifiers.

* `(:assert type)`: Anchor matches like `^` (start), `$` (end), or `!` (word
  boundary).

## Quantifiers: Greedy and Lazy

ChrysaLisp supports both greedy (matching as much as possible) and lazy
(matching as little as possible) quantifiers.

* **Greedy:** `*` (zero or more), `+` (one or more), `?` (zero or one).

* **Lazy:** `*?`, `+?`, `??`.

The lazy behavior is achieved by the order of offsets in the `(:nil ...)` split
instructions during compilation. By swapping the priority of the loop vs. the
exit branch in the NFA, the engine naturally finds the shortest or longest
match.

## Submatches and Capture Groups

Capture groups `(...)` are supported and automatically tracked during the
search. The results of a match include not just the full match bounds, but a
list of all submatches (capture groups).

```vdu
(. regexp :search "2026-02-10" "(\\d+)-(\\d+)-(\\d+)")
; Returns a list of matches, each containing:
; ((0 10) (0 4) (5 7) (8 10)) 
;  ^full  ^$1   ^$2   ^$3
```

These submatches are essential for the `replace-regex` functionality, where they
can be referenced in the replacement string as `$0`, `$1`, etc.

## Global Integration: `searching.inc`

For convenience, the Regexp functionality is wrapped in a set of global
functions in `lib/text/searching.inc`. These functions manage a global `Regexp`
instance (`+regexp`) and provide a simplified API:

* **`match? (text pattern)`**: Returns `:t` if the pattern matches anywhere.

* **`matches (text pattern)`**: Returns a list of all matches (including
  submatches).

* **`replace-regex (text pattern replacement)`**: Performs global search and
  replace.

### Optimized Replacement: The `splice` Primitive

Regexp replacement in ChrysaLisp is extremely efficient. The system
pre-compiles the replacement string into a "template" and then uses the
built-in `splice` primitive to perform the entire transformation in a
single, atomic operation. This avoids repeated string allocations and
copying.

## Performance Considerations

The engine includes several optimizations:

1. **Fast Path Scanning:** Before running the full NFA, the `match?` and
   `search` methods check if the pattern starts with a literal or a character
   class. If so, they use the high-speed `bskipn` or `bskip` primitives to
   rapidly skip over text that cannot possibly match.

2. **Meta-Data Caching:** Compiled NFA objects are cached in an `Fmap` keyed by
   the pattern string, ensuring that the compilation step only happens once per
   unique pattern.

3. **Graph Element Caching:** During compilation, individual NFA segments are
   memoized to reduce object churn and memory usage.

By combining the theoretical elegance of NFAs with the practical speed of
ChrysaLisp's string primitives, the Regexp system provides a powerful tool for
complex text processing that remains consistent with the system's overall
performance goals.
