# RegexpEngine

## Overview

`RegexpEngine` is an enhanced regular expression engine for ChrysaLisp, inspired by **cl-ppcre** (Common Lisp Portable Perl-Compatible Regular Expressions). It extends the basic `Regexp` class with PCRE-compatible features including named capture groups, lookahead/lookbehind assertions, non-greedy quantifiers, and backreferences.

## Features

### 1. Named Capture Groups
Capture parts of the pattern with meaningful names:
```lisp
(?<name>pattern)    ; Named capture group
```

### 2. Lookahead Assertions
Assert what follows without consuming characters:
```lisp
(?=pattern)         ; Positive lookahead
(?!pattern)         ; Negative lookahead
```

### 3. Lookbehind Assertions
Assert what precedes without consuming characters:
```lisp
(?<=pattern)        ; Positive lookbehind
(?<!pattern)        ; Negative lookbehind
```

### 4. Non-Greedy Quantifiers
Match as few characters as possible:
```lisp
*?                  ; Non-greedy zero or more
+?                  ; Non-greedy one or more
??                  ; Non-greedy zero or one
{n,m}?              ; Non-greedy range
```

### 5. Backreferences
Reference previously captured groups:
```lisp
\1                  ; Reference first capture group
\2                  ; Reference second capture group
```

### 6. Enhanced Character Classes
```lisp
\d                  ; Digits [0-9]
\D                  ; Non-digits
\w                  ; Word characters [A-Za-z0-9_]
\W                  ; Non-word characters
\s                  ; Whitespace [ \t\r\n\f\v]
\S                  ; Non-whitespace
\l                  ; Lowercase [a-z]
\u                  ; Uppercase [A-Z]
\a                  ; Alphabetic [A-Za-z]
\p                  ; Alphanumeric [A-Za-z0-9]
\x                  ; Hexadecimal [A-Fa-f0-9]
```

## Class Hierarchy

```
Search
  └── Regexp
        └── RegexpEngine
```

## Constructor

```lisp
(RegexpEngine [num_buckets]) -> regexp-engine
```

Creates a new RegexpEngine instance with optional hash map bucket size for caching compiled patterns.

**Parameters:**
- `num_buckets` (optional): Size of internal cache hash maps (default implementation-defined)

**Example:**
```lisp
(defq engine (RegexpEngine))
(defq engine-large (RegexpEngine 1024))  ; Larger cache
```

## Methods

### :compile-enhanced

```lisp
(. engine :compile-enhanced pattern) -> ast | :nil
```

Compile a regular expression pattern into an Abstract Syntax Tree (AST) for efficient matching.

**Parameters:**
- `pattern`: String containing the regular expression pattern

**Returns:**
- AST representation of the pattern, or `:nil` if compilation fails

**Example:**
```lisp
(defq engine (RegexpEngine))
(defq ast (. engine :compile-enhanced "(?<word>\\w+)"))
```

### :match-enhanced

```lisp
(. engine :match-enhanced text pattern) -> result | :nil
```

Match text against a pattern using the enhanced engine.

**Parameters:**
- `text`: String to search in
- `pattern`: Regular expression pattern

**Returns:**
- Result list `(success start end captures)` or `:nil` if no match
  - `success`: `:t` if matched
  - `start`: Starting position of match
  - `end`: Ending position of match
  - `captures`: List of captured groups

**Example:**
```lisp
(defq result (. engine :match-enhanced "Hello World" "(?<greeting>\\w+)"))
; Returns: (:t 0 5 ((0 0 5 "Hello")))
```

### :get-named-groups

```lisp
(. engine :get-named-groups) -> fmap
```

Get a map of all named capture groups from the last match.

**Returns:**
- Fmap containing named group captures

**Example:**
```lisp
(. engine :match-enhanced "John Doe" "(?<first>\\w+) (?<last>\\w+)")
(defq groups (. engine :get-named-groups))
; groups contains mappings: "first" -> capture, "last" -> capture
```

### :find-all

```lisp
(. engine :find-all text pattern) -> matches
```

Find all non-overlapping matches of pattern in text.

**Parameters:**
- `text`: String to search in
- `pattern`: Regular expression pattern

**Returns:**
- List of all match results

**Example:**
```lisp
(defq matches (. engine :find-all "cat dog cat" "cat"))
; Returns list of all "cat" matches
```

### :exec-ast

```lisp
(. engine :exec-ast text ast pos) -> result | :nil
```

Execute a compiled AST starting at a specific position. This is a low-level method used internally.

**Parameters:**
- `text`: String to match against
- `ast`: Compiled AST node
- `pos`: Starting position in text

**Returns:**
- Result list or `:nil` if no match

## Pattern Syntax

### Basic Elements

| Pattern | Description | Example |
|---------|-------------|---------|
| `abc` | Literal characters | Matches "abc" |
| `.` | Any character | `a.c` matches "abc", "adc" |
| `\n` | Newline | Matches newline character |
| `\t` | Tab | Matches tab character |
| `\r` | Carriage return | Matches CR character |

### Character Classes

| Pattern | Description | Example |
|---------|-------------|---------|
| `[abc]` | Any of a, b, or c | Matches "a", "b", or "c" |
| `[^abc]` | Not a, b, or c | Matches any char except a, b, c |
| `[a-z]` | Range (lowercase) | Matches any lowercase letter |
| `[A-Z]` | Range (uppercase) | Matches any uppercase letter |
| `[0-9]` | Range (digits) | Matches any digit |
| `\d` | Digit | Equivalent to `[0-9]` |
| `\D` | Non-digit | Equivalent to `[^0-9]` |
| `\w` | Word character | Equivalent to `[A-Za-z0-9_]` |
| `\W` | Non-word character | Equivalent to `[^A-Za-z0-9_]` |
| `\s` | Whitespace | Matches space, tab, newline, etc. |
| `\S` | Non-whitespace | Matches any non-whitespace |

### Anchors

| Pattern | Description | Example |
|---------|-------------|---------|
| `^` | Start of text | `^hello` matches "hello" at start |
| `$` | End of text | `world$` matches "world" at end |
| `\b` | Word boundary | `\bword\b` matches whole word |

### Quantifiers

| Pattern | Description | Example |
|---------|-------------|---------|
| `*` | Zero or more (greedy) | `a*` matches "", "a", "aa", ... |
| `+` | One or more (greedy) | `a+` matches "a", "aa", ... |
| `?` | Zero or one (greedy) | `a?` matches "", "a" |
| `{n}` | Exactly n times | `a{3}` matches "aaa" |
| `{n,}` | At least n times | `a{2,}` matches "aa", "aaa", ... |
| `{n,m}` | Between n and m times | `a{2,4}` matches "aa", "aaa", "aaaa" |
| `*?` | Zero or more (non-greedy) | Matches as few as possible |
| `+?` | One or more (non-greedy) | Matches as few as possible |
| `??` | Zero or one (non-greedy) | Matches as few as possible |
| `{n,m}?` | Range (non-greedy) | Matches as few as possible |

### Groups

| Pattern | Description | Example |
|---------|-------------|---------|
| `(...)` | Capturing group | `(abc)` captures "abc" |
| `(?:...)` | Non-capturing group | `(?:abc)` matches but doesn't capture |
| `(?<name>...)` | Named capture group | `(?<id>\d+)` captures as "id" |

### Alternation

| Pattern | Description | Example |
|---------|-------------|---------|
| `\|` | Or | `cat\|dog` matches "cat" or "dog" |

### Assertions

| Pattern | Description | Example |
|---------|-------------|---------|
| `(?=...)` | Positive lookahead | `foo(?=bar)` matches "foo" before "bar" |
| `(?!...)` | Negative lookahead | `foo(?!bar)` matches "foo" not before "bar" |
| `(?<=...)` | Positive lookbehind | `(?<=foo)bar` matches "bar" after "foo" |
| `(?<!...)` | Negative lookbehind | `(?<!foo)bar` matches "bar" not after "foo" |

### Backreferences

| Pattern | Description | Example |
|---------|-------------|---------|
| `\1`, `\2`, ... | Reference captured group | `(\w+) \1` matches repeated words |

## Examples

### Basic Matching

```lisp
(defq engine (RegexpEngine))

; Simple literal match
(. engine :match-enhanced "Hello World" "World")
; Returns: (:t 6 11 ())

; Digit matching
(. engine :match-enhanced "Price: $25" "\\d+")
; Returns: (:t 8 10 ())
```

### Email Validation

```lisp
(defq email-pattern "\\w+@\\w+\\.\\w+")
(. engine :match-enhanced "user@example.com" email-pattern)
```

### Named Capture Groups

```lisp
(defq name-pattern "(?<first>\\w+) (?<last>\\w+)")
(. engine :match-enhanced "John Doe" name-pattern)
(defq groups (. engine :get-named-groups))
; Access named groups from 'groups' map
```

### URL Parsing

```lisp
(defq url-pattern "(?<protocol>https?)://(?<domain>[\\w.]+)/(?<path>\\w+)")
(. engine :match-enhanced "https://example.com/path" url-pattern)
```

### Phone Number

```lisp
(defq phone-pattern "\\(?\\d{3}\\)?[- ]?\\d{3}[- ]?\\d{4}")
(. engine :match-enhanced "(555) 123-4567" phone-pattern)
```

### Date Validation

```lisp
(defq date-pattern "(?<year>\\d{4})-(?<month>\\d{2})-(?<day>\\d{2})")
(. engine :match-enhanced "2024-12-25" date-pattern)
```

### Finding All Matches

```lisp
(defq text "The cat and the dog and the bird")
(defq matches (. engine :find-all text "\\w+"))
; Returns all words
```

### Lookahead Example

```lisp
; Find "foo" only when followed by "bar"
(. engine :match-enhanced "foobar" "foo(?=bar)")
; Matches "foo"

; Find "foo" only when NOT followed by "bar"
(. engine :match-enhanced "foobaz" "foo(?!bar)")
; Matches "foo"
```

### Backreference Example

```lisp
; Match repeated words
(. engine :match-enhanced "hello hello" "(\\w+) \\1")
; Matches "hello hello"
```

### Non-Greedy Quantifiers

```lisp
; Greedy match (matches most)
(. engine :match-enhanced "<tag>content</tag>" "<.+>")
; Matches entire "<tag>content</tag>"

; Non-greedy match (matches least)
(. engine :match-enhanced "<tag>content</tag>" "<.+?>")
; Matches only "<tag>"
```

## Performance Considerations

### Pattern Caching
- Compiled patterns are automatically cached
- Reusing the same `RegexpEngine` instance for multiple matches improves performance
- The cache size can be configured via the constructor

### Optimization
- Consecutive literal characters are optimized into single string matches
- Character class matching uses efficient binary search when possible
- The AST-based approach enables advanced optimizations

### Best Practices
1. Reuse `RegexpEngine` instances when matching multiple patterns
2. Pre-compile frequently used patterns
3. Use non-capturing groups `(?:...)` when capture is not needed
4. Prefer character classes over complex alternations

## Differences from Basic Regexp

The `RegexpEngine` class extends `Regexp` with:

1. **AST-based compilation** - More flexible and extensible
2. **Named capture groups** - Not available in basic Regexp
3. **Lookahead/lookbehind** - Not available in basic Regexp
4. **Non-greedy quantifiers** - Not available in basic Regexp
5. **Backreferences** - Not available in basic Regexp
6. **Enhanced optimization** - Better performance for complex patterns

## Limitations

Current limitations (may be addressed in future versions):

1. Lookbehind is simplified and may not handle all cases
2. Backreferences are partially implemented
3. Unicode support is limited to the underlying character encoding
4. Some advanced PCRE features (e.g., atomic groups, possessive quantifiers) are not yet implemented

## See Also

- [Regexp](Regexp.md) - Basic regular expression class
- [Search](Search.md) - Abstract search interface
- Text Processing Guide
- Pattern Matching Best Practices

## Testing

A comprehensive test suite is available:

```bash
./run_tui.sh cmd/test_regexp_engine.lisp
```

The test suite covers:
- Basic literal matching
- Character classes
- Anchors
- Quantifiers (greedy and non-greedy)
- Capturing and named groups
- Alternation
- Lookahead and lookbehind
- Backreferences
- Complex real-world patterns
- Edge cases

## License

Part of the ChrysaLisp project. See project LICENSE file.
