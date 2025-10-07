# Parsing Text in ChrysaLisp: A High-Performance Approach

In ChrysaLisp, text parsing is not an afterthought but a direct reflection of
its core philosophies: simplicity, efficiency, and performance. Instead of
relying on complex regex engines (which are available via the Regexp class !)
for common tasks, the system provides a suite of powerful yet simple functions
that operate on strings and special strings called `char-class`. This approach,
centered around binary searches and sequential skipping, allows for remarkably
fast and memory-efficient text manipulation.

## The Foundation: `char-class`

At the heart of ChrysaLisp's parsing toolkit is the `char-class`. It is not a
complex object but simply a string containing unique characters sorted in
ascending order. This structure is optimized for use with the `bfind` and
`bskip` family of functions, which leverage its sorted nature to perform binary
searches.

The `char-class` function simplifies their creation. For example, to define a
class for whitespace or digits:

```vdu
(defq +char_class_space (char-class " \t"))
(defq +char_class_digit (char-class "0-9"))
```

The `char-class` function expands ranges (like `"0-9"`) and handles sorting and
ensuring uniqueness, producing an optimized string ready for searching.

## Core Parsing Functions

**`bfind` (Binary Find)**

This function is the fundamental building block. It performs a binary search for
a character within a `char-class`.

*   **Signature:** `(bfind char cls) -> :nil | idx`

*   **Action:** If the character `char` exists in the `char-class` string `cls`,
    it returns its index; otherwise, it returns `:nil`. Because of its binary
    search nature, it is significantly faster than a linear scan for determining
    character membership in a set.

**The `bskip` Family (Forward Skipping)**

These functions iterate forward through a string from a given index.

*   **`bskip`**: Skips characters that **are** in the `char-class`. It returns
    the index of the first character *not* found in the class. This is ideal for
    consuming leading characters, like whitespace.

    * **Signature:** `(bskip cls str idx) -> idx`

*   **`bskipn`**: The inverse of `bskip`. It skips characters that **are not**
    in the `char-class`. It returns the index of the first character that *is*
    found in the class. This is useful for finding the beginning of a token or
    delimiter.

    * **Signature:** `(bskipn cls str idx) -> idx`

**The `rbskip` Family (Reverse Skipping)**

These functions perform the same logic as their forward counterparts but search
backward from a given index. A key design feature is that the returned index is
"slice-compatible," meaning it can be used directly as the `end` parameter in a
`slice` operation without needing manual adjustments.

*   **`rbskip`**: Searches backward from an index to find the first character
    *not* in the `char-class`. This is perfect for trimming trailing characters.

    * **Signature:** `(rbskip cls str idx) -> idx`

*   **`rbskipn`**: Searches backward from an index to find the first character
    *in* the `char-class`.

    * **Signature:** `(rbskipn cls str idx) -> idx`

## Practical Examples from ChrysaLisp's Source

The elegance of this system is best demonstrated by how these simple primitives
are composed to build robust, high-level parsing logic.

**Example 1: The `trim` and `split` Functions**

These fundamental string utilities are implemented concisely using the `bskip`
family.

*   **`trim-start`**: Uses `bskip` to find the index of the first non-whitespace
    character and returns a slice from that point.

    ```vdu
    (defun trim-start (s &optional cls)
        ; (trim-start str [cls]) -> str
        (slice s (bskip (ifn cls " ") s 0) -1))
    ```

*   **`trim-end`**: Uses `rbskip` to find the index of the last non-whitespace
    character and returns a slice up to that point.

    ```vdu
    (defun trim-end (s &optional cls)
        ; (trim-end str [cls]) -> str
        (slice s 0 (rbskip (ifn cls " ") s -1)))
    ```

*   **`trim`**: This function perfectly combines the forward and reverse skip
    functions into a single, elegant line. It uses `bskip` for the `start` index
    of the slice and `rbskip` for the `end` index.

    ```vdu
    (defun trim (s &optional cls)
        ; (trim str [cls]) -> str
        (slice s (bskip (setd cls " ") s 0) (rbskip cls s -1)))
    ```

*   **`split`**: This function masterfully combines `bskip` and `bskipn` to
    break a string into a list of words, demonstrating a common parsing loop.

    ```vdu
    (defun split (s &optional cls)
        ; (split str [cls]) -> strs
        (defq i 0 out (list) l (length s) cls (ifn cls " "))
        (while (< i l)
            ; Skip any delimiters at the current position
            (if (/= (defq j (bskip cls s i)) i) (setq i j))
            ; Find the next delimiter to mark the end of the word
            (if (/= (defq j (bskipn cls s i)) i) 
                (push out (slice s i (setq i j))))) 
        out)
    ```

**Example 2: Syntax Highlighting State Machine**

The syntax highlighter uses `bfind` to quickly determine a character's category
and decide the parser's next state. This avoids complex, multi-character
lookaheads.

```vdu
; From lib/text/syntax.inc

(defq +char_class_not_text (char-class "\q:;{"))

(defun is-text-char? (_)
	(cond
		((defq i (bfind _ +char_class_not_text))
			(elem-get '(:string1 :keysym :comment :string2) i))
		((is-digit-char? _) :number)
		((is-symbol-char? _) :symbol)
		(:text)))
```

Here, `bfind` instantly checks if a character is a quote, colon, semicolon, or
brace, returning a new state symbol if it is. If not, it proceeds to check other
character classes.

**Example 3: Command Line Options Parsing**

The `options-split` function in the options library uses `bskip` and `bskipn` to
tokenize the command line, correctly handling whitespace and quoted arguments.

```vdu
; From lib/options/options.inc

(defun options-split (args)
	; (options-split args) -> (a0 [a1] ...)
	(defq out (list) i 0 state :space)
	(while (< i (length args))
		(case state
			(:space
				; Consume whitespace
				(when (< (setq i (bskip +char_class_white_space args i)) (length args))
					(if (eql (elem-get args i) (ascii-char 34))
						(setq i (inc i) state :quote)
						(setq state :normal))))
			(:normal
				; Find the end of a non-quoted argument
				(push out (slice args i (setq state :space i
					(bskipn +char_class_white_space args i)))))
			(:quote
				; Find the closing quote
				(push out (slice args i (setq i (bskipn (ascii-char 34) args i))))
				(setq i (inc i) state :space)))) 
    out)
```

By composing these fast, low-level primitives, ChrysaLisp achieves powerful and
efficient text parsing without the overhead of a traditional regex engine,
staying true to its core design principles.
