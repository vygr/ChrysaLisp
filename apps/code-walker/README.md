# Code Walker / AST Explorer

A visual tool for exploring ChrysaLisp's REPL compilation pipeline: Read → Expand → Bind → Eval.

## Overview

The Code Walker is an interactive application that demonstrates how ChrysaLisp processes code through its multi-phase compilation pipeline. It's an educational tool for understanding:

- **Phase 1: READ** - How source code is parsed into an Abstract Syntax Tree (AST)
- **Phase 2: EXPAND** - How macros are expanded into primitive forms
- **Phase 3: BIND** - How symbols are pre-bound to function addresses (O(1) optimization)
- **Phase 4: EVAL** - How the final form is evaluated to produce a result

## Features

- **Live Code Processing**: Enter any ChrysaLisp expression and see it transform through each phase
- **Example Library**: Quick-load common patterns (defun, let, case, ui-window)
- **Error Handling**: Safe processing with clear error messages at each phase
- **Visual Output**: Color-coded phases with formatted output

## Usage

### From ChrysaLisp GUI

1. Launch ChrysaLisp GUI: `./run.sh`
2. Open the launcher and find "Code Walker"
3. Or run from terminal: `(open-child "apps/code-walker/app.lisp")`

### Interface

- **Input Field**: Enter any Lisp expression
- **Process Button**: Manually process the code (or press Enter in the input field)
- **Clear Button**: Reset all fields
- **Example Buttons**: Load common macro examples
- **Output Panels**: View each phase of the compilation pipeline

## Example Explorations

### 1. Macro Expansion (defun)

Input: `(defun add (a b) (+ a b))`

- **Read**: Parses into nested list structure
- **Expand**: Shows how `defun` macro expands to `(defq add (lambda ...))`
- **Bind**: Pre-binds the `+` function and other symbols
- **Eval**: Creates the function and returns it

### 2. Lexical Binding (let)

Input: `(let ((x 10) (y 20)) (+ x y))`

- **Read**: Parses the let form
- **Expand**: Shows how `let` creates a lambda and applies it
- **Bind**: Pre-binds functions
- **Eval**: Executes and returns 30

### 3. Pattern Matching (case)

Input: `(case 2 (0 :zero) (1 :one) (2 :two) (:t :other))`

- **Read**: Parses the case form
- **Expand**: Shows how `case` compiles to efficient key/value lookup
- **Bind**: Pre-binds the lookup functions
- **Eval**: Returns :two

### 4. UI DSL (ui-window)

Input: `(ui-window w () (ui-label _ (:text "Hello")))`

- **Read**: Parses the nested UI structure
- **Expand**: Shows multi-level macro expansion of UI DSL
- **Bind**: Pre-binds UI construction functions
- **Eval**: Creates the actual GUI object hierarchy

## Technical Details

### The REPL Pipeline

ChrysaLisp's REPL is not just an interactive shell—it's a JIT compiler that processes every form through multiple optimization passes:

1. **Read (`lisp :read`)**: Iterative (not recursive) parser that builds an AST from character stream
2. **Expand (`lisp :repl_expand`)**: Top-down, depth-first macro expansion pass
3. **Bind (`lisp :repl_bind`)**: Symbol pre-binding pass that replaces symbols with direct function pointers
4. **Eval (`lisp :repl_eval`)**: The actual execution phase, optimized by previous passes

### Zero-Cost Abstractions

The Expand phase demonstrates ChrysaLisp's "zero-cost abstractions" principle:
- Macro work is done once at compile-time
- Runtime sees only the expanded, primitive forms
- High-level syntax (like `defun`, `let`, `case`) has no runtime overhead

### O(1) Function Calls

The Bind phase demonstrates ChrysaLisp's O(1) function call optimization:
- Function symbols are replaced with direct memory addresses
- No hash map lookup at runtime
- Direct jump to machine code

## Educational Value

This tool is invaluable for:
- **Learning ChrysaLisp**: See exactly how your code transforms
- **Macro Development**: Debug macro expansions visually
- **Performance Understanding**: Observe pre-binding optimizations
- **Language Design**: Study a sophisticated Lisp implementation

## Implementation Notes

The Code Walker uses:
- `read` - To parse code from string
- `macroexpand` - To expand macros
- `prebind` - To pre-bind symbols
- `eval` - To evaluate final form
- `catch` - For safe error handling at each phase

All processing is done safely within `catch` blocks to prevent crashes from invalid input.

## Related Documentation

For deeper understanding, see:
- `docs/ai_digest/inner_thoughts.md` - The REPL pipeline explained
- `docs/ai_digest/modern_lisp.md` - ChrysaLisp's Lisp dialect
- `docs/ai_digest/art_of_the_call.md` - O(1) optimization details

## Future Enhancements

Potential improvements:
- Syntax highlighting for each phase
- Step-by-step macro expansion animation
- Export results to file
- Compare multiple expressions side-by-side
- Show memory addresses in Bind phase
- Disassemble to VP instructions
