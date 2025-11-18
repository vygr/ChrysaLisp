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
- **Step-by-Step Macro Expansion**: Click "Step Expand" to see macros expand one level at a time
- **Diff View**: Toggle-able diff display showing exactly what changed between phases (READ→EXPAND, EXPAND→BIND)
- **Improved Formatting**: Indented tree view for complex nested structures
- **Memory Address Visualization**: See which symbols are pre-bound to function pointers (O(1) optimization)
- **Export Functionality**: Save all phase results to a timestamped text file
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
- **Process All Button**: Process code through all four phases at once (or press Enter in the input field)
- **Step Expand Button**: Expand macros one level at a time to see intermediate states
- **Export Button**: Save all results to a timestamped file in your home directory
- **Diffs Toggle Button**: Turn diff display ON/OFF to see what changes between phases
- **Clear Button**: Reset all fields
- **Example Buttons**: Load common macro examples (defun, let, case, ui-window)
- **Output Panels**: View each phase of the compilation pipeline with color coding
- **Diff Panels**: See line-by-line differences between phases (when enabled)

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

## Advanced Usage

### Step-by-Step Macro Expansion

The "Step Expand" button allows you to see macro expansion incrementally:

1. Enter a macro-heavy expression (e.g., `(defun add (a b) (+ a b))`)
2. Click "Step Expand" repeatedly
3. Watch each macro level expand one at a time
4. See the message "[Expansion step N]" showing progress
5. Continue until "[No more macros to expand]"

This is invaluable for understanding complex nested macros like UI DSL.

### Memory Address Visualization

The Bind phase output now shows which symbols were pre-bound:

```
[Pre-bound symbols (O(1) optimization):]
  + -> <Func:0x...>
  defq -> <Func:0x...>
  lambda -> <Func:0x...>
```

This demonstrates how ChrysaLisp achieves O(1) function calls by replacing symbol names with direct function pointers during compilation.

### Diff View

The "Diffs: ON/OFF" toggle button controls diff display:

1. With diffs enabled (default), you'll see two diff panels:
   - **Diff: READ → EXPAND** - Shows what the macro expansion changed
   - **Diff: EXPAND → BIND** - Shows which symbols were replaced with function pointers

2. Diff format uses standard notation:
   ```
   - removed line
   + added line
     unchanged line
   ```

3. Perfect for understanding:
   - Exactly what a macro does
   - Which symbols get pre-bound
   - Line-by-line transformation

**Example Diff Output:**
```
[Diff: READ → EXPAND]
- (defun add (a b) (+ a b))
+ (defq add
+   (lambda (a b)
+     (+ a b)))
```

This clearly shows that `defun` is just syntactic sugar that expands to `defq` + `lambda`.

### Export Results

Click "Export" to save a complete analysis to your home directory:
- Filename format: `code-walker-{timestamp}.txt`
- Contains all four phases with headers
- Perfect for documentation or sharing explorations
- Success message shows in the Eval output panel

## Future Enhancements

Potential improvements still to implement:
- Compare multiple expressions side-by-side
- Disassemble to VP instructions
- Tree visualization of AST structure with interactive nodes
- Syntax highlighting within output panels
- History/session management
