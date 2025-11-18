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
- **Comparison Mode**: Compare two expressions side-by-side across all phases
- **Step-by-Step Macro Expansion**: Click "Step Expand" to see macros expand one level at a time
- **Diff View**: Toggle-able diff display showing exactly what changed between phases (READ→EXPAND, EXPAND→BIND)
- **Tree Visualization**: ASCII tree view showing hierarchical AST structure with node types and counts
- **Improved Formatting**: Indented s-expression view for complex nested structures
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

- **Input Fields**:
  - Expression A: Always visible
  - Expression B: Appears when Compare mode is ON
- **Process All Button**: Process code through all four phases at once (or press Enter in the input field)
- **Step Expand Button**: Expand macros one level at a time to see intermediate states
- **Export Button**: Save all results to a timestamped file in your home directory
- **Diffs Toggle Button**: Turn diff display ON/OFF to see what changes between phases
- **Tree Toggle Button**: Turn tree visualization ON/OFF to see AST structure
- **Compare Toggle Button**: Turn comparison mode ON/OFF to compare two expressions
- **Clear Button**: Reset all fields
- **Example Buttons**: Load common macro examples (defun, let, case, ui-window)
- **Output Panels**: View each phase of the compilation pipeline with color coding
- **Diff Panels**: See line-by-line differences between phases (when enabled)
- **Tree Panels**: See ASCII tree visualization of AST structure (when enabled)

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

### Tree Visualization

The "Tree: ON/OFF" toggle button controls ASCII tree display:

1. With tree view enabled, you'll see hierarchical structure for each phase:
   - **READ phase**: Original parsed AST structure
   - **EXPAND phase**: Structure after macro expansion
   - **BIND phase**: Structure with pre-bound functions

2. Tree format uses ASCII box-drawing characters:
   ```
   └── ( [3 items]
       ├── sym: defun
       ├── sym: add
       └── ( [2 items]
           ├── sym: a
           └── sym: b
           )
   ```

3. Shows node types:
   - `sym:` for symbols
   - `num:` for numbers
   - `str:` for strings
   - `func:` for function objects (after binding)
   - `( [N items]` for lists

4. Perfect for:
   - Understanding AST structure visually
   - Seeing nested list depth
   - Counting items in each level
   - Comparing structure changes between phases

**Example Tree Output:**
```
[Tree Structure]
└── ( [4 items]
    ├── sym: defq
    ├── sym: add
    ├── ( [3 items]
    │   ├── sym: lambda
    │   ├── ( [2 items]
    │   │   ├── sym: a
    │   │   └── sym: b
    │   │       )
    │   └── ( [3 items]
    │       ├── func: <Func:0x...>
    │       ├── sym: a
    │       └── sym: b
    │           )
    │       )
    └── ...
```

The tree makes nested structure immediately clear and shows how lists are composed.

### Comparison Mode

The "Compare: ON/OFF" toggle button enables side-by-side expression comparison:

1. Click "Compare: ON" to enable comparison mode
2. Expression B input field appears
3. Enter two different expressions to compare
4. Click "Process All" to see them compared

**Comparison Output Format:**
```
>>> Expression A <<<
(defun add (a b) (+ a b))

>>> Expression B <<<
(defun mul (x y) (* x y))
```

Each phase shows both expressions labeled for easy identification.

**Perfect for:**
- Comparing different macro implementations
- Understanding how similar expressions expand differently
- Testing variations of code
- Educational demonstrations
- Before/after refactoring comparisons

**Example Comparison:**
- **Expression A:** `(when (> x 0) (print x))`
- **Expression B:** `(if (> x 0) (print x))`

The EXPAND phase will show that `when` expands to `if`, revealing they're equivalent!

### Export Results

Click "Export" to save a complete analysis to your home directory:
- Filename format: `code-walker-{timestamp}.txt`
- Contains all four phases with headers
- Perfect for documentation or sharing explorations
- Success message shows in the Eval output panel

## Future Enhancements

Potential improvements still to implement:
- Disassemble to VP instructions
- Interactive tree nodes (expand/collapse)
- Per-word syntax highlighting within output panels
- History/session management
- Save/load workspaces
- Search/filter within results
