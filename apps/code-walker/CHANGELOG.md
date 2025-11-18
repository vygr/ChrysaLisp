# Code Walker / AST Explorer - Changelog

## Version 3.0 - Diff View (Current)

**Date:** 2025-11-18

### New Feature: Diff View Highlighting

**Revolutionary feature for understanding transformations!**

#### Diff Display System
- **Toggle Button:** "Diffs: ON/OFF" to control diff visibility
- **Two Diff Panels:**
  - READ → EXPAND diff (shows macro transformations)
  - EXPAND → BIND diff (shows symbol pre-binding)
- **Standard Notation:** `- removed`, `+ added`, `  unchanged`

#### Implementation
- `compute-diff()` - Line-by-line comparison of forms
- `split()` - String splitting helper
- `update-diffs()` - Refresh diff displays
- `toggle-diffs()` - Toggle diff visibility
- State tracking: `*last_read*`, `*last_expand*`, `*last_bind*`

#### Educational Impact
Makes it crystal clear:
- What each compilation phase actually does
- How macros transform code
- Which symbols get pre-bound for O(1) optimization
- Line-by-line changes between phases

#### Example Output
```
[Diff: READ → EXPAND]
- (defun add (a b) (+ a b))
+ (defq add
+   (lambda (a b)
+     (+ a b)))
```

Shows instantly that `defun` is syntactic sugar for `defq` + `lambda`!

---

## Version 2.0 - Enhanced Features

**Date:** 2025-11-18

### New Features

#### 1. Step-by-Step Macro Expansion
- **Button:** "Step Expand" button for incremental macro expansion
- **Functionality:** Expands macros one level at a time
- **UI Feedback:** Shows expansion step counter
- **Educational Value:** Perfect for understanding complex nested macros
- **Implementation:** `expand-one-level()` function performs single-level expansion

#### 2. Improved Pretty-Printing with Indentation
- **Smart Formatting:** Short lists on one line, long lists indented
- **Nested Structures:** Proper indentation for complex forms
- **Readability:** Makes AST exploration much easier
- **Implementation:** Enhanced `pretty-print()` function with optional indent parameter

#### 3. Export Functionality
- **Button:** "Export" button saves all results to file
- **File Format:** Timestamped text files (`code-walker-{timestamp}.txt`)
- **Content:** All four phases with formatted headers
- **Location:** Files saved to user's home directory
- **Feedback:** Success message in EVAL output panel
- **Implementation:** `export-results()` function

#### 4. Memory Address Visualization
- **Feature:** Shows which symbols were pre-bound in BIND phase
- **Educational:** Demonstrates O(1) function call optimization
- **Format:** Lists symbol → function address mappings
- **Example:**
  ```
  [Pre-bound symbols (O(1) optimization):]
    + -> <Func:0x...>
    defq -> <Func:0x...>
  ```
- **Implementation:** `analyze-bindings()` function recursively tracks bindings

### Enhanced UI

- **Process All:** Renamed from "Process" for clarity
- **Step Expand:** New button for incremental expansion
- **Export:** New button for saving results
- **Clear:** Resets all fields including expansion state

### Documentation

- **README.md:** Updated with all new features and advanced usage guide
- **TESTING.md:** Comprehensive testing checklist per CONTRIBUTING.md
- **CHANGELOG.md:** This file tracking all changes

### Technical Improvements

- Better error handling in all phases
- State management for step-by-step expansion
- Recursive binding analysis
- Improved layout updates

## Version 1.0 - Initial Release

**Date:** 2025-11-18

### Core Features

#### Phase Visualization
- **Phase 1: READ** - Parse code to AST
- **Phase 2: EXPAND** - Macro expansion
- **Phase 3: BIND** - Symbol pre-binding
- **Phase 4: EVAL** - Evaluation result

#### Interactive UI
- Input field for entering Lisp expressions
- Process button for full pipeline execution
- Clear button for resetting
- Four example buttons (defun, let, case, ui-window)
- Color-coded output panels

#### Safe Processing
- Error handling at each phase
- Catch blocks prevent crashes
- Clear error messages

### Implementation Details

#### Functions
- `safe-read()` - Parse with error handling
- `safe-macroexpand()` - Expand with error handling
- `safe-prebind()` - Prebind with error handling
- `safe-eval()` - Evaluate with error handling
- `pretty-print()` - Format output
- `process-code()` - Main processing pipeline
- `clear-all()` - Reset UI state
- `load-example()` - Load example code

#### UI Components
- Window with title bar
- Input section with label and textfield
- Button section with action buttons
- Example buttons section
- Scrollable output section with four phase panels

### Files
- `app.lisp` - Main application (261 lines)
- `README.md` - User documentation
- `TESTING.md` - Testing guide
- `CHANGELOG.md` - This file

## Future Enhancements

Planned features for future versions:
- Compare multiple expressions side-by-side
- VP instruction disassembly view
- Diff view highlighting changes between phases
- Tree visualization of AST structure
- Syntax highlighting within output panels
- Save/load workspace sessions
- History of processed expressions
- Interactive AST tree browser
- Integration with debugger

## Testing Status

✅ All features implemented per requirements
✅ Follows ChrysaLisp coding conventions
✅ Comprehensive testing documentation created
✅ No modifications to core system files
✅ Pure Lisp application - no platform-specific code

## Commits

- `6497914` - Add Code Walker/AST Explorer application
- `0c8cba2` - Enhance Code Walker with advanced features
- `5a86f31` - Add comprehensive testing documentation for Code Walker

## Branch

`claude/code-walker-ast-explorer-016dTD4JasQkL7BLW4SbsRnN`

---

*This application demonstrates ChrysaLisp's sophisticated REPL pipeline and serves as an invaluable educational tool for understanding macro expansion, symbol pre-binding, and the compilation process.*
