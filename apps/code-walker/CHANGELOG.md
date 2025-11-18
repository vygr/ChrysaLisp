# Code Walker / AST Explorer - Changelog

## Version 7.0 - Session Save/Load (Current)

**Date:** 2025-11-18

### New Feature: Session Persistence

**Save and load complete exploration sessions!**

#### Session System
- **Save Session Button:** Saves complete session to timestamped file
- **Load Session Button:** Loads previously saved session
- **File Format:** `.cws` (Code Walker Session) text format
- **Complete State:** Saves all inputs, outputs, history, and toggle states
- **Session Files:** Stored in home directory as `code-walker-session-{timestamp}.cws`

#### What's Saved in a Session
- **Toggle States:** Diffs ON/OFF, Tree ON/OFF, Compare ON/OFF
- **Current Inputs:** Both Expression A and Expression B text
- **Current Outputs:** All 4 phase outputs (READ, EXPAND, BIND, EVAL)
- **Complete History:** All history entries (up to 50) with full state
- **History Position:** Current index in history

#### Implementation
- `save-session()` - Writes complete session to file (52 lines)
- `load-session()` - Reads and restores session (107 lines)
- File format: Sectioned text format with markers
  - `[TOGGLES]` - Toggle state section
  - `[CURRENT_INPUT]` - Current expression inputs
  - `[CURRENT_OUTPUT]` - Current phase outputs
  - `[HISTORY]` - All history entries
- Error handling with catch blocks for invalid files
- Success/error messages in EVAL output

#### File Format
```
CODE_WALKER_SESSION_V1

[TOGGLES]
:t
:nil
:t

[CURRENT_INPUT]
(defun add (a b) (+ a b))
(defun mul (x y) (* x y))

[CURRENT_OUTPUT]
...phase outputs with ---PHASE--- separators...

[HISTORY]
5
2
---ENTRY---
...entry fields with ---FIELD--- separators...
```

#### Use Cases
- **Save Progress:** Save exploration session for later
- **Share Sessions:** Share complete analysis with colleagues
- **Teaching:** Create curated exploration sessions for students
- **Documentation:** Save examples for documentation
- **Experimentation:** Save state before trying risky changes
- **Resume Work:** Pick up where you left off across app restarts

#### Workflow Example
```
1. Explore several macros (defun, let, case)
2. Navigate through history reviewing them
3. Click "Save Session" - saves to code-walker-session-123456789.cws
4. Close Code Walker
5. Rename file to code-walker-session-latest.cws
6. Later: Open Code Walker, click "Load Session"
7. Complete state restored - continue where you left off!
```

**Note:** Load Session currently loads from `code-walker-session-latest.cws` in home directory. Save Session creates timestamped files - rename desired session to "latest" to load it.

**Future Enhancement:** File browser UI for selecting which session to load.

---

## Version 6.0 - History Navigation

**Date:** 2025-11-18

### New Feature: Exploration History with Navigation

**Navigate through your code exploration history!**

#### History System
- **History Label:** Shows current position "History: N/M" (e.g., "History: 3/10")
- **◄ Prev Button:** Navigate backward through history
- **Next ► Button:** Navigate forward through history
- **Clear History Button:** Clear all history entries
- **Automatic Saving:** Each successful processing saves to history
- **Smart Truncation:** When navigating back and processing new code, forward history is cleared

#### Implementation
- `save-to-history()` - Saves complete state after each processing (30 lines)
- `load-from-history()` - Restores complete state from history entry (35 lines)
- `history-prev()` - Navigate to previous entry (4 lines)
- `history-next()` - Navigate to next entry (4 lines)
- `clear-history()` - Clear all history (3 lines)
- `update-history-label()` - Update position display (7 lines)
- History storage: `*history*` list with max 50 entries
- Each entry stores: both input fields, compare mode, all 4 phase outputs

#### State Preservation
Each history entry captures complete exploration state:
- Expression A text
- Expression B text (for comparison mode)
- Compare mode ON/OFF
- All 4 phase outputs (READ, EXPAND, BIND, EVAL)

#### Usage Flow
1. Process several different expressions
2. Click "◄ Prev" to review earlier explorations
3. Click "Next ►" to move forward
4. At any point in history, can process new code
5. History position shows where you are: "History: 3/10"

#### Educational Impact
Perfect for learning workflows:
- Compare current exploration with previous attempts
- Review how understanding evolved
- Quick access to working examples
- Non-linear exploration - jump back and forth
- Save interesting discoveries without export
- Build up a session of related explorations

**Example Session:**
```
1. Explore (defun add (a b) (+ a b))          <- History: 1/5
2. Explore (let ((x 10)) (* x 2))             <- History: 2/5
3. Explore (case n (0 :zero) (1 :one))        <- History: 3/5
4. Click ◄ Prev to review defun expansion    <- Back to: History: 1/5
5. Click Next ► twice to jump to case        <- Forward to: History: 3/5
```

**Integration with Other Features:**
- Works seamlessly with comparison mode (saves both expressions)
- Preserves diff view state
- Preserves tree visualization state
- Clear button also clears history

---

## Version 5.0 - Comparison Mode

**Date:** 2025-11-18

### New Feature: Side-by-Side Expression Comparison

**Compare two expressions across all compilation phases!**

#### Comparison System
- **Toggle Button:** "Compare: ON/OFF" to enable comparison mode
- **Dual Input Fields:** Expression A (always visible) and Expression B (when compare is ON)
- **Side-by-Side Output:** Both expressions shown in each phase with clear labels
- **Automatic Processing:** Processes both expressions when clicking "Process All"

#### Implementation
- `process-compare()` - Processes both expressions in parallel (58 lines)
- `toggle-compare()` - Toggles comparison mode and shows/hides second input (10 lines)
- Dual state tracking: `*last_read2*`, `*last_expand2*`, `*last_bind2*`
- Dynamic UI visibility for second input field

#### Example Output
```
>>> Expression A <<<
(defun add (a b) (+ a b))

>>> Expression B <<<
(defun mul (x y) (* x y))
```

#### Educational Impact
Perfect for understanding differences:
- Compare macro implementations (when vs if, defun vs defmacro)
- See how similar expressions expand differently
- Test code variations side-by-side
- Before/after refactoring comparisons
- Educational demonstrations of equivalent forms

---

## Version 4.0 - Tree Visualization

**Date:** 2025-11-18

### New Feature: ASCII Tree Visualization

**Visual AST structure exploration!**

#### Tree Display System
- **Toggle Button:** "Tree: ON/OFF" to control tree visualization
- **Three Tree Panels:** One for each major phase (READ, EXPAND, BIND)
- **ASCII Box Drawing:** Uses `├──`, `└──`, `│` for clear hierarchy
- **Node Type Labels:** Shows `sym:`, `num:`, `str:`, `func:` for different types
- **Item Counts:** Displays `[N items]` for lists

#### Implementation
- `tree-print()` - Recursive ASCII tree builder (46 lines)
- `update-trees()` - Refreshes all tree displays
- `toggle-tree()` - Toggles tree visibility on/off
- Three new UI labels: `*tree_read*`, `*tree_expand*`, `*tree_bind*`

#### Example Output
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
```

#### Educational Impact
Makes structure comprehension instant:
- Visual hierarchy of nested forms
- Node types clearly labeled
- List depths and item counts visible
- Easy comparison between phases
- Perfect for understanding complex macros

---

## Version 3.0 - Diff View

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
