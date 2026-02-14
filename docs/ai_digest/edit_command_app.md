# The ChrysaLisp Parallel Programmable Editor

The `edit` command (`cmd/edit.lisp`) is not merely a search-and-replace tool; it
is a massively parallel, programmable document processing engine.

Unlike traditional tools like `sed` or `awk` which use specialized, restricted
mini-languages, `edit` allows you to write full **ChrysaLisp** programs to
manipulate text. These scripts are compiled into native code and executed within
a specialized environment populated with high-level editing primitives.

Because the editing logic is encapsulated in a pure function, the operation can
be trivially distributed across the network using the `-j` (jobs) flag, allowing
you to refactor thousands of files simultaneously across a compute cluster.

## Usage

```code
edit [options] [path] ...
```

### Options

* `-c --cmd "..."`: **Immediate mode.** A string containing the Lisp commands to
  execute. These commands are automatically wrapped in a `(defun edit-script ()
  ...)` lambda before compilation.

* `-s --script path`: **Script mode.** A file path containing the Lisp script to
  execute. In this mode, **you must explicitly define** the entry point function
  `(defun edit-script () ...)` within your file.

* `-j --jobs num`: The number of jobs per node to use. Defaults to 1.

If both `-c` and `-s` are provided, the `-c` commands are compiled *before* the
script file. This is useful for setting up configuration variables or defining
helper functions that the script depends on.

If no file paths are provided as arguments, `edit` reads paths from `stdin`.

## The Execution Model

When `edit` runs, it performs the following steps:

1. **Environment Setup**: It creates a specialized Lisp environment inheriting
   from `*root_env*`.

2. **API Binding**: It populates this environment with editor-specific functions
   (see API Reference below).

3. **Compilation**: It reads your script, compiles it into native code, and
   prepares the `(edit-script)` function. This happens **once** on each node.

4. **Distribution**: It spawns worker tasks (locally or across the network).

5. **Execution**: For each file:

	* The file is loaded into a `Document` object (`*edit*`).

	* Your compiled `(edit-script)` function is executed.

	* If no error is thrown, and the document has been modified, the document is
	  saved back to disk.

### Implicit Context

Your script executes within the scope of a worker function. Two special
variables are implicitly bound and available to your script:

* `*edit*`: The current `Document` object being edited.

* `*file*`: The string path of the current file.

The primary cursor starts at `(0 0 0 0)` (Top of file).

### Variable Scope and Logic

Because the script is standard ChrysaLisp, you can use:

* **Variables**: `(defq count 0)` (local to the specific file pass).

* **Control Flow**: `(if ...)` , `(cond ...)`, `(while ...)`.

* **Iteration**: `(times ...)`, `(map ...)`.

* **Math**: All standard math functions.

* **Macros**: You may define macros within your script.

## API Reference

These commands are available directly within your edit script. They operate on
the implicit `*edit*`.

### Navigation

Move the cursor(s) around the document.

* `(edit-top)` / `(edit-bottom)`: Move to start/end of document.

* `(edit-home)` / `(edit-end)`: Move to start/end of current line.

* `(edit-left [cnt])`: Move cursor left `cnt` times (default 1).

* `(edit-right [cnt])`: Move cursor right `cnt` times (default 1).

* `(edit-up [cnt])`: Move cursor up `cnt` lines.

* `(edit-down [cnt])`: Move cursor down `cnt` lines.

* `(edit-bracket-left)`: Move to the matching opening bracket `(`.

* `(edit-bracket-right)`: Move to the matching closing bracket `)`.

* `(edit-ws-left)`: Jump left over whitespace.

* `(edit-ws-right)`: Jump right over whitespace.

* `(edit-cx)`: Return primary cursor X position.

* `(edit-cy)`: Return primary cursor Y position.

* `(edit-eof?)`: Returns `:t` if primary cursor is at the end of the file.

### Selection

Create or modify selections. ChrysaLisp uses a multi-cursor editing model.

* `(edit-select-all)`: Select the entire document.

* `(edit-select-line)`: Select the current line(s).

* `(edit-select-word)`: Select the word(s) under the cursor/s.

* `(edit-select-paragraph)`: Select the current paragraph(s).

* `(edit-select-block)`: Select the content of surrounding brackets `(...)`.

* `(edit-select-form)`: Select the current S-expression/Lisp form(s).

* `(edit-select-ws-left)` / `(edit-select-ws-right)`: Select following or
  preceding whitespace.

* `(edit-select-bracket-left)` / `(edit-select-bracket-right)`: Select from
  cursor to matching bracket.

* `(edit-select-home)`: Select from cursor to start of line.

* `(edit-select-end)`: Select from cursor to end of line.

* `(edit-select-top)`: Select from cursor to start of document.

* `(edit-select-bottom)`: Select from cursor to end of document.

* `(edit-select-left [cnt])`: Extend selection left.

* `(edit-select-right [cnt])`: Extend selection right.

* `(edit-select-up [cnt])`: Extend selection up.

* `(edit-select-down [cnt])`: Extend selection down.

### Search & Cursors

Search operations populate a "found" buffer. You must explicitly convert matches
to cursors to edit them.

* `(edit-find pattern [:w :r])`: Find occurrences of `pattern`. Use `:w` for
  Whole words and `:r` for Regex mode.

* `(edit-find-next)` / `(edit-find-prev)`: Select the next or previous match.

* `(edit-find-add-next)`: Add the next match to current cursors.

* `(edit-cursors)`: **Replace** current cursors with cursors at every location
  found.

* `(edit-add-cursors)`: **Add** new cursors at found locations to existing
  cursors.

* `(edit-primary)`: **Collapse** all cursors into a single primary cursor.

### Focus

Focus operations restrict editing and searching to a specific region of the
document.

* `(edit-get-focus)`: Return the current focus region cursor.

* `(edit-set-focus [csr])`: Set the focus region. Defaults to the last cursor.

* `(edit-filter-cursors)`: Remove all cursors that are outside the focus region.

* `(edit-focus-cursors)`: Set cursors to the subset of global found matches that fall
  within the current focus region.

### Mutation

Modify text at current cursor positions or selections.

* `(edit-insert txt)`: Insert text. Replaces selection if one exists.

* `(edit-replace pattern)`: Replace selected text using `$n` regex substitution
  patterns.

* `(edit-delete [cnt])`: Delete character/selection to the right.

* `(edit-backspace [cnt])`: Delete character/selection to the left.

* `(edit-cut)`: Cut selection and return it.

* `(edit-paste txt)`: Paste text at cursor(s).

* `(edit-trim)`: Remove trailing whitespace and empty end lines.

* `(edit-sort)`: Sort selected lines alphabetically.

* `(edit-unique)`: Remove duplicate lines within selection.

* `(edit-upper)` / `(edit-lower)`: Convert selection case.

* `(edit-reflow)`: Reflow text in paragraph/selection.

* `(edit-split)`: Split lines into words.

* `(edit-comment)`: Toggle line comment prefixes (`;;`).

* `(edit-indent [cnt])`: Indent selected lines (tab right).

* `(edit-outdent [cnt])`: Outdent selected lines (tab left).

### Properties & Utilities

* `(edit-get-text)`: Return the currently selected text as a string.

* `(edit-get-primary-text)`: Return the text selected by the primary cursor as a
  string.

* `(edit-get-filename)`: Return the filename currently being processed.

* `(edit-copy)`: Copy selection and return it.

* `(edit-print ...)`: Print to `stdout` (useful for dry-runs). Defaults to the
  selected text.

* `(edit-split-text txt [cls])`: Utility: Split string by char class (default
  form feed and newline).

* `(edit-join-text list [cls])`: Utility: Join list into string (default
  newline).

## Examples

### 1. Regex Replace

Find all instances of "foo" or "bar" and replace them with "baz".

```code
files . .txt |
edit -c
"(edit-find {foo|bar} :r)(edit-cursors)(edit-insert {baz})"
```

### 2. Semantic Commenting

Find every definition of the function `main` and comment it out.

```code
files . .lisp |
edit -c
"(edit-find {defun main})(edit-cursors)(edit-select-block)(edit-comment)"
```

### 3. Header Injection

Insert a copyright notice at the top of every file.

```code
files . .lisp |
edit -c
"(edit-top)(edit-insert {;; Copyright 2026\n\n})"
```

### 4. Line Numbering

Prepend line numbers to every line in a file using a loop and `edit-cy`.

```code
edit -c
"(until (edit-eof?)
	(edit-insert (str (inc (edit-cy)) {: }))
	(edit-down)
	(edit-home))"
file.txt
```

### 5. Conditional Logic

Only edit the file if it contains a specific Todo marker.

```code
files . .src |
edit -c
"(when (edit-find {TODO_FIX_THIS})
	(edit-cursors)
	(edit-select-line)
	(edit-delete))"
```

### 6. Data Extraction

Don't edit the file, just extract data. This finds all URLs and prints them.

```code
files . .ms |
edit -c
"(edit-find {https://[^ \q]+} :r)(edit-cursors)(edit-print)"
```

### 7. Massive Parallel Refactoring

Rename a function across the entire OS codebase using all available compute
nodes.

```code
files -a . .lisp |
edit -c
"(edit-find {old-func-name} :w)(edit-cursors)(edit-insert {new-func-name})"
```

### 8. Remove Multiple Blank Lines

Use Regex to find empty lines and reduce them to a single newline.

```code
files . .vp |
edit -c
"(edit-find {^\\s*$} :r)(edit-cursors)(edit-select-line)(edit-delete)(edit-insert {\n})"
```
