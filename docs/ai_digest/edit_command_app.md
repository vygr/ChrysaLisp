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

With the `-c` option your script commands will be auto wrapped into an `(defun
edit-script () ...)` lambda, before execution.

With the `-s` option your script is assumed to use advanced ChrysaLisp features,
such as macros, and as such a simple wrapping will not suffice. So the
assumption is that you will provide the `(defun edit-script () ...)` within the
script file !

* `-c --cmd "..."`: Immediate mode. A string containing the Lisp commands to
  execute.

* `-s --script path`: Script mode. A file path containing the Lisp script to
  execute.

* `-j --jobs num`: The number of jobs per node to use. Defaults to 1.

If no file paths are provided arguments, `edit` reads paths from `stdin`.

## The Execution Model

When `edit` runs, it performs the following steps:

1. **Environment Setup**: It creates a specialized Lisp environment inheriting
   from `*root_env*`.

2. **API Binding**: It populates this environment with editor-specific functions
   (see `edit-` API Reference below).

3. **Compilation**: It reads your script (from `-c` or `-s`), compiles it into
   native code, wraps or assumes you created a function called `(edit-script)`.
   This happens **once** on each node.

4. **Distribution**: It spawns worker tasks (locally or across the network).

5. **Execution**: For each file:

	* The file is loaded into a `Document` object (`*doc*`).

	* Your compiled `(edit-script)` function is executed.

	* If no error is thrown, and the document has been modified, the document is
	  saved back to disk.

### Implicit Context

Your script executes within the scope of a worker function. Two special
variables are implicitly bound and available to your script:

* `*doc*`: The current `Document` object being edited.

* `*file*`: The string path of the current file.

The primary cursor is at `(0 0 0 0)`. ie `(edit-top)`

### Variable Scope and Logic

Because the script is standard ChrysaLisp, you can use:

* **Variables**: `(defq global_count 0)` (local to the specific file pass).

* **Control Flow**: `(if ...)` , `(cond ...)`, `(while ...)`.

* **Iteration**: `(times ...)`, `(map ...)`.

* **Math**: All standard math functions.

* **Macros**: Yes, even define macros, with all the power that comes with them.

## API Reference

These commands are available directly within your edit script. They operate on
the implicit `*doc*`.

### Navigation

Move the cursor(s) around the document. Most operations respect multi-cursor
state.

| Command | Description |
| :--- | :--- |
| `(edit-top)` / `(edit-bottom)` | Move to start/end of document. |
| `(edit-home)` / `(edit-end)` | Move to start/end of current line. |
| `(edit-left [cnt])` / `(edit-right [cnt])` | Move cursor left/right. |
| `(edit-up [cnt])` / `(edit-down [cnt])` | Move cursor up/down. |
| `(edit-bracket-left)` | Move to the matching opening bracket `(` |
| `(edit-bracket-right)` | Move to the matching closing bracket `)` |
| `(edit-ws-left)` | Jump left over whitespace. |
| `(edit-ws-right)` | Jump right over whitespace. |

### Selection

Create or modify selections.

| Command | Description |
| :--- | :--- |
| `(edit-select-all)` | Select the entire document. |
| `(edit-select-line)` | Select the current line(s). |
| `(edit-select-word)` | Select the word(s) under the cursor/s. |
| `(edit-select-paragraph)` | Select the current paragraph(s) (block of text separated by blank lines). |
| `(edit-select-block)` | Select the content inside surrounding brackets `(...)`. |
| `(edit-select-form)` | Select the current S-expression/Lisp form(s). |
| `(edit-select-home)` ... | Directional selection expansion (home/end/top/bottom). |
| `(edit-select-up [cnt])` ... | Directional selection expansion (up/down/left/right). |

### Search & Cursors

ChrysaLisp uses a multi-cursor editing model. Search operations do not move the
cursor immediately; they populate a "found" buffer. You must explicitly convert
matches to cursors.

| Command | Description |
| :--- | :--- |
| `(edit-find pattern [:w] [:r])` | Find all occurrences of `pattern`. <br>`:w` = Whole words only.<br>`:r` = Treat pattern as Regex. |
| `(edit-cursors)` | **Replace** current cursors with cursors at every location found by `find`. |
| `(edit-add-cursors)` | **Add** new cursors at found locations to the existing set of cursors. |

### Mutation

Modify the text at the current cursor position(s) or selection(s).

| Command | Description |
| :--- | :--- |
| `(edit-insert string)` | Insert text. If a selection exists, it replaces the selection. |
| `(edit-delete [cnt])` | Delete character (or selection) to the right. |
| `(edit-backspace [cnt])` | Delete character (or selection) to the left. |
| `(edit-trim)` | Remove trailing whitespace from lines and empty lines from ends of file. |
| `(edit-sort)` | Sort selected lines alphabetically. |
| `(edit-unique)` | Remove duplicate lines within the selection. |
| `(edit-upper)` / `(edit-lower)` | Convert selection case. |
| `(edit-reflow)` | Reflow text in paragraph/selection (word wrap). |
| `(edit-split)` | Split lines into words. |
| `(edit-comment)` | Toggle line comment prefixes (`;;`). |
| `(edit-indent [cnt])` | Indent selected lines (tab right). |
| `(edit-outdent [cnt])` | Outdent selected lines (tab left). |

### Properties & IO

| Command | Description |
| :--- | :--- |
| `(edit-get-text)` | Return the currently selected text as a string. |
| `(edit-get-filename)` | Return the filename currently being processed. |
| `(edit-print ...)` | Print to `stdout`. Useful for dry-runs or extraction. |

---

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

### 4. Line Numbering (Variables and Loops)

Prepend line numbers to every line in a file.

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

Rename a function across the entire OS codebase, utilizing all available compute
nodes.

```code
files -a . .lisp |
edit -c
"(edit-find {old-func-name} :w)(edit-cursors)(edit-insert {new-func-name})"
```

### 8. Remove all multi blank lines

Remove all multi blank lines.

```code
files . .vp |
edit -c
"(edit-find {^\s+$} :r)(edit-cursors)(edit-select-line)(edit-delete)(edit-insert {\n})"
```
