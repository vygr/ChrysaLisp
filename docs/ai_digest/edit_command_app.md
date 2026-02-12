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

* `-c --cmd "..."`: Immediate mode. A string containing the Lisp script to
  execute.

* `-s --script path`: Script mode. A file path containing the Lisp script to
  execute.

* `-j --jobs num`: The number of concurrent jobs/nodes to use. Defaults to 1
  (local execution).

If no file paths are provided arguments, `edit` reads paths from `stdin`.

## The Execution Model

When `edit` runs, it performs the following steps:

1. **Environment Setup**: It creates a specialized Lisp environment inheriting
   from `*root_env*`.

2. **API Binding**: It populates this environment with editor-specific functions
   (see API Reference below).

3. **Compilation**: It reads your script (from `-c` or `-s`), wraps it in a
   `lambda`, and compiles it into a native function. This happens **once**.

4. **Distribution**: It spawns worker tasks (locally or across the network).

5. **Execution**: For each file:

    * The file is loaded into a `Document` object (`*doc*`).

    * Your compiled function is executed.

    * If no error is thrown, the document is saved back to disk.

### Implicit Context

Your script executes within the scope of a worker function. Two special
variables are implicitly bound and available to your script:

* `*doc*`: The current `Document` object being edited.

* `*file*`: The string path of the current file.

### Variable Scope and Logic

Because the script is standard ChrysaLisp, you can use:

* **Variables**: `(defq global_count 0)` (shared across the batch on a specific
  node) or `(let ((i 0)) ...)` (local to the specific file pass).

* **Control Flow**: `(if ...)` , `(cond ...)`, `(while ...)`.

* **Iteration**: `(dotimes ...)`, `(map ...)`.

* **Math**: All standard math functions.

## API Reference

These commands are available directly within your edit script. They operate on
the implicit `*doc*`.

### Navigation

Move the cursor(s) around the document. Most operations respect multi-cursor
state.

| Command | Description |
| :--- | :--- |
| `(top)` / `(bottom)` | Move to start/end of document. |
| `(home)` / `(end)` | Move to start/end of current line. |
| `(left)` / `(right)` | Move cursor left/right. |
| `(up)` / `(down)` | Move cursor up/down. |
| `(bracket-left)` | Move to the matching opening bracket `(`, `[`, `{`. |
| `(bracket-right)` | Move to the matching closing bracket `)`, `]`, `}`. |
| `(ws-left)` | Jump left over whitespace. |
| `(ws-right)` | Jump right over whitespace. |

### Selection

Create or modify selections.

| Command | Description |
| :--- | :--- |
| `(select-all)` | Select the entire document. |
| `(select-line)` | Select the current line(s). |
| `(select-word)` | Select the word under the cursor. |
| `(select-paragraph)` | Select the current paragraph (block of text separated by blank lines). |
| `(select-block)` | Select the content inside surrounding brackets `(...)`. |
| `(select-form)` | Select the current S-expression/Lisp form. |
| `(select-left)` ... | Directional selection expansion (up/down/left/right/home/end/top/bottom). |

### Search & Cursors

ChrysaLisp uses a multi-cursor editing model. Search operations do not move the
cursor immediately; they populate a "found" buffer. You must explicitly convert
matches to cursors.

| Command | Description |
| :--- | :--- |
| `(find pattern [:w] [:r])` | Find all occurrences of `pattern`. <br>`:w` = Whole words only.<br>`:r` = Treat pattern as Regex. |
| `(cursors)` | **Replace** current cursors with cursors at every location found by `find`. |
| `(add-cursors)` | **Add** new cursors at found locations to the existing set of cursors. |

### Mutation

Modify the text at the current cursor position(s) or selection(s).

| Command | Description |
| :--- | :--- |
| `(insert string)` | Insert text. If a selection exists, it replaces the selection. |
| `(delete)` | Delete character (or selection) to the right. |
| `(backspace)` | Delete character (or selection) to the left. |
| `(trim)` | Remove trailing whitespace from lines and empty lines from ends of file. |
| `(sort)` | Sort selected lines alphabetically. |
| `(unique)` | Remove duplicate lines within the selection. |
| `(upper)` / `(lower)` | Convert selection case. |
| `(reflow)` | Reflow text in paragraph/selection (word wrap). |
| `(split)` | Split lines (hard wrap) at column limit. |
| `(comment)` | Toggle line comment prefixes (`;;`). |
| `(indent)` | Indent selected lines (tab right). |
| `(outdent)` | Outdent selected lines (tab left). |

### Properties & IO

| Command | Description |
| :--- | :--- |
| `(get-text)` | Return the currently selected text as a string. |
| `(get-filename)` | Return the filename currently being processed. |
| `(print ...)` | Print to `stdout`. Useful for dry-runs or extraction. |

---

## Examples

### 1. Regex Replace

Find all instances of "foo" or "bar" and replace them with "baz".

```code
edit -c "(find 'foo|bar' :r) (cursors) (insert 'baz')" *.txt
```

### 2. Semantic Commenting

Find every definition of the function `main` and comment it out.

```code
edit -c "(find 'defun main') (cursors) (select-form) (comment)" *.lisp
```

### 3. Header Injection

Insert a copyright notice at the top of every file.

```code
edit -c "(top) (insert ';; Copyright 2024\n')" *.lisp
```

### 4. Line Numbering (Variables and Loops)

Prepend line numbers to every line in a file.

```code
edit -c "
(top) 
(defq i 0) 
(while (down) 
    (home) 
    (insert (str (++ i) ': '))
)" file.txt
```

### 5. Conditional Logic

Only edit the file if it contains a specific Todo marker.

```code
edit -c "
(if (find 'TODO_FIX_THIS') 
    (progn 
        (cursors) 
        (select-line) 
        (delete)
    )
)" *.src
```

### 6. Data Extraction

Don't edit the file, just extract data. This finds all URLs and prints them.

```code
edit -c "
(find 'https://[^ \q]+' :r) 
(cursors) 
(each print (get-text))
" *.md
```

### 7. Massive Parallel Refactoring

Rename a function across the entire OS codebase, utilizing all available compute
nodes.

```code
files -a . .lisp | edit -j 64 -c "(find 'old-func-name' :w) (cursors) (insert 'new-func-name')"
```
