# VP Source database

The ChrysaLisp VP Source Database is a structured metadata caching system that
indexes and represents the class hierarchies, virtual method dispatch tables,
and compiled function signatures of the Virtual Processor (VP) assembler
codebase. Implemented in `lib/files/info.inc`, the database scans and parses the
VP source files (`.vp` and `.inc`) to output two serialized, fast-loading
tree-structured files: `class_db.tre` and `func_db.tre`.

This database serves as the semantic engine for static analysis tools like
`trace`. It enables programmatic vertical and horizontal traversal of the
codebase, ensuring that call-graph resolution remains precise and type-bounded.

## Database Architecture

The metadata is distributed across two separate databases, allowing for
decoupled class and function lookups.

### 1. Class Database (`class_db.tre`)

The Class Database models the object-oriented structure of ChrysaLisp. It maps
class names to their structural components, parent-child relations, and member
method definitions.

Each class in the database is represented as an `Emap` containing the following
keys:

* `:parent`: The parent class symbol (e.g., `:array` is the parent of `:list`),
  or `:nil` if the class is a root node.

* `:subclasses`: A list of child class symbols that inherit directly from this
  class. This facilitates downward traversal of the inheritance tree.

* `:methods`: An `Emap` of all methods defined, overridden, or finalized by this
  class. Each method entry is itself an `Emap` containing:

    * `:type`: The dispatch type (`:static`, `:virtual`, `:override`, or
      `:final`).

    * `:function`: The name of the compiled VP function implementing the method
      (e.g., `"class/str/compare"`).

    * `:inputs`: A formatted string of input registers (e.g., `":r0, :r1"`).

    * `:outputs`: A formatted string of return registers (e.g., `":r0, :r1"`).

    * `:trashes`: The documented register clobbers.

    * `:overrides`: For `:virtual` methods, a list of all subclass symbols that
      subsequently override or finalize this method.

    * `:file`: The source file path string where the method's underlying
      function is defined (e.g., `"class/nums/class.vp"`).

    * `:trashes_line`: The 1-based line number of the `;trashes` comment in the
      source file.

#### Example Class Entry (`class_db.tre`)

```vdu
(Emap
    (:nums (Emap
        (:parent :array)
        (:subclasses (:fixeds))
        (:methods (Emap
            (:add (Emap
                (:type :virtual)
                (:function "class/nums/add")
                (:inputs ":r0-:r2")
                (:outputs ":r0-:r1")
                (:trashes ":r1-:r7")
                (:overrides (:fixeds :reals))
                (:file "class/nums/class.vp")
                (:trashes_line 14))))))))
```

### 2. Function Database (`func_db.tre`)

The Function Database compiles the signatures and clobber lists of all functions
defined in the VP source, regardless of whether they participate in a class
vtable.

Each function is represented as an `Emap` containing:

* `:implements`: A list of class-method tuples `((class method) ...)` mapping
  this function back to its class declarations.

* `:inputs`: A formatted string of input registers.

* `:outputs`: A formatted string of output registers.

* `:trashes`: The calculated or documented register clobbers.

* `:file`: The source file path string where the function is defined.

* `:trashes_line`: The 1-based line number of the `;trashes` comment in the
  source file.

#### Example Function Entry (`func_db.tre`)

```vdu
(Emap
    ("class/nums/add" (Emap
        (:implements ((:nums :add)))
        (:inputs ":r0-:r2")
        (:outputs ":r0-:r1")
        (:trashes ":r1-:r7")
        (:file "class/nums/class.vp")
        (:trashes_line 14))))
```

## Compilation and Build Pipeline

The databases are generated through a strict, multi-stage pipeline triggered
when the database files on disk are missing or out of date.

```
                  +-----------------------+
                  |  files-classes-info   |
                  +-----------+-----------+
                              |
                     [Checks Stale State]
                              |
                              v
                +---------------------------+
                | build-and-save-databases  |
                +-------------+-------------+
                              |
             +----------------+----------------+
             |                                 |
             v                                 v
      [unified-pass]                 [inherit-signatures]
             |                                 |
             v                                 v
      [link-databases]              [build-method-overrides]
             |                                 |
             +----------------+----------------+
                              |
                              v
                         [tree-save]
```

### 1. Staleness Verification

Before loading, `files-classes-info` checks if either `class_db.tre` or
`func_db.tre` is older than any of the source files returned by
`files-all-vp-source`. If either file is stale or missing, the rebuild pipeline
is triggered.

```file
lib/files/info.inc "(defun files-classes-info" ""
```

### 2. Structural Parsing (`unified-pass`)

The `unified-pass` function performs a single-pass scan of all source files. It
parses structural operators to populate the raw maps:

* `def-class`: Establishes the class node and sets up the parent and subclass
  pointers.

* `dec-method`: Parses the method declaration to extract the dispatch type, the
  underlying implementing function name, and any inline parameter signatures.

* `def-method` / `def-func` / `defun`: Flags the start of a function definition,
  prompting the parser to scan the preceding header comment block for `:inputs`,
  `:outputs`, and `:trashes` annotations.

* **Location Capture:** During this pass, `unified-pass` extracts the current
  file path and line number coordinates of the `;trashes` metadata comment,
  passing them to `flush-metadata` to populate the raw function entries.

### 3. Resolution and Inheritance (`link-databases` & `inherit-signatures`)

Once raw parsing is complete, the compiler resolves references across the
databases:

* `link-databases`: Cross-references the `class_db` and `func_db` to ensure that
  compiled function signatures match the declarations in class vtables. This
  step propagates the `:file` path and `:trashes_line` location fields from each
  raw function entry over to its virtual method entry in the class database.

* `inherit-signatures`: Walks up the class inheritance tree (from child to
  parent) to automatically resolve and propagate missing inputs, outputs, or
  trashes signatures.

### 4. Overrides Compilation (`build-method-overrides`)

The final structural pass scans all classes to build the `:overrides` list for
every `:virtual` method. This allows static analysis tools to quickly find all
polymorphic implementations without traversing the entire database.

```file
lib/files/info.inc "(defun build-method-overrides" ""
```

### 5. Writing the Databases (`build-and-save-databases`)

The orchestrated builder runs all passes and saves both databases sequentially
to the `lib/files/` directory using the `tree-save` serializer.

```file
lib/files/info.inc "(defun build-and-save-databases" ""
```

## Programmatic Query API

`lib/files/info.inc` exports three primary functions for external query use:

### `(files-classes-info)`

Loads (and compiles if out of date) the class database, returning the root
`class_db` `Emap`.

### `(files-function-info)`

Loads the function database, returning the `func_db` `Fmap`.

### `(files-all-vp-source)`

Collects and topologically sorts all non-app `.vp` source files based on their
internal `include` and `import` dependencies, returning an ordered list.

```file
lib/files/info.inc "(defun files-all-vp-source" ""
```