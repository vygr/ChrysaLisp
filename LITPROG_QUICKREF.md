# LITPROG Quick Reference Card

```
╔════════════════════════════════════════════════════════════════╗
║                    LITPROG QUICK REFERENCE                    ║
║              Literate Programming for ChrysaLisp              ║
╚════════════════════════════════════════════════════════════════╝
```

## Core Commands

| Command | Description |
|---------|-------------|
| `(litprog-tangle "source.lit" "output/")` | Extract code to files |
| `(litprog-weave "source.lit" "out.html" :format :html)` | Generate HTML docs |
| `(litprog-weave "source.lit" "out.md" :format :markdown)` | Generate Markdown docs |
| `(litprog-weave "source.lit" "out.tex" :format :latex)` | Generate LaTeX docs |
| `(litprog-help)` | Show help |

## Syntax Styles

### 1. Noweb (Classic)

```
Define a chunk:
<<chunk-name>>=
code here
@

Reference a chunk:
<<another-chunk>>=
Some code
<<chunk-name>>
More code
@

Tangle to file:
<<filename.lisp>>=
code here
@
```

**Use when:** You want classic, clean syntax

### 2. Org-Mode (Emacs)

```
Named chunk:
#+NAME: chunk-name
#+BEGIN_SRC lisp
code here
#+END_SRC

Tangle to file:
#+BEGIN_SRC lisp :tangle filename.lisp
code here
#+END_SRC

Both:
#+NAME: chunk-name
#+BEGIN_SRC lisp :tangle filename.lisp
code here
#+END_SRC
```

**Use when:** Working with Emacs, org-mode users

### 3. Markdown Fence (Modern)

````
Named chunk:
```lisp {#chunk-name}
code here
```

Tangle to file:
```lisp {.tangle=filename.lisp}
code here
```

Both:
```lisp {#chunk-name .tangle=filename.lisp}
code here
```
````

**Use when:** GitHub, GitLab, modern developers

## Output Formats

| Format | Extension | Use Case |
|--------|-----------|----------|
| `:markdown` | `.md` | GitHub, GitLab, static sites |
| `:html` | `.html` | Web publishing, browsers |
| `:latex` | `.tex` | Academic papers, PDFs |
| `:text` | `.txt` | Plain text, simple docs |

## Common Patterns

### Single File Output

```
# My Program

<<myprogram.lisp>>=
<<imports>>
<<functions>>
<<main>>
@

<<imports>>=
(import "lib/asm/asm.inc")
@

<<functions>>=
(defun hello () (print "Hello!"))
@

<<main>>=
(hello)
@
```

### Multiple File Output (Org-Mode)

```
* File 1: Main Program

#+BEGIN_SRC lisp :tangle main.lisp
(import "helpers.lisp")
(main)
#+END_SRC

* File 2: Helpers

#+BEGIN_SRC lisp :tangle helpers.lisp
(defun main () (print "Hello!"))
#+END_SRC
```

### Multiple File Output (Markdown)

````
## Main Program

```lisp {.tangle=main.lisp}
(import "helpers.lisp")
(main)
```

## Helper Functions

```lisp {.tangle=helpers.lisp}
(defun main () (print "Hello!"))
```
````

## Chunk Composition

```
Chunks can reference other chunks:

<<main>>=
(defun main ()
  <<step-1>>
  <<step-2>>
  <<step-3>>)
@

<<step-1>>=
(print "Step 1")
@

<<step-2>>=
(print "Step 2")
@

<<step-3>>=
(print "Step 3")
@

Result:
(defun main ()
  (print "Step 1")
  (print "Step 2")
  (print "Step 3"))
```

## Indentation Handling

```
Chunk references inherit indentation:

<<function>>=
(defun foo ()
  <<body>>)
@

<<body>>=
(print "Line 1")
(print "Line 2")
@

Result:
(defun foo ()
  (print "Line 1")
  (print "Line 2"))
```

## Workflow Examples

### Development Workflow

```bash
# 1. Write literate source
vim myprogram.lit

# 2. Tangle to get code
chrysalisp -e "(import \"litprog.lisp\")"
chrysalisp -e "(litprog-tangle \"myprogram.lit\" \"src/\")"

# 3. Test the code
chrysalisp src/myprogram.lisp

# 4. Generate docs
chrysalisp -e "(litprog-weave \"myprogram.lit\" \"docs.html\" :format :html)"

# 5. Commit only the .lit file
git add myprogram.lit
git commit -m "Add feature"
```

### Documentation Workflow

```bash
# Generate all formats
chrysalisp -e "(import \"litprog.lisp\")"
chrysalisp -e "(litprog-weave \"doc.lit\" \"README.md\" :format :markdown)"
chrysalisp -e "(litprog-weave \"doc.lit\" \"index.html\" :format :html)"
chrysalisp -e "(litprog-weave \"doc.lit\" \"paper.tex\" :format :latex)"

# Publish to GitHub Pages
cp index.html docs/
git add docs/index.html
git commit -m "Update docs"
git push
```

## Best Practices

### ✅ DO

- **Explain WHY, not just WHAT**
  ```
  We use a hash map instead of a list because lookups
  are O(1) instead of O(n), which is critical for
  our 10,000+ element dataset.
  ```

- **Use descriptive chunk names**
  ```
  <<parse-http-headers>>
  <<validate-user-credentials>>
  <<calculate-invoice-total>>
  ```

- **Organize by narrative, not compilation**
  ```
  1. High-level overview
  2. Main algorithm
  3. Helper functions
  4. Edge cases
  5. Optimizations
  ```

- **Keep chunks focused**
  ```
  5-20 lines per chunk
  One clear purpose per chunk
  ```

### ❌ DON'T

- **Don't just comment code**
  ```
  Bad:
  <<parse>>=
  ; Parse the data
  (parse data)
  @

  Good:
  The input comes in CSV format with headers.
  We need to split on commas while handling
  quoted strings that may contain commas.

  <<parse>>=
  (parse-csv data :quoted t)
  @
  ```

- **Don't use vague names**
  ```
  Bad: <<stuff>>, <<part1>>, <<x>>
  Good: <<initialize-database>>, <<parse-request>>, <<render-view>>
  ```

- **Don't make chunks too small**
  ```
  Bad:
  <<add>>= (+ a b) @
  <<sub>>= (- a b) @

  Good:
  <<arithmetic-operations>>=
  (defun add (a b) (+ a b))
  (defun sub (a b) (- a b))
  @
  ```

## Troubleshooting

| Problem | Solution |
|---------|----------|
| "Undefined chunk reference" | Define chunk before referencing it |
| "No files generated" | Use filename in chunk or `:tangle` directive |
| "Wrong indentation" | Check indentation of chunk reference |
| "Chunk not found" | Check spelling, case-sensitive |
| "Empty output" | Ensure chunks have content |

## Examples Location

```
examples/
├── hello_literate.lit          (Noweb style)
├── fibonacci_orgmode.lit       (Org-mode style)
├── web_server_markdown.lit     (Markdown style)
└── advanced_mixed_styles.lit   (All three!)
```

## File Extensions

| Extension | Type |
|-----------|------|
| `.lit` | Literate source (recommended) |
| `.nw` | Noweb source (traditional) |
| `.org` | Org-mode source |
| `.md` | Markdown source (if using fences) |

## Editor Support

- **Emacs:** org-mode with Babel (native support)
- **Vim:** Markdown/org syntax highlighting
- **VS Code:** Markdown preview works well
- **Any editor:** All formats are plain text

## Command Line (via Makefile)

```bash
make -f Makefile.litprog help        # Show help
make -f Makefile.litprog examples    # Process all examples
make -f Makefile.litprog tangle      # Tangle all
make -f Makefile.litprog weave       # Weave all
make -f Makefile.litprog clean       # Remove output
make -f Makefile.litprog hello       # Just Hello World
make -f Makefile.litprog ref         # Show reference
```

## Integration Examples

### With Git Hooks

```bash
# .git/hooks/pre-commit
#!/bin/bash
echo "Tangling literate sources..."
chrysalisp -e "(import \"litprog.lisp\")"
chrysalisp -e "(litprog-tangle \"src/main.lit\" \"src/\")"
git add src/*.lisp
```

### With CI/CD

```yaml
# GitHub Actions
- name: Build from literate source
  run: |
    chrysalisp -e "(import \"litprog.lisp\")"
    chrysalisp -e "(litprog-tangle \"src/main.lit\" \"build/\")"
```

### With Make

```makefile
%.lisp: %.lit
	chrysalisp -e "(import \"litprog.lisp\")" \
	           -e "(litprog-tangle \"$<\" \"./\")"
```

## Resources

- **Full Documentation:** `LITPROG_README.md`
- **Examples:** `examples/` directory
- **Demo:** `demo_litprog.lisp`
- **Help:** `(litprog-help)`

## Philosophy

> "Instead of imagining that our main task is to instruct a computer
> what to do, let us concentrate rather on explaining to humans
> what we want the computer to do." — Donald Knuth

**Literate programming is about writing programs for people first,
computers second.**

---

**Version:** 1.0
**Author:** LITPROG for ChrysaLisp
**License:** Public Domain

```
╔════════════════════════════════════════════════════════════════╗
║              Happy Literate Programming! 📚✨                  ║
╚════════════════════════════════════════════════════════════════╝
```
