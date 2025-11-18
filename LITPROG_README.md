# LITPROG - Literate Programming for ChrysaLisp

```
╔════════════════════════════════════════════════════════════════╗
║  LITPROG - A Comprehensive Literate Programming Tool          ║
║  "Let us change our traditional attitude to the               ║
║   construction of programs: Instead of imagining that         ║
║   our main task is to instruct a computer what to do,         ║
║   let us concentrate rather on explaining to humans           ║
║   what we want the computer to do." - Donald Knuth            ║
╚════════════════════════════════════════════════════════════════╝
```

## What is Literate Programming?

**Literate Programming** is a methodology introduced by Donald Knuth that treats a
program as a piece of literature directed at human beings rather than a computer.
Instead of writing code with occasional comments, you write an essay explaining
your thought process, with code snippets embedded throughout.

### Key Benefits

1. **Human-First Design** - Code is organized by narrative logic, not technical requirements
2. **Self-Documenting** - Documentation and code can't diverge (they're the same file!)
3. **Better Understanding** - Forces you to think clearly about what you're doing
4. **Teaching Tool** - Perfect for tutorials, examples, and educational materials
5. **Maintenance** - Future maintainers (including yourself!) understand the "why"
6. **Multiple Outputs** - Generate both working code AND beautiful documentation

## Features

LITPROG supports three popular literate programming syntaxes:

### 1. Noweb Style (Classic)

```
<<chunk-name>>=
Code goes here
@

Later, you can reference:
<<chunk-name>>
```

### 2. Org-Mode Style (Emacs)

```
#+NAME: chunk-name
#+BEGIN_SRC lisp :tangle output.lisp
Code goes here
#+END_SRC
```

### 3. Markdown Fence Style (Modern)

```
```lisp {#chunk-name .tangle=output.lisp}
Code goes here
```
```

### Additional Features

- ✅ **Multiple Output Formats**: Markdown, HTML, LaTeX, Plain Text
- ✅ **Chunk Composition**: Reference chunks within chunks
- ✅ **Multiple Files**: Tangle to different output files
- ✅ **Syntax Highlighting**: Beautiful code display in documentation
- ✅ **Cross-References**: Automatic index generation
- ✅ **Language Agnostic**: Works with any programming language
- ✅ **Incremental Development**: Build code piece by piece

## Installation

Simply copy `litprog.lisp` to your ChrysaLisp installation:

```bash
cp litprog.lisp ~/ChrysaLisp/lib/
```

## Quick Start

### 1. Create a Literate Program

Create a file `myprogram.lit`:

```markdown
# My First Literate Program

This program calculates factorials.

<<factorial.lisp>>=
(defun factorial (n)
  (if (<= n 1)
    1
    (* n (factorial (- n 1)))))

(print (factorial 5))
@
```

### 2. Tangle (Extract Code)

```lisp
(import "litprog.lisp")
(litprog-tangle "myprogram.lit" "output/")
```

This creates `output/factorial.lisp` containing:

```lisp
(defun factorial (n)
  (if (<= n 1)
    1
    (* n (factorial (- n 1)))))

(print (factorial 5))
```

### 3. Weave (Generate Documentation)

```lisp
(litprog-weave "myprogram.lit" "docs.html" :format :html)
```

This creates beautiful HTML documentation with syntax highlighting!

## Usage Guide

### API Reference

#### `litprog-tangle`

Extract code from literate source to executable files.

```lisp
(litprog-tangle source-file output-directory)
```

**Parameters:**
- `source-file` - Path to the literate source file (`.lit`)
- `output-directory` - Directory where code files will be generated

**Example:**
```lisp
(litprog-tangle "src/program.lit" "build/")
```

#### `litprog-weave`

Generate documentation from literate source.

```lisp
(litprog-weave source-file output-file :format format-type)
```

**Parameters:**
- `source-file` - Path to the literate source file (`.lit`)
- `output-file` - Path for the generated documentation
- `:format` - Output format (`:markdown`, `:html`, `:latex`, `:text`)

**Examples:**
```lisp
; Generate HTML
(litprog-weave "src/program.lit" "docs/index.html" :format :html)

; Generate Markdown
(litprog-weave "src/program.lit" "README.md" :format :markdown)

; Generate LaTeX
(litprog-weave "src/program.lit" "paper.tex" :format :latex)
```

#### `litprog-help`

Display usage information.

```lisp
(litprog-help)
```

### Syntax Guide

#### Noweb Syntax

**Define a chunk:**
```
<<chunk-name>>=
code here
@
```

**Reference a chunk:**
```
<<chunk-name>>
```

**Tangle to a specific file:**
```
<<filename.lisp>>=
code here
@
```

**Example:**
```
# Calculator Program

<<calculator.lisp>>=
<<imports>>
<<functions>>
<<main>>
@

<<imports>>=
(import "lib/math.inc")
@

<<functions>>=
(defun add (a b) (+ a b))
(defun subtract (a b) (- a b))
@

<<main>>=
(print (add 5 3))
@
```

#### Org-Mode Syntax

**Named chunk:**
```
#+NAME: chunk-name
#+BEGIN_SRC lisp
code here
#+END_SRC
```

**Tangle to file:**
```
#+BEGIN_SRC lisp :tangle output.lisp
code here
#+END_SRC
```

**Example:**
```
* Calculator Functions

#+NAME: add-function
#+BEGIN_SRC lisp :tangle calculator.lisp
(defun add (a b) (+ a b))
#+END_SRC

#+NAME: subtract-function
#+BEGIN_SRC lisp :tangle calculator.lisp
(defun subtract (a b) (- a b))
#+END_SRC
```

#### Markdown Fence Syntax

**Named chunk:**
```
```lisp {#chunk-name}
code here
```
```

**Tangle to file:**
```
```lisp {.tangle=output.lisp}
code here
```
```

**Both name and tangle:**
```
```lisp {#chunk-name .tangle=output.lisp}
code here
```
```

**Example:**
````markdown
# Calculator Functions

```lisp {#add-function .tangle=calculator.lisp}
(defun add (a b) (+ a b))
```

```lisp {#subtract-function .tangle=calculator.lisp}
(defun subtract (a b) (- a b))
```
````

## Examples

The `examples/` directory contains three comprehensive examples:

### 1. Hello World (Noweb Style)

**File:** `examples/hello_literate.lit`

A simple introduction demonstrating:
- Noweb syntax
- Chunk composition
- Documentation integration

**Try it:**
```lisp
(litprog-tangle "examples/hello_literate.lit" "output/")
(litprog-weave "examples/hello_literate.lit" "hello_docs.html" :format :html)
```

### 2. Fibonacci Calculator (Org-Mode Style)

**File:** `examples/fibonacci_orgmode.lit`

Advanced example showing:
- Three different algorithms (recursive, iterative, memoized)
- Performance analysis
- Benchmarking utilities
- Mathematical explanations

**Try it:**
```lisp
(litprog-tangle "examples/fibonacci_orgmode.lit" "output/")
(litprog-weave "examples/fibonacci_orgmode.lit" "fib_docs.html" :format :html)
```

### 3. Web Server (Markdown Fence Style)

**File:** `examples/web_server_markdown.lit`

Complex multi-file example demonstrating:
- Multiple output files
- Modular architecture
- Request routing
- HTTP protocol parsing
- Handler functions

**Try it:**
```lisp
(litprog-tangle "examples/web_server_markdown.lit" "output/")
(litprog-weave "examples/web_server_markdown.lit" "server_docs.html" :format :html)
```

## Best Practices

### 1. Write for Humans First

Don't just comment your code - explain your thinking:

**Bad:**
```
<<parse>>=
; Parse the input
(defq parts (split input " "))
@
```

**Good:**
```
We need to break the input into words. Since our input format uses
spaces as delimiters, we can use the built-in `split` function.

<<parse>>=
(defq parts (split input " "))
@
```

### 2. Chunk Size Matters

- **Too small:** Excessive fragmentation, hard to follow
- **Too large:** Defeats the purpose, hard to reuse

**Good chunk size:** 5-20 lines of code with a clear single purpose

### 3. Name Chunks Descriptively

**Bad:**
```
<<stuff>>=
<<part1>>=
<<x>>=
```

**Good:**
```
<<parse-http-headers>>=
<<validate-user-input>>=
<<calculate-fibonacci>>=
```

### 4. Use Narrative Order

Organize by story flow, not compilation order:

```markdown
# User Authentication System

First, let's understand what happens when a user logs in...

<<handle-login-request>>=
...
@

To verify credentials, we need to check against the database...

<<verify-credentials>>=
...
@

The database connection is established like this...

<<database-connection>>=
...
@
```

### 5. Provide Context

Explain WHY, not just WHAT:

```markdown
## Why Memoization?

The naive recursive Fibonacci implementation is exponentially slow
because it recalculates the same values repeatedly. For fib(40),
it makes over 300 million function calls!

By caching results, we reduce this to just 40 calculations - a
dramatic improvement.

<<fib-memoized>>=
(defun fib-memoized (n)
  (if (. *cache* n)
    (. *cache* n)      ; Return cached value
    (... )))           ; Calculate and cache
@
```

## Comparison with Traditional Approaches

### Traditional Programming

```
myprogram.lisp          (code with comments)
README.md               (separate documentation)
docs/architecture.md    (more separate docs)
```

**Problems:**
- Documentation gets outdated
- Comments are terse and technical
- No narrative flow
- Can't understand WHY decisions were made

### Literate Programming

```
myprogram.lit           (single source of truth)
  ├─> tangle ──> myprogram.lisp      (executable code)
  └─> weave  ──> myprogram.html      (beautiful docs)
```

**Benefits:**
- Code and docs always in sync
- Rich explanations with formatting
- Story-like flow
- Design decisions preserved

## Output Format Examples

### HTML Output

```html
<!DOCTYPE html>
<html>
<head>
  <title>Literate Program</title>
  <style>
    body { font-family: Georgia, serif; }
    pre { background: #f5f5f5; }
    h3 { color: #333; border-bottom: 2px solid #007acc; }
  </style>
</head>
<body>
  <h3>Chunk: <code>factorial</code></h3>
  <p><em>Tangles to: math.lisp</em></p>
  <pre><code class="language-lisp">
(defun factorial (n)
  (if (<= n 1)
    1
    (* n (factorial (- n 1)))))
  </code></pre>
</body>
</html>
```

### Markdown Output

```markdown
### Chunk: `factorial`

*Tangles to: math.lisp*

```lisp
(defun factorial (n)
  (if (<= n 1)
    1
    (* n (factorial (- n 1)))))
```
```

### LaTeX Output

```latex
\subsection{Chunk: \texttt{factorial}}
\textit{Tangles to: math.lisp}

\begin{verbatim}
(defun factorial (n)
  (if (<= n 1)
    1
    (* n (factorial (- n 1)))))
\end{verbatim}
```

## Workflow Integration

### With Version Control

```bash
# Only commit the .lit file
git add myprogram.lit
git commit -m "Add factorial implementation"

# Generate code before building
make tangle
make build

# Generate docs for GitHub Pages
make weave-html
```

### With CI/CD

```yaml
# .github/workflows/build.yml
steps:
  - name: Tangle literate sources
    run: |
      chrysalisp -e "(import \"litprog.lisp\")"
      chrysalisp -e "(litprog-tangle \"src/main.lit\" \"build/\")"

  - name: Build project
    run: make build

  - name: Generate documentation
    run: |
      chrysalisp -e "(litprog-weave \"src/main.lit\" \"docs/index.html\" :format :html)"

  - name: Deploy docs
    uses: peaceiris/actions-gh-pages@v3
```

### With Documentation Sites

Generate markdown for static site generators:

```bash
# For Jekyll
litprog-weave src/tutorial.lit docs/tutorial.md :format :markdown

# For MkDocs
litprog-weave src/api.lit docs/api-reference.md :format :markdown

# For Sphinx (via LaTeX)
litprog-weave src/manual.lit docs/manual.tex :format :latex
```

## Advanced Techniques

### Chunk Parameters

You can pass context through chunk names:

```
We need different sorting algorithms for different data types:

<<sort-numbers>>=
(defun sort-numbers (lst)
  <<generic-sort-impl>>)
@

<<sort-strings>>=
(defun sort-strings (lst)
  <<generic-sort-impl>>)
@

<<generic-sort-impl>>=
; Implementation here uses the context
; of the containing function
@
```

### Multiple Language Support

Mix languages in one document:

```markdown
# Full-Stack Application

Frontend (JavaScript):
```javascript {.tangle=app.js}
function handleClick() {
  fetch('/api/data')
    .then(response => response.json())
    .then(data => console.log(data));
}
```

Backend (ChrysaLisp):
```lisp {.tangle=server.lisp}
(defun handle-api-request (req)
  (make-json-response
    (list :data "Hello from ChrysaLisp!")))
```
```

### Documentation Chunks

Use chunks for documentation too:

```
<<introduction-text>>=
This program solves the traveling salesman problem
using a genetic algorithm approach.
@

The main documentation includes:
<<introduction-text>>

And later in the appendix:
<<introduction-text>>
```

## Troubleshooting

### Chunk Not Found

**Error:** `Warning: undefined chunk reference: my-chunk`

**Solution:** Make sure the chunk is defined before it's referenced:
```
<<my-chunk>>=
code here
@

Later:
<<my-chunk>>  ← Now this works
```

### Tangle File Not Created

**Problem:** `litprog-tangle` runs but no files are created.

**Solution:** Check that chunks specify output files:
```
<<myfile.lisp>>=   ← Must end in a filename
code
@

OR

#+BEGIN_SRC lisp :tangle myfile.lisp   ← :tangle directive
code
#+END_SRC
```

### Indentation Issues

**Problem:** Code has wrong indentation when tangled.

**Solution:** LITPROG preserves the indentation of chunk references:

```
<<outer>>=
(defun foo ()
  <<inner>>)   ← inner will be indented 2 spaces
@

<<inner>>=
(print "Hello")
(print "World")
@

Result:
(defun foo ()
  (print "Hello")
  (print "World"))
```

## Philosophy

Literate programming embodies several key principles:

### 1. Programs are for People

> "Let us change our traditional attitude to the construction of programs:
> Instead of imagining that our main task is to instruct a computer what to do,
> let us concentrate rather on explaining to humans what we want the computer
> to do." - Donald Knuth

### 2. Narrative Order Over Compiler Order

Traditional: Start → Imports → Globals → Helpers → Main → End
Literate: Start → Main Idea → Supporting Concepts → Details → End

### 3. Documentation as a First-Class Citizen

Not an afterthought. Not separate. The documentation IS the program.

### 4. Teaching While Building

Every program should teach the next person (including future you) how it works.

## Further Reading

### Original Papers

- Knuth, D. E. (1984). "Literate Programming". *The Computer Journal*. 27 (2): 97–111.
- Ramsey, N. (1994). "Literate programming simplified". *IEEE Software*. 11 (5): 97–105.

### Tools and Systems

- **noweb** - Norman Ramsey's language-independent tool
- **org-mode** - Emacs org-mode with Babel
- **Docco** - Quick-and-dirty literate programming
- **Jupyter** - Notebooks as literate documents

### Examples in the Wild

- TeX/LaTeX - Written by Knuth using WEB
- SQLite - Has literate documentation
- Linux Kernel - Some subsystems use literate docs
- Many academic papers include literate code

## Contributing

Have ideas for improving LITPROG? Found a bug?

1. The tool is self-documented (eating our own dog food!)
2. Improvements should maintain simplicity
3. Add examples for new features
4. Keep it ChrysaLisp-idiomatic

## License

LITPROG is released into the public domain. Use it however you want!

## Acknowledgments

- **Donald Knuth** - For inventing literate programming
- **Norman Ramsey** - For noweb, the inspiration for this tool
- **ChrysaLisp Community** - For building an amazing system

---

```
╔════════════════════════════════════════════════════════════════╗
║  "The practitioner of literate programming can be regarded    ║
║   as an essayist, whose main concern is with exposition       ║
║   and excellence of style." - Donald Knuth                    ║
╚════════════════════════════════════════════════════════════════╝
```

**Happy Literate Programming!** 📚✨
