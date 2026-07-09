# ChrysaLisp Docs Application Architecture

The `apps/desktop/docs` application in ChrysaLisp is a markdown document viewer
that rejects the traditional monolithic "Rich Text" widget approach. Instead of
rendering a single massive text buffer, it leverages ChrysaLisp's graphical
composition system, dynamic module loading, and functional primitives to build
documents out of independent UI components.

This document details the internal architecture of the application, focusing on
its text layout engine, its dynamic code-block delegation system, and its
multi-tiered search capabilities.

## Text Rendering and Flow Layout

The text rendering engine (primarily located in
`apps/desktop/docs/handlers/text.inc`) treats a markdown document not as a
string to be painted, but as a hierarchical UI tree.

* **The Word-as-Widget Philosophy**

  * Every single word in a standard markdown paragraph is instantiated as its
    own independent `Text` widget.

  * Formatting is maintained via a state machine. As the parser encounters
    markup tags, it flips bits in a `state` integer (e.g., `+state_bold`,
    `+state_code`). When a word is instantiated, it checks this bitmask and
    dynamically pulls the correct font or color property from its parent
    container via ChrysaLisp's O(1) property inheritance (`:hmap :search`).

* **The Multi-Sequence `reduce!` Ladder**

  * Markdown parsing is accomplished without complex regex engines or nested
    loops. Instead, it uses a functional "ladder" utilizing ChrysaLisp's
    `reduce!` function.

  * `reduce!` is capable of stepping through multiple parallel sequences
    simultaneously. The parser feeds it a list of regex patterns (e.g.,
    `\\*\\*[^*]+\\*\\*` for bold), a corresponding string of replacement tags
    (e.g., `"b"`), and the character strip counts.

  * The raw text line flows down this ladder. If the bold pattern matches, it
    splices out the asterisks, inserts `<b>...</b>` tags, and passes the mutated
    string to the next rung (e.g., italic), resulting in a clean, tokenized
    string ready for the state machine.

* **Atomic Scatter-Copy Formatting via `splice`**

  * To ensure extreme performance and prevent memory-allocation thrashing
    during the `reduce!` ladder passes, the parser completely avoids creating
    intermediate string objects.

  * Instead of using repeated `slice` and `cat` operations for every matched
    markdown token, the parser uses ChrysaLisp's highly optimized, native
    `splice` sequence primitive.

  * As the regex engine finds formatting matches (like bold or italic words),
    the parser simply pushes raw byte-offsets into an interleaved numeric
    vector (`nums`).

  * These offsets are paired to alternate between two sources: the next
    segment of preserved text from the original line (`src1`), and the
    required injected markup tags from a secondary string (`src2`).

  * Once the regex pass is complete, a single call to `splice` is made. The
    VP engine calculates the exact required size of the final buffer,
    performs a single memory allocation, and executes a hardware-accelerated
    scatter-copy to stitch the alternating text and tags together instantly.

* **Flow-Based Word Wrapping**

  * Mathematical word wrapping is completely offloaded to the emergent behavior
    of ChrysaLisp's standard `Flow` GUI components.

  * To build a paragraph, the system creates a horizontal `Flow` widget to
    represent a single line.

  * It loops through the parsed words, spawning a `Text` widget for each. It
    queries the hardware-accelerated preferred width of the word widget using
    `(. text :pref_size)` and adds this to a running total.

  * The moment the running total exceeds the document window's width bounds, the
    line has naturally wrapped. The system simply creates a *new* horizontal
    `Flow` widget, resets the width counter, applies any necessary hanging
    indents (for bulleted lists), and continues placing word widgets.

## Dynamic Section Handlers

The document parser does not hardcode the logic for every possible markdown
block. Instead, it utilizes a powerful Delegation Pattern driven by Markdown's
fenced code blocks ("```tag").

* **Lazy Module Loading**

  * When the parser encounters a fenced block, it reads the tag (e.g.,
    "```lisp", "```image").

  * It passes this tag to the `handler-func` router. The router checks an
    environment map (`handlers`) for an existing function.

  * If no handler is found, the system dynamically generates a file path
    (`handlers/tag.inc`), reads the file from disk, and compiles/evaluates it
    live into the running application using `repl`. The new handler is cached
    and immediately used to process the block's text.

* **Built-in Handlers**

  * **`vdu` / `code`:** Renders standard code blocks. Instead of using `Text`
    widgets, it spawns a highly efficient `Vdu` (Video Display Unit) widget,
    complete with syntax highlighting driven by `lib/text/syntax.inc`.

  * **`image`:** Reads an image file path from the block and spawns a
    hardware-accelerated `Canvas` widget directly inline with the text,
    supporting `.cpm`, `.tga`, and fully vector-parsed `.svg` and `.cwb` files.

  * **`file`:** Opens an external text or code file and embeds its contents. It
    supports start and end regex markers, allowing documentation to dynamically
    pull in specific functions directly from the OS source code, ensuring docs
    never go out of sync with the codebase.

  * **`lispq` / `lisp` :** Evaluates the contents of the code block as
    live ChrysaLisp code within a sandbox. If the code returns a string, it
    prints the string. Astoundingly, if the code returns a GUI `View` object,
    *it embeds that live, interactive widget directly into the document*.

  * **`widget`:** Directly imports an application widget module (e.g., a
    calculator or a game) and embeds it natively within the document flow,
    proving that ChrysaLisp documents are actually interactive UI containers.

## The Search System

The application features a dual-tiered search system that covers both the
currently open document and the entire file system.

* **Polymorphic Search Engines**

  * Searching utilizes two distinct classes: `Substr` (for simple text matching)
    and `Regexp` (for complex pattern matching).

  * Both classes share the exact same method signatures (`:compile`, `:search`,
    `:match?`). The `query` function looks at the user's UI toggles (Regex,
    Whole Words) and returns the correct engine instance. The rest of the
    application remains entirely unaware of which engine is doing the work.

  * Search patterns are compiled and memoized via the `memoize` macro, meaning
    repeating a search is an O(1) cache lookup rather than a recompilation of
    the regex state machine.

* **Local Widget Highlighting**

  * Because the document is built from individual `Text` widgets, local search
    does not require complex string index tracking to highlight text.

  * When a document is loaded, every word widget is pushed into a flat
    `*search_widgets*` array.

  * When the user searches, the app filters this array using the search engine.
    It simply changes the background color property (`:color
    *env_highlight_col*`) of the matching `Text` widgets and commands the parent
    `Scroll` view to ensure that specific widget is visible on screen.

* **Global Distributed Search**

  * The global search feature (searching across the whole `docs/` directory) is
    not processed sequentially.

  * It utilizes ChrysaLisp's `pipe-farm` actor-model system. The app spawns a
    distributed fleet of `grep` tasks, one for every CPU core available in the
    entire node cluster.

  * Each core processes a subset of the markdown files independently, using
    lock-free message passing to return matching file paths back to the main GUI
    thread, where they are displayed in the application's file tree sidebar.

## Supported Text Styles

The text rendering engine dynamically translates inline markdown into visual
state bitmasks, allowing multiple properties to be applied to individual word
widgets on the fly. Here is a style sheet demonstrating all currently available
inline formatting:

You can use standard **bold** (`**bold**`) or *italic* (`*italic*`) text, or
seamlessly combine them for ***bold-italic*** (`***bold-italic***`) emphasis.

Technical documentation frequently uses `inline code` (`` `inline code` ``)
formatting.

Additionally, you can draw the reader's focus using ==highlighted==
(`==highlighted==`) text, or indicate revisions and deletions with a
~~strikethrough~~ (`~~strikethrough~~`) effect.